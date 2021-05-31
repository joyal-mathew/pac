use crate::{ lexing::Type, utils::{ Result, Serial }, parsing::* };
use std::{ mem, fs, collections::HashMap };

#[derive(Clone, Debug)]
struct Var {
    ptr: usize,
    type_: Type,
    assigned: bool,
    named: bool,
}

fn create_signature(return_type: Type, params: &Vec<Statement>) -> Type {
    let mut param_types = Vec::new();

    for param in params {
        if let Statement::Declaration(t, v) = param {
            param_types.extend(std::iter::repeat(t.clone()).take(v.len()));
        }
    }

    Type::Function(Box::new(return_type), param_types)
}

fn is_array(t: &Type) -> bool {
    mem::discriminant(t) == mem::discriminant(&Type::Array(Box::new(Type::Void)))
}

fn is_main(sig: &Type) -> bool {
    if let Type::Function(return_type, param_types) = sig {
        return **return_type == Type::Void && param_types.is_empty()
    }

    return false;
}

fn is_alloc(t: &Type) -> bool {
    is_array(t) || *t == Type::String || *t == Type::ArrayInfer
}

struct Scope {
    vars: HashMap<String, Var>,
    ptr: usize,
}

impl Scope {
    fn new(global: &HashMap<String, Var>) -> Self {
        Self {
            vars: global.clone(),
            ptr: 0,
        }
    }

    fn get_var(&mut self, name: &String, assign: bool) -> Result<Var> {
        if assign {
            self.vars.entry(name.to_string()).and_modify(|v| v.assigned = true);
        }
        self.vars.get(name).cloned().ok_or(format!("undefined variable {}", name))
    }

    fn declare(&mut self, ident: String, type_: Type, assigned: bool) -> Result<usize> {
        if self.vars.contains_key(&ident) {
            err!("variable already delcared in this scope")
        }
        else {
            self.vars.insert(ident, Var { type_, ptr: self.ptr, assigned, named: false });
            self.ptr += 8;
            Ok(self.ptr - 8)
        }
    }
}

pub struct Compiler {
    parser: Parser,

    data: String,
    program: String,

    functions: HashMap<String, Var>,
    label: usize,

    break_label: Vec<usize>,
    continue_label: Vec<usize>,
}

impl Compiler {
    pub fn new(parser: Parser) -> Self {
        let mut functions = HashMap::new();

        functions.insert("print_str".to_string(), Var { ptr: 0, type_: Type::Function(Box::new(Type::Void), vec![Type::Str]), assigned: true, named: true });
        functions.insert("print_string".to_string(), Var { ptr: 0, type_: Type::Function(Box::new(Type::Void), vec![Type::String]), assigned: true, named: true });
        functions.insert("println_str".to_string(), Var { ptr: 0, type_: Type::Function(Box::new(Type::Void), vec![Type::Str]), assigned: true, named: true });
        functions.insert("println_string".to_string(), Var { ptr: 0, type_: Type::Function(Box::new(Type::Void), vec![Type::String]), assigned: true, named: true });

        functions.insert("join".to_string(), Var { ptr: 0, type_: Type::Function(Box::new(Type::String), vec![Type::Array(Box::new(Type::Str))]), assigned: true, named: true });

        Self {
            parser,
            data: String::new(),
            program: String::new(),
            functions,
            label: 0,
            break_label: Vec::new(),
            continue_label: Vec::new(),
        }
    }

    pub fn compile(&mut self) -> Result<&String> {
        let declarations = self.parser.parse()?;

        emit!(self, "call .Fmain");
        emit!(self, "sys @brk");

        for declaration in &declarations {
            match declaration {
                TopLevelDeclaration::Function(name, return_type, signature, _, _) => if self.functions.contains_key(name) {
                    return err!("redefinition of function");
                }
                else {
                    let sig = create_signature(return_type.clone(), signature);
                    if name == "main" && !is_main(&sig) {
                        return err!("main function has invalid signature");
                    }
                    self.functions.insert(name.clone(), Var { ptr: 0, type_: sig, assigned: true, named: true });
                }
                _ => unimplemented!()
            }
        }

        for declaration in &declarations {
            match declaration {
                TopLevelDeclaration::Function(..) => self.function(declaration)?,
                _ => unimplemented!()
            }
        }

        for file in fs::read_dir("include").unwrap() {
            self.program.push('\n');
            self.program.push_str(&fs::read_to_string(file.unwrap().path()).unwrap());
            self.program.push('\n');
        }

        self.program = format!("{}\n{}", self.program, self.data);

        Ok(&self.program)
    }

    fn drop_one(&mut self, t: &Type, scope: &Scope) {
        match t {
            Type::Array(_) => self.call(Some(".Pdroparr"), scope, false),
            Type::String => self.call(Some(".Pdropstr"), scope, false),
            _ => (),
        }
    }

    fn drop_all(&mut self, scope: &Scope, no_drop: Option<&String>) {
        for (ident, var) in scope.vars.iter() {
            if is_alloc(&var.type_) && var.assigned {
                emit!(self, "push {}", var.ptr);
                self.call(Some(".Pdecref"), scope, false);
                if let Some(s) = no_drop {
                    if ident == s {
                        emit!(self, "clean");
                        continue;
                    }
                }
                self.drop_one(&var.type_, scope);
                emit!(self, "clean");
            }
        }
    }

    fn function(&mut self, func: &TopLevelDeclaration) -> Result<()> {
        if let TopLevelDeclaration::Function(name, return_type, params, local_vars, block) = func {
            emit!(self, "\n#F{}", name);
            let mut scope = Scope::new(&self.functions);

            for param in params {
                if let Statement::Declaration(t, idents) = param {
                    for ident in idents {
                        let ptr = scope.declare(ident.to_string(), t.clone(), true)?;
                        emit!(self, "pull {}", ptr);
                        if is_alloc(t) {
                            self.call(Some(".Pincref"), &scope, false);
                        }
                        emit!(self, "clean");
                    }
                }
            }

            for var in local_vars {
                if let Statement::Declaration(t, idents) = var {
                    for ident in idents {
                        scope.declare(ident.to_string(), t.clone(), false)?;
                    }
                }
            }

            let returns = self.evaluate(block, &mut scope, return_type.clone())?;

            if let Type::Void = return_type {
                if !returns {
                    self.drop_all(&scope, None);
                    emit!(self, "push_l 0");
                    emit!(self, "ret");
                }

                Ok(())
            }
            else {
                if !returns {
                    err!("non-void function can terminate without returning")
                }
                else {
                    Ok(())
                }
            }
        }
        else {
            unimplemented!()
        }
    }

    fn evaluate(&mut self, statement: &Statement, scope: &mut Scope, ret: Type) -> Result<bool> {
        match statement {
            Statement::Declaration(t, idents) => {
                for ident in idents {
                    scope.declare(ident.clone(), t.clone(), false)?;
                }
                Ok(false)
            }
            Statement::Expression(e) => {
                let ret = self.calculate(e, scope)?;
                self.drop_one(&ret, scope);
                emit!(self, "clean");
                Ok(false)
            }
            Statement::Block(statements) => {
                let mut r = false;
                for s in statements {
                    r = r || self.evaluate(s, scope, ret.clone())?;
                }
                Ok(r)
            }
            Statement::If(condition, on_if, on_else) => {
                let l1 = self.get_label();
                let l2 = self.get_label();

                let ret_type = self.calculate(condition, scope)?;
                if ret_type != Type::Int {
                    return err!("expected int in condition");
                }

                emit!(self, "br .L{}", l1);
                let r1 = self.evaluate(&*on_else, scope, ret.clone())?;
                emit!(self, "jmp .L{}", l2);
                emit!(self, "#L{}", l1);
                let r2 = self.evaluate(&*on_if, scope, ret.clone())?;
                emit!(self, "#L{}", l2);

                Ok(r1 & r2)
            }
            Statement::While(condition, block) => {
                let l1 = self.get_label();
                let l2 = self.get_label();
                let l3 = self.get_label();

                emit!(self, "jmp .L{}", l2);
                emit!(self, "#L{}", l1);
                self.break_label.push(l3);
                self.continue_label.push(l2);
                self.evaluate(&*block, scope, ret)?;
                self.break_label.pop();
                self.continue_label.pop();
                emit!(self, "#L{}", l2);
                let ret_type = self.calculate(condition, scope)?;
                if ret_type != Type::Int {
                    return err!("expected int in condition");
                }
                emit!(self, "br .L{}", l1);
                emit!(self, "#L{}", l3);

                Ok(false)
            }
            Statement::Return(expr) => {
                let mut no_drop = None;
                if let Some(e) = expr {
                    Self::validate_types(ret, self.calculate(e, scope)?)?;

                    if let Expression::Term(Symbol::Identifier(s)) = e {
                        no_drop = Some(s);
                    }
                }
                else {
                    if ret != Type::Void {
                        return err!("expected return value");
                    }

                    emit!(self, "push_l 0");
                }

                self.drop_all(scope, no_drop);

                emit!(self, "ret");

                Ok(true)
            }
            Statement::Break(depth) => {
                emit!(self, "jmp .L{}", self.break_label[self.break_label.len() - 1 - depth]);
                Ok(false)
            }
            Statement::Continue(depth) => {
                emit!(self, "jmp .L{}", self.continue_label[self.continue_label.len() - 1 - depth]);
                Ok(false)
            }
        }
    }

    fn calculate(&mut self, expr: &Expression, scope: &mut Scope) -> Result<Type> {
        match expr {
            Expression::Term(Symbol::Float(f)) => {
                emit!(self, "push_l {}", u64::deserialize(f.serialize()));
                Ok(Type::Float)
            }
            Expression::Term(Symbol::Int(i)) => {
                emit!(self, "push_l {}", i);
                Ok(Type::Int)
            }
            Expression::Term(Symbol::Str(s)) => {
                emit!(self, "push_l '{}", s);
                Ok(Type::Str)
            }
            Expression::Term(Symbol::String(s)) => {
                let ld = self.write(s);
                emit!(self, "push_l .L{}", ld);
                emit!(self, "push_l {}", s.len() + 16);
                self.call(Some(".Pmake_string"), scope, true);
                Ok(Type::String)
            }
            Expression::Term(Symbol::Array(items)) => {
                let mut arr_type: Option<Type> = None;
                for item in items {
                    let item_type = self.calculate(&item, scope)?;
                    if let Some(t) = &arr_type {
                        if item_type != t.clone() {
                            return err!("mixed types in array");
                        }
                    }
                    else {
                        arr_type = Some(item_type.clone());
                    }
                }
                emit!(self, "push_l {}", items.len() * 8);
                self.call(Some(".Pmake_array"), scope, true);

                Ok(if let Some(t) = arr_type {
                    Type::Array(Box::new(t))
                }
                else {
                    Type::ArrayInfer
                })
            }
            Expression::Term(Symbol::Identifier(ident)) => {
                let var = scope.get_var(&ident, false)?;
                if !var.assigned {
                    return err!("attempt to use unassined variable");
                }
                if var.named {
                    emit!(self, "push_l {}", ".F".to_string() + ident);
                }
                else {
                    emit!(self, "push {}", var.ptr);
                }
                Ok(var.type_)
            }
            Expression::Call(name, params) => {
                let mut called_sig = Vec::new();
                for param in params.iter().rev() {
                    called_sig.push(self.calculate(param, scope)?);
                }

                let signature = self.calculate(name, scope)?;
                self.call(None, scope, true);

                if let Type::Function(ret, params) = signature {
                    if called_sig.len() != params.len() || called_sig.iter().rev().zip(params.iter()).any(|s| !Self::types_match(s.1.clone(), s.0.clone())) {
                        return err!("incorrect signature on function call, expected {:?} got {:?}", params, called_sig);
                    }

                    Ok(*ret)
                }
                else {
                    err!("{:?} cannot be called", signature)
                }
            }
            Expression::Index(arr, idx) => {
                let arr_type = self.calculate(arr, scope)?;

                match arr_type {
                    Type::Array(inner) => {
                        emit!(self, "copy");
                        emit!(self, "push_l 8");
                        emit!(self, "i_add");
                        emit!(self, "deref");
                        let idx_type = self.calculate(idx, scope)?;
                        if idx_type != Type::Int {
                            return err!("{:?} cannot index", idx_type);
                        }
                        emit!(self, "push_l 8");
                        emit!(self, "i_mul");
                        emit!(self, "clone 16");
                        self.call(Some(".Pindex_check"), scope, true);
                        emit!(self, "swap");
                        emit!(self, "clean");
                        emit!(self, "swap");
                        emit!(self, "push_l 24");
                        emit!(self, "i_add");
                        emit!(self, "deref");
                        emit!(self, "i_add");
                        emit!(self, "deref");

                        Ok(*inner)
                    }
                    Type::String => {
                        emit!(self, "copy");
                        emit!(self, "push_l 8");
                        emit!(self, "i_add");
                        emit!(self, "deref");
                        let idx_type = self.calculate(idx, scope)?;
                        if idx_type != Type::Int {
                            return err!("{:?} cannot index", idx_type);
                        }
                        emit!(self, "clone 16");
                        self.call(Some(".Pindex_check"), scope, true);
                        emit!(self, "swap");
                        emit!(self, "clean");
                        emit!(self, "push_l 16");
                        emit!(self, "i_add");
                        emit!(self, "i_add");
                        emit!(self, "deref");
                        emit!(self, "push_l 255");
                        emit!(self, "b_and");

                        Ok(Type::Str)
                    }
                    _ => err!("{:?} cannot be indexed", arr_type),
                }
            }
            Expression::BinaryOperation(op, lhs, rhs) => {
                match op {
                    Operation::Assign => {
                        return match &**lhs {
                            Expression::Term(Symbol::Identifier(s)) => {
                                let assigned = scope.get_var(&s, false)?.assigned;
                                let var = scope.get_var(&s, true)?;
                                if is_alloc(&var.type_) && assigned {
                                    emit!(self, "push {}", var.ptr);
                                    self.call(Some(".Pdecref"), scope, false);
                                    self.drop_one(&var.type_, scope);
                                    emit!(self, "clean");
                                }
                                let type_ret = self.calculate(rhs, scope)?;
                                emit!(self, "pull {}", var.ptr);

                                if is_alloc(&type_ret) {
                                    self.call(Some(".Pincref"), scope, false);
                                }

                                Self::validate_types(var.type_, type_ret)
                            }
                            Expression::Index(arr, idx) => {
                                let arr_type = self.calculate(arr, scope)?;

                                match arr_type {
                                    Type::Array(inner) => {
                                        emit!(self, "copy");
                                        emit!(self, "push_l 8");
                                        emit!(self, "i_add");
                                        emit!(self, "deref");
                                        let idx_type = self.calculate(idx, scope)?;
                                        if idx_type != Type::Int {
                                            return err!("{:?} cannot index", idx_type);
                                        }
                                        emit!(self, "push_l 8");
                                        emit!(self, "i_mul");
                                        emit!(self, "clone 16");
                                        self.call(Some(".Pindex_check"), scope, true);
                                        emit!(self, "swap");
                                        emit!(self, "clean");
                                        emit!(self, "swap");
                                        emit!(self, "push_l 24");
                                        emit!(self, "i_add");
                                        emit!(self, "deref");
                                        emit!(self, "i_add");
                                        let type_ret = self.calculate(rhs, scope)?;
                                        emit!(self, "swap");
                                        emit!(self, "clone 16");
                                        emit!(self, "clean");
                                        emit!(self, "store");
                                        emit!(self, "clean");

                                        Self::validate_types(*inner, type_ret)
                                    }
                                    Type::String => {
                                        emit!(self, "copy");
                                        emit!(self, "push_l 8");
                                        emit!(self, "i_add");
                                        emit!(self, "deref");
                                        let idx_type = self.calculate(idx, scope)?;
                                        if idx_type != Type::Int {
                                            return err!("{:?} cannot index", idx_type);
                                        }
                                        emit!(self, "clone 16");
                                        self.call(Some(".Pindex_check"), scope, true);
                                        emit!(self, "swap");
                                        emit!(self, "clean");

                                        emit!(self, "offset {}", scope.ptr);
                                        emit!(self, "push_l 16");
                                        emit!(self, "i_add");
                                        emit!(self, "i_add");
                                        emit!(self, "copy");
                                        emit!(self, "deref");
                                        emit!(self, "push_l 18446744073709551360");
                                        emit!(self, "b_and");
                                        let type_ret = self.calculate(rhs, scope)?;
                                        emit!(self, "pull 0");
                                        emit!(self, "push_l 255");
                                        emit!(self, "b_and");
                                        emit!(self, "b_or");
                                        emit!(self, "store");
                                        emit!(self, "clean");
                                        emit!(self, "push 0");
                                        emit!(self, "unoffset {}", scope.ptr);

                                        if type_ret == Type::Str {
                                            Ok(type_ret)
                                        }
                                        else {
                                            err!("only str can set a string")
                                        }
                                    }
                                    _ => err!("{:?} cannot be indexed", arr_type),
                                }
                            }
                            _ => err!("expected assignable value")
                        }
                    }
                    Operation::Cast => {
                        let type_from = self.calculate(&*lhs, scope)?;
                        return if let Expression::Term(Symbol::Type(type_to)) = &**rhs {
                            self.convert(&type_from, &type_to)?;
                            Ok(type_to.clone())
                        }
                        else {
                            unreachable!();
                        }
                    }
                    Operation::LogicalAnd => {
                        let l1 = self.get_label();
                        let l2 = self.get_label();

                        let type_l = self.calculate(&*lhs, scope)?;
                        if type_l != Type::Int {
                            return err!("expected int in condition");
                        }

                        emit!(self, "br .L{}", l1);
                        emit!(self, "push_l 0");
                        emit!(self, "jmp .L{}", l2);
                        emit!(self, "#L{}", l1);
                        let type_r = self.calculate(&*rhs, scope)?;
                        if type_r != Type::Int {
                            return err!("expected int in condition");
                        }
                        emit!(self, "#L{}", l2);

                        return Ok(Type::Int);
                    }
                    Operation::LogicalOr => {
                        let l1 = self.get_label();

                        let type_l = self.calculate(&*lhs, scope)?;
                        if type_l != Type::Int {
                            return err!("expected int in condition");
                        }

                        emit!(self, "copy");
                        emit!(self, "br .L{}", l1);
                        emit!(self, "clean");
                        let type_r = self.calculate(&*rhs, scope)?;
                        if type_r != Type::Int {
                            return err!("expected int in condition");
                        }
                        emit!(self, "#L{}", l1);

                        return Ok(Type::Int);
                    }
                    _ => ()
                }

                let type_r = self.calculate(&*rhs, scope)?.clone();
                let type_l = self.calculate(&*lhs, scope)?.clone();

                if let (Type::Array(inner), Operation::Push) = (&type_l, op) {
                    if type_r == **inner {
                        self.call(Some(".Ppush"), scope, false);

                        return Ok(type_l);
                    }
                }

                match (type_r.clone(), type_l.clone()) {
                    (Type::Int, Type::Int) => match op {
                        Operation::Add => { emit!(self, "i_add"); Ok(Type::Int) }
                        Operation::Subtract => { emit!(self, "i_sub"); Ok(Type::Int) }
                        Operation::Multiply => { emit!(self, "i_mul"); Ok(Type::Int) }
                        Operation::Divide => {
                            self.call(Some(".Pdivide"), scope, false);
                            Ok(Type::Int)
                        }
                        Operation::Remainder => {
                            self.call(Some(".Premainder"), scope, false);
                            Ok(Type::Int)
                        }
                        Operation::Equal => { emit!(self, "i_eq"); Ok(Type::Int) }
                        Operation::Inequal => { emit!(self, "i_neq"); Ok(Type::Int) }
                        Operation::Less => { emit!(self, "i_lt"); Ok(Type::Int) }
                        Operation::Greater => { emit!(self, "i_gt"); Ok(Type::Int) }
                        Operation::LessOrEqual => { emit!(self, "i_lte"); Ok(Type::Int) }
                        Operation::GreaterOrEqual => { emit!(self, "i_gte"); Ok(Type::Int) }
                        Operation::LeftShift => { emit!(self, "shl"); Ok(Type::Int) }
                        Operation::RightShift => { emit!(self, "shr"); Ok(Type::Int) }
                        Operation::BitwiseAnd => { emit!(self, "b_and"); Ok(Type::Int) }
                        Operation::BitwiseOr => { emit!(self, "b_or"); Ok(Type::Int) }
                        Operation::BitwiseXor => { emit!(self, "b_xor"); Ok(Type::Int) }
                        _ => err!("the operation ({:?}) is not defined for these types: {:?} and {:?}", op, type_r, type_l),
                    }
                    (Type::Float, Type::Float) => match op {
                        Operation::Add => { emit!(self, "f_add"); Ok(Type::Float) }
                        Operation::Subtract => { emit!(self, "f_sub"); Ok(Type::Float) }
                        Operation::Multiply => { emit!(self, "f_mul"); Ok(Type::Float) }
                        Operation::Divide => { emit!(self, "f_div"); Ok(Type::Float) }
                        Operation::Remainder => { emit!(self, "f_rem"); Ok(Type::Float) }
                        Operation::Equal => { emit!(self, "f_eq"); Ok(Type::Float) }
                        Operation::Inequal => { emit!(self, "f_neq"); Ok(Type::Float) }
                        Operation::Less => { emit!(self, "f_lt"); Ok(Type::Float) }
                        Operation::Greater => { emit!(self, "f_gt"); Ok(Type::Float) }
                        Operation::LessOrEqual => { emit!(self, "f_lte"); Ok(Type::Float) }
                        Operation::GreaterOrEqual => { emit!(self, "f_gte"); Ok(Type::Float) }
                        _ => err!("the operation is not defined for these types"),
                    }
                    _ => err!("the operation ({:?}) is not defined for these types: {:?} and {:?}", op, type_r, type_l),
                }
            }
            Expression::UnaryOperation(op, arg) => {
                let type_ret = self.calculate(&*arg, scope)?;

                if let Type::Array(inner) = type_ret.clone() {
                    match op {
                        Operation::Length => {
                            emit!(self, "push_l 8");
                            emit!(self, "i_add");
                            emit!(self, "deref");
                            emit!(self, "push_l 8");
                            emit!(self, "swap");
                            emit!(self, "i_div");
                            return Ok(Type::Int);
                        }
                        Operation::Pop => {
                            self.call(Some(".Ppop"), scope, false);
                            return Ok(*inner);
                        }
                        _ => ()
                    }
                }

                match type_ret {
                    Type::Int => match op {
                        Operation::Negate => { emit!(self, "i_neg"); Ok(Type::Int) },
                        Operation::LogicalNot => { emit!(self, "i_not"); Ok(Type::Int) },
                        Operation::BitwiseNot => { emit!(self, "b_not"); Ok(Type::Int) },
                        _ => err!("operation is not defined for this type"),
                    }
                    Type::Float => match op {
                        Operation::Negate => { emit!(self, "f_neg"); Ok(Type::Float) },
                        Operation::LogicalNot => { emit!(self, "f_not"); Ok(Type::Float) },
                        _ => err!("operation is not defined for this type"),
                    }
                    Type::String => match op {
                        Operation::Length => {
                            emit!(self, "push_l 8");
                            emit!(self, "i_add");
                            emit!(self, "deref");
                            Ok(Type::Int)
                        }
                        _ => err!("operation is not defined for this type"),
                    }
                    _ => err!("operation is not defined for this type"),
                }
            }
            _ => unreachable!(),
        }
    }

    fn emit(&mut self, line: &str) {
        self.program += line;
        self.program.push('\n');
    }

    fn call(&mut self, name: Option<&str>, scope: &Scope, frame: bool) {
        let call_instruction = if let Some(s) = name {
            format!("call {}", s)
        }
        else {
            "call_s".to_string()
        };

        if scope.ptr != 0 && frame {
            emit!(self, "offset {}", scope.ptr);
            emit!(self, "{}", call_instruction);
            emit!(self, "unoffset {}", scope.ptr);
        }
        else {
            emit!(self, "{}", call_instruction);
        }
    }

    fn write(&mut self, data: &str) -> usize {
        let mut qwords = Vec::new();
        let mut qword: u64 = 0;
        for (i, b) in data.bytes().enumerate() {
            qword <<= 8;
            qword |= b as u64;
            if (i + 1) % 8 == 0 {
                qwords.push(format!("'{}", qword));
                qword = 0;
            }
            else if i == data.len() - 1 {
                qword <<= 8 * (8 - (i + 1) % 8);
                qwords.push(format!("'{}", qword));
            }
        }
        let ld = self.get_label();
        self.data.push_str(&format!("#L{} 0 {} {}\n", ld, data.len().to_string(), qwords.join(" ")));
        ld
    }

    fn get_label(&mut self) -> usize {
        self.label += 1;
        self.label
    }

    fn convert(&mut self, type_from: &Type, type_to: &Type) -> Result<()> {
        if mem::discriminant(type_from) != mem::discriminant(type_to) {
            match (type_from, type_to) {
                (Type::Int, Type::Str) => (),
                (Type::Str, Type::Int) => (),
                (Type::Int, Type::Float) => emit!(self, "itf"),
                (Type::Float, Type::Int) => emit!(self, "fti"),
                _ => return err!("this cast cannnot be done")
            }
        }

        Ok(())
    }

    fn validate_types(stated: Type, seen: Type) -> Result<Type> {
        if Self::types_match(stated.clone(), seen.clone()) {
            Ok(stated)
        }
        else {
            err!("mimatched types {:?} and {:?}", stated, seen)
        }
    }

    fn types_match(stated: Type, seen: Type) -> bool {
        if is_array(&stated) {
            if seen == Type::ArrayInfer {
                true
            }
            else {
                if let (Type::Array(stated_inner), Type::Array(seen_inner)) = (stated, seen) {
                    Self::types_match(*stated_inner, *seen_inner)
                }
                else {
                    false
                }
            }
        }
        else {
            stated == seen
        }
    }
}
