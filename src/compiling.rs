use crate::{ lexing::Type, utils::{ Result, Serial }, parsing::* };
use std::{ mem, fs, collections::HashMap };

const ALLOCATIONS: [Type; 1] = [ Type::String ];

#[derive(Clone)]
struct Var {
    ptr: usize,
    type_: Type,
    assigned: bool
}

struct Signature {
    return_type: Type,
    param_types: Vec<Type>,
}

impl Signature {
    fn new(return_type: Type, params: &Vec<Statement>) -> Self {
        let mut param_types = Vec::new();

        for param in params {
            if let Statement::Declaration(t, v) = param {
                param_types.extend(std::iter::repeat(t.clone()).take(v.len()));
            }
        }

        Self {
            return_type,
            param_types,
        }
    }

    fn is_main(&self) -> bool {
        self.return_type == Type::Void && self.param_types.is_empty()
    }
}

struct Scope {
    vars: HashMap<String, Var>,
    ptr: usize,
    allocations: Vec<usize>,
}

impl Scope {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            ptr: 0,
            allocations: Vec::new(),
        }
    }

    fn get_var(&self, name: &String) -> Result<Var> {
        self.vars.get(name).cloned().ok_or(format!("undefined variable {}", name))
    }

    fn delcare(&mut self, ident: String, type_: Type) -> Result<usize> {
        if self.vars.contains_key(&ident) {
            err!("variable already delcared in this scope")
        }
        else {
            self.vars.insert(ident, Var { type_, ptr: self.ptr, assigned: false });
            self.ptr += 8;
            Ok(self.ptr - 8)
        }
    }
}

pub struct Compiler {
    parser: Parser,

    data: String,
    program: String,

    functions: HashMap<String, Signature>,
    label: usize,

    break_label: Vec<usize>,
    continue_label: Vec<usize>,
}

impl Compiler {
    pub fn new(parser: Parser) -> Self {
        let mut functions = HashMap::new();

        functions.insert("print_str".to_string(), Signature { return_type: Type::Void, param_types: vec![Type::Str] });
        functions.insert("print_string".to_string(), Signature { return_type: Type::Void, param_types: vec![Type::String] });
        functions.insert("println_str".to_string(), Signature { return_type: Type::Void, param_types: vec![Type::Str] });
        functions.insert("println_string".to_string(), Signature { return_type: Type::Void, param_types: vec![Type::String] });
        functions.insert("get_char".to_string(), Signature { return_type: Type::Str, param_types: vec![Type::String, Type::Int] });
        functions.insert("set_char".to_string(), Signature { return_type: Type::Void, param_types: vec![Type::String, Type::Int, Type::Str] });
        functions.insert("get_len".to_string(), Signature { return_type: Type::Int, param_types: vec![Type::String] });

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

        self.emit("call .Fmain");
        self.emit("sys @brk");

        for declaration in &declarations {
            match declaration {
                TopLevelDeclaration::Function(name, return_type, signature, _) => if self.functions.contains_key(name) {
                    return err!("redefinition of function");
                }
                else {
                    let sig = Signature::new(return_type.clone(), signature);
                    if name == "main" && !sig.is_main() {
                        return err!("main function has invalid signature");
                    }
                    self.functions.insert(name.clone(), sig);
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

        for line in self.program.split('\n') {
            println!("{}", line);
        }

        Ok(&self.program)
    }

    fn function(&mut self, func: &TopLevelDeclaration) -> Result<()> {
        if let TopLevelDeclaration::Function(name, return_type, params, block) = func {
            self.emit(&format!("\n#F{}", name));
            let mut scope = Scope::new();

            for param in params {
                if let Statement::Declaration(t, idents) = param {
                    for ident in idents {
                        let ptr = scope.delcare(ident.to_string(), t.clone())?;
                        if ALLOCATIONS.contains(t) {
                            scope.allocations.push(ptr);
                            self.emit("call .Pincref");
                        }
                        self.emit(&format!("pop {}", ptr));
                    }
                }
            }

            let returns = self.evaluate(block, &mut scope, return_type.clone())?;

            if let Type::Void = return_type {
                if !returns {
                    for allocation in &scope.allocations {
                        self.emit(&format!("push {}", allocation));
                        self.emit("call .Pdecref");
                        self.emit("call .Pdrop");
                    }

                    self.emit("push_l 0");
                    self.emit("ret");
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
                    scope.delcare(ident.clone(), t.clone())?;
                }
                Ok(false)
            }
            Statement::Expression(e) => {
                let ret_type = self.calculate(e, scope)?;
                if ALLOCATIONS.contains(&ret_type) {
                    self.emit("call .Pdrop");
                }
                self.emit("clean");
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

                self.emit(&format!("br .L{}", l1));
                let r1 = self.evaluate(&*on_else, scope, ret.clone())?;
                self.emit(&format!("jmp .L{}", l2));
                self.emit(&format!("#L{}", l1));
                let r2 = self.evaluate(&*on_if, scope, ret.clone())?;
                self.emit(&format!("#L{}", l2));

                Ok(r1 & r2)
            }
            Statement::While(condition, block) => {
                let l1 = self.get_label();
                let l2 = self.get_label();
                let l3 = self.get_label();

                self.emit(&format!("jmp .L{}", l2));
                self.emit(&format!("#L{}", l1));
                self.break_label.push(l3);
                self.continue_label.push(l2);
                self.evaluate(&*block, scope, ret)?;
                self.break_label.pop();
                self.continue_label.pop();
                self.emit(&format!("#L{}", l2));
                let ret_type = self.calculate(condition, scope)?;
                if ret_type != Type::Int {
                    return err!("expected int in condition");
                }
                self.emit(&format!("br .L{}", l1));
                self.emit(&format!("#L{}", l3));

                Ok(false)
            }
            Statement::Return(expr) => {
                let mut no_drop = None;
                if let Some(e) = expr {
                    if self.calculate(e, scope)? != ret {
                        return err!("invalid return type");
                    }

                    if ALLOCATIONS.contains(&ret) {
                        if let Expression::Term(Symbol::Identifier(s)) = e {
                            no_drop = Some(scope.vars.get(s).unwrap().ptr);
                        }
                    }
                }
                else {
                    if ret != Type::Void {
                        return err!("expected return value");
                    }

                    self.emit("push_l 0");
                }

                for allocation in &scope.allocations {
                    self.emit(&format!("push {}", allocation));
                    self.emit("call .Pdecref");
                    match no_drop {
                        Some(p) => if p != *allocation {
                            self.emit("call .Pdrop");
                        }
                        None => self.emit("call .Pdrop"),
                    }
                    self.emit("clean");
                }

                self.emit("ret");

                Ok(true)
            }
            Statement::Break(depth) => {
                self.emit(&format!("jmp .L{}", self.break_label[self.break_label.len() - 1 - depth]));
                Ok(false)
            }
            Statement::Continue(depth) => {
                self.emit(&format!("jmp .L{}", self.continue_label[self.continue_label.len() - 1 - depth]));
                Ok(false)
            }
        }
    }

    fn lvalue(&self, expr: &Expression, scope: &Scope) -> Result<Var> {
        if let Expression::Term(Symbol::Identifier(ident)) = expr {
            scope.get_var(&ident)
        }
        else {
            err!("expected assignable value")
        }
    }

    fn calculate(&mut self, expr: &Expression, scope: &mut Scope) -> Result<Type> {
        match expr {
            Expression::Term(Symbol::Float(f)) => {
                self.emit(&format!("push_l {}", u64::deserialize(f.serialize())));
                Ok(Type::Float)
            }
            Expression::Term(Symbol::Int(i)) => {
                self.emit(&format!("push_l {}", i));
                Ok(Type::Int)
            }
            Expression::Term(Symbol::Str(s)) => {
                self.emit(&format!("push_l '{}", s));
                Ok(Type::Str)
            }
            Expression::Term(Symbol::String(s)) => {
                let ld = self.write(s);
                self.emit(&format!("push_l .L{}", ld));
                self.emit(&format!("push_l {}", s.len() + 16));
                self.call(".Pmake_string", scope);
                Ok(Type::String)
            }
            Expression::Term(Symbol::Array(items)) => {
                let length = items.len() * 8 + 24;
                self.emit("push_l 0");
                self.emit(&format!("push_l {}", length));
                self.emit(&format!("push_l {}", length));
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
                self.emit(&format!("push_l {}", length));
                self.call(".Pmake_array", scope);

                Ok(if let Some(t) = arr_type {
                    Type::Array(Box::new(t))
                }
                else {
                    Type::ArrayInfer
                })
            }
            Expression::Term(Symbol::Identifier(ident)) => {
                let var = scope.get_var(&ident)?;
                self.emit(&format!("push {}", var.ptr));
                Ok(var.type_)
            }
            Expression::Term(Symbol::Call(name, params)) => {
                let mut called_sig = Vec::new();
                for param in params.iter().rev() {
                    called_sig.push(self.calculate(param, scope)?);
                }

                self.call(&(".F".to_string() + name), scope);

                let signature = self.functions.get(name).ok_or("undefined function")?;
                if called_sig.len() != signature.param_types.len() || called_sig.iter().rev().zip(signature.param_types.iter()).any(|s| s.0 != s.1) {
                    return err!("incorrect signature on function call, expected {:?} got {:?}", signature.param_types, called_sig);
                }
                let ret_type = signature.return_type.clone();
                Ok(ret_type)
            }
            Expression::BinaryOperation(op, lhs, rhs) => {
                match op {
                    Operation::Assign => {
                        let type_ret = self.calculate(&*rhs, scope)?;
                        let var = self.lvalue(&**lhs, scope)?;

                        return if type_ret == var.type_ {
                            self.emit(&format!("pull {}", var.ptr));
                            if ALLOCATIONS.contains(&type_ret) {
                                if var.assigned {
                                    self.emit(&format!("push")); // TODO:
                                    self.emit("call .Pdecref");
                                    self.emit("call .Pdrop");
                                }
                                scope.allocations.push(var.ptr);
                                self.emit("call .Pincref");
                            }
                            Ok(type_ret)
                        }
                        else if mem::discriminant(&var.type_) == mem::discriminant(&Type::Array(Box::new(Type::Void))) {
                            Self::infer(var.type_, type_ret)
                        }
                        else {
                            err!("mismatched types on assignment: {:?} and {:?}", &var.type_, &type_ret)
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

                        self.emit(&format!("br .L{}", l1));
                        self.emit("push_l 0");
                        self.emit(&format!("jmp .L{}", l2));
                        self.emit(&format!("#L{}", l1));
                        let type_r = self.calculate(&*rhs, scope)?;
                        if type_r != Type::Int {
                            return err!("expected int in condition");
                        }
                        self.emit(&format!("#L{}", l2));

                        return Ok(Type::Int);
                    }
                    Operation::LogicalOr => {
                        let l1 = self.get_label();

                        let type_l = self.calculate(&*lhs, scope)?;
                        if type_l != Type::Int {
                            return err!("expected int in condition");
                        }

                        self.emit("copy");
                        self.emit(&format!("br .L{}", l1));
                        self.emit("clean");
                        let type_r = self.calculate(&*rhs, scope)?;
                        if type_r != Type::Int {
                            return err!("expected int in condition");
                        }
                        self.emit(&format!("#L{}", l1));

                        return Ok(Type::Int);
                    }
                    _ => ()
                }

                let type_r = self.calculate(&*rhs, scope)?.clone();
                let type_l = self.calculate(&*lhs, scope)?.clone();

                match (type_r.clone(), type_l.clone()) {
                    (Type::Int, Type::Int) => match op {
                        Operation::Add => { self.emit("i_add"); Ok(Type::Int) }
                        Operation::Subtract => { self.emit("i_sub"); Ok(Type::Int) }
                        Operation::Multiply => { self.emit("i_mul"); Ok(Type::Int) }
                        Operation::Divide => { self.emit("i_div"); Ok(Type::Int) }
                        Operation::Remainder => { self.emit("i_rem"); Ok(Type::Int) }
                        Operation::Equal => { self.emit("i_eq"); Ok(Type::Int) }
                        Operation::Inequal => { self.emit("i_neq"); Ok(Type::Int) }
                        Operation::Less => { self.emit("i_lt"); Ok(Type::Int) }
                        Operation::Greater => { self.emit("i_gt"); Ok(Type::Int) }
                        Operation::LessOrEqual => { self.emit("i_lte"); Ok(Type::Int) }
                        Operation::GreaterOrEqual => { self.emit("i_gte"); Ok(Type::Int) }
                        Operation::LeftShift => { self.emit("shl"); Ok(Type::Int) }
                        Operation::RightShift => { self.emit("shr"); Ok(Type::Int) }
                        Operation::BitwiseAnd => { self.emit("b_and"); Ok(Type::Int) }
                        Operation::BitwiseOr => { self.emit("b_or"); Ok(Type::Int) }
                        Operation::BitwiseXor => { self.emit("b_xor"); Ok(Type::Int) }
                        _ => err!("the operation ({:?}) is not defined for these types: {:?} and {:?}", op, type_r, type_l),
                    }
                    (Type::Float, Type::Float) => match op {
                        Operation::Add => { self.emit("f_add"); Ok(Type::Float) }
                        Operation::Subtract => { self.emit("f_sub"); Ok(Type::Float) }
                        Operation::Multiply => { self.emit("f_mul"); Ok(Type::Float) }
                        Operation::Divide => { self.emit("f_div"); Ok(Type::Float) }
                        Operation::Remainder => { self.emit("f_rem"); Ok(Type::Float) }
                        Operation::Equal => { self.emit("f_eq"); Ok(Type::Float) }
                        Operation::Inequal => { self.emit("f_neq"); Ok(Type::Float) }
                        Operation::Less => { self.emit("f_lt"); Ok(Type::Float) }
                        Operation::Greater => { self.emit("f_gt"); Ok(Type::Float) }
                        Operation::LessOrEqual => { self.emit("f_lte"); Ok(Type::Float) }
                        Operation::GreaterOrEqual => { self.emit("f_gte"); Ok(Type::Float) }
                        _ => err!("the operation is not defined for these types"),
                    }
                    _ => err!("the operation ({:?}) is not defined for these types: {:?} and {:?}", op, type_r, type_l),
                }
            }
            Expression::UnaryOperation(op, arg) => {
                let type_ret = self.calculate(&*arg, scope)?;

                match type_ret {
                    Type::Int => match op {
                        Operation::Negate => { self.emit("i_neg"); Ok(Type::Int) },
                        Operation::LogicalNot => { self.emit("i_not"); Ok(Type::Int) },
                        Operation::BitwiseNot => { self.emit("b_not"); Ok(Type::Int) },
                        _ => err!("operation is not defined for this type"),
                    }
                    Type::Float => match op {
                        Operation::Negate => { self.emit("f_neg"); Ok(Type::Float) },
                        Operation::LogicalNot => { self.emit("f_not"); Ok(Type::Float) },
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

    fn call(&mut self, name: &str, scope: &Scope) {
        if scope.ptr != 0 {
            self.emit(&format!("offset {}", scope.ptr));
            self.emit(&format!("call {}", name));
            self.emit(&format!("unoffset {}", scope.ptr));
        }
        else {
            self.emit(&format!("call {}", name));
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
                (Type::Int, Type::Float) => self.emit("itf"),
                (Type::Float, Type::Int) => self.emit("fti"),
                _ => return err!("this cast cannnot be done")
            }
        }

        Ok(())
    }

    fn infer(stated: Type, seen: Type) -> Result<Type> {
        let errmsg = err!("mismatched types, {:?} and {:?}", stated, seen);
        if let Type::ArrayInfer = seen {
            Ok(stated)
        }
        else if let (Type::Array(stated_inner), Type::Array(seen_inner)) = (stated, seen) {
            Self::infer(*stated_inner, *seen_inner)
        }
        else {
            errmsg
        }
    }
}
