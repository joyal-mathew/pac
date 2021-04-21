use crate::{ lexing::Type, utils::{ Result, Serial }, parsing::* };
use std::{ mem, fs, collections::HashMap };

#[derive(Clone)]
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

fn is_main(sig: &Type) -> bool {
    if let Type::Function(return_type, param_types) = sig {
        return **return_type == Type::Void && param_types.is_empty()
    }

    return false;
}

struct Scope {
    vars: HashMap<String, Var>,
    ptr: usize,
    allocations: Vec<(usize, Type)>,
}

impl Scope {
    fn new(global: &HashMap<String, Var>) -> Self {
        Self {
            vars: global.clone(),
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
            self.vars.insert(ident, Var { type_, ptr: self.ptr, assigned: false, named: false });
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
    would_alloc: [Type; 2],

    break_label: Vec<usize>,
    continue_label: Vec<usize>,
}

impl Compiler {
    const REF_INC: usize = 1;
    const REF_DEC: usize = 2;
    const REF_DROP: usize = 4;

    pub fn new(parser: Parser) -> Self {
        let mut functions = HashMap::new();

        functions.insert("print_str".to_string(), Var { ptr: 0, type_: Type::Function(Box::new(Type::Void), vec![Type::Str]), assigned: true, named: true });
        functions.insert("print_string".to_string(), Var { ptr: 0, type_: Type::Function(Box::new(Type::Void), vec![Type::String]), assigned: true, named: true });
        functions.insert("println_str".to_string(), Var { ptr: 0, type_: Type::Function(Box::new(Type::Void), vec![Type::Str]), assigned: true, named: true });
        functions.insert("println_string".to_string(), Var { ptr: 0, type_: Type::Function(Box::new(Type::Void), vec![Type::String]), assigned: true, named: true });

        functions.insert("get_char".to_string(), Var { ptr: 0, type_: Type::Function(Box::new(Type::Str), vec![Type::String, Type::Int]), assigned: true, named: true });
        functions.insert("set_char".to_string(), Var { ptr: 0, type_: Type::Function(Box::new(Type::Void), vec![Type::String, Type::Int, Type::Str]), assigned: true, named: true });
        functions.insert("str_len".to_string(), Var { ptr: 0, type_: Type::Function(Box::new(Type::Int), vec![Type::String]), assigned: true, named: true });

        Self {
            parser,
            data: String::new(),
            program: String::new(),
            functions,
            label: 0,
            would_alloc: [ Type::String, Type::Array(Box::new(Type::Void)) ],
            break_label: Vec::new(),
            continue_label: Vec::new(),
        }
    }

    fn requires_alloc(&self, t: &Type) -> bool {
        self.would_alloc.iter().any(|a| mem::discriminant(a) == mem::discriminant(t))
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

        for line in self.program.split('\n') {
            println!("{}", line);
        }

        Ok(&self.program)
    }

    fn count_refs(&mut self, t: &Type, scope: &Scope, op: usize) {
        if op & Self::REF_INC != 0 {
            self.emit("call .Pincref")
        }
        if op & Self::REF_DEC != 0 {
            self.emit("call .Pdecref")
        }
        if op & Self::REF_DROP != 0 {
            self.emit("call .Pdrop")
        }

        if let Type::Array(inner) = t {
            if self.requires_alloc(inner) {
                let l1 = self.get_label();
                let l2 = self.get_label();

                if scope.ptr != 0 {
                    self.emit(&format!("offset {}", scope.ptr));
                }
                self.emit("copy"); // TODO: move to .paa file
                self.emit("push_l 8");
                self.emit("i_add");
                self.emit("deref");
                self.emit("push_l 24");
                self.emit("swap");
                self.emit("i_sub");
                self.emit("push_l 8");
                self.emit("swap");
                self.emit("i_div");
                self.emit("pop 8");
                self.emit("push_l 0");
                self.emit("pop 0");
                self.emit("copy");
                self.emit("push_l 24");
                self.emit("i_add");
                self.emit("pop 16");
                self.emit(&format!("#L{}", l1));
                self.emit("push 0");
                self.emit("push 8");
                self.emit("i_lte");
                self.emit(&format!("br .L{}", l2));
                self.emit("push 0");
                self.emit("push_l 8");
                self.emit("i_mul");
                self.emit("push 16");
                self.emit("i_add");
                self.emit("deref");
                self.count_refs(inner, scope, op);
                self.emit("clean");
                self.emit("push 0");
                self.emit("push_l 1");
                self.emit("i_add");
                self.emit("pop 0");
                self.emit(&format!("jmp .L{}", l1));
                self.emit(&format!("#L{}", l2));
                if scope.ptr != 0 {
                    self.emit(&format!("unoffset {}", scope.ptr));
                }
            }
        }
    }

    fn function(&mut self, func: &TopLevelDeclaration) -> Result<()> {
        if let TopLevelDeclaration::Function(name, return_type, params, block) = func {
            self.emit(&format!("\n#F{}", name));
            let mut scope = Scope::new(&self.functions);

            for param in params {
                if let Statement::Declaration(t, idents) = param {
                    for ident in idents {
                        let ptr = scope.delcare(ident.to_string(), t.clone())?;
                        if self.requires_alloc(t) {
                            scope.allocations.push((ptr, t.clone()));
                            self.count_refs(t, &scope, Self::REF_INC);
                        }
                        self.emit(&format!("pop {}", ptr));
                    }
                }
            }

            let returns = self.evaluate(block, &mut scope, return_type.clone())?;

            if let Type::Void = return_type {
                if !returns {
                    for (ptr, t) in &scope.allocations {
                        self.emit(&format!("push {}", ptr));
                        self.count_refs(t, &scope, Self::REF_DEC | Self::REF_DROP);
                        self.emit("clean");
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
                if self.requires_alloc(&ret_type) {
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

                    if self.requires_alloc(&ret) {
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

                for (ptr, t) in &scope.allocations {
                    self.emit(&format!("push {}", ptr));
                    self.count_refs(t, scope, Self::REF_DEC | match no_drop {
                        Some(p) => if p != *ptr {
                            Self::REF_DROP
                        }
                        else {
                            0
                        }
                        None => Self::REF_DROP,
                    });
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

    fn lvalue(&mut self, expr: &Expression, scope: &mut Scope) -> Result<Var> {
        match expr {
            Expression::Term(Symbol::Identifier(ident)) => {
                let var = scope.get_var(&ident)?;
                self.emit(&format!("pull {}", var.ptr));
                Ok(var)
            },
            Expression::Index(arr, idx) => {
                let arr_type = self.calculate(arr, scope)?;
                let idx_type = self.calculate(idx, scope)?;

                if let Type::Array(inner) = arr_type {
                    if idx_type != Type::Int {
                        return err!("{:?} cannot index", idx_type);
                    }

                    self.emit("copy 8");
                    self.emit("push_l 8");
                    self.emit("i_mul");
                    self.emit("push_l 24");
                    self.emit("i_add");
                    self.emit("i_add");
                    self.emit("swap");
                    self.emit("store");

                    Ok(Var { ptr: 0, type_: *inner, assigned: true, named: false })
                }
                else {
                    err!("{:?} cannot be indexed", arr_type)
                }
            }
            _ => err!("expected an assignable value")
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
                self.call(Some(".Pmake_string"), scope);
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
                self.call(Some(".Pmake_array"), scope);

                Ok(if let Some(t) = arr_type {
                    Type::Array(Box::new(t))
                }
                else {
                    Type::ArrayInfer
                })
            }
            Expression::Term(Symbol::Identifier(ident)) => {
                let var = scope.get_var(&ident)?;
                if var.named {
                    self.emit(&format!("push_l {}", ".F".to_string() + ident));
                }
                else {
                    self.emit(&format!("push {}", var.ptr));
                }
                Ok(var.type_)
            }
            Expression::Call(name, params) => {
                let mut called_sig = Vec::new();
                for param in params.iter().rev() {
                    called_sig.push(self.calculate(param, scope)?);
                }

                let signature = self.calculate(name, scope)?;
                self.call(None, scope);

                if let Type::Function(ret, params) = signature {
                    if called_sig.len() != params.len() || called_sig.iter().rev().zip(params.iter()).any(|s| s.0 != s.1) {
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
                let idx_type = self.calculate(idx, scope)?;

                if let Type::Array(inner) = arr_type {
                    if idx_type != Type::Int {
                        return err!("{:?} cannot index", idx_type);
                    }

                    self.emit("push_l 8");
                    self.emit("i_mul");
                    self.emit("push_l 24");
                    self.emit("i_add");
                    self.emit("i_add");
                    self.emit("deref");

                    Ok(*inner)
                }
                else {
                    err!("{:?} cannot be indexed", arr_type)
                }
            }
            Expression::BinaryOperation(op, lhs, rhs) => {
                match op {
                    Operation::Assign => {
                        let type_ret = self.calculate(&*rhs, scope)?;
                        let var = self.lvalue(&**lhs, scope)?;

                        return if type_ret == var.type_ {
                            if self.requires_alloc(&type_ret) {
                                if var.assigned {
                                    self.emit(&format!("push"));
                                    self.emit("call .Pdecref");
                                    self.emit("call .Pdrop");
                                }
                                scope.allocations.push((var.ptr, var.type_));
                                self.count_refs(&type_ret, scope, Self::REF_INC);
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

    fn call(&mut self, name: Option<&str>, scope: &Scope) {
        let call_instruction = if let Some(s) = name {
            format!("call {}", s)
        }
        else {
            "call_s".to_string()
        };

        if scope.ptr != 0 {
            self.emit(&format!("offset {}", scope.ptr));
            self.emit(&call_instruction);
            self.emit(&format!("unoffset {}", scope.ptr));
        }
        else {
            self.emit(&call_instruction);
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
