use crate::utils::Result;
use crate::lexing::*;
use std::mem;

#[derive(Debug)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Equal,
    Inequal,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Negate,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    RightShift,
    LeftShift,
    BitwiseNot,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    Assign,
    Cast,
    Length,
    Pop,
    Push,
}

#[derive(Debug)]
pub enum Symbol {
    Int(u64),
    Float(f64),
    Str(u64),
    String(String),

    Identifier(String),
    Array(Vec<Expression>),
    Type(Type),
}

#[derive(Debug)]
pub enum TopLevelDeclaration {
    Function(String, Type, Vec<Statement>, Statement),
    Structure,
}

#[derive(Debug)]
pub enum Statement {
    Declaration(Type, Vec<String>),
    Expression(Expression),
    Block(Vec<Statement>),
    If(Expression, Box<Statement>, Box<Statement>),
    While(Expression, Box<Statement>),
    Break(usize),
    Continue(usize),
    Return(Option<Expression>),
}

#[derive(Debug)]
pub enum Expression {
    Term(Symbol),
    Call(Box<Expression>, Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    BinaryOperation(Operation, Box<Expression>, Box<Expression>),
    UnaryOperation(Operation, Box<Expression>),
}

pub struct Parser {
    lexer: Lexer,
    current: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            lexer,
            current: Token::EndOfFile,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<TopLevelDeclaration>> {
        self.advance()?;
        let mut declarations = Vec::new();

        loop {
            match self.current {
                Token::Keyword(Keyword::Function) => declarations.push(self.function()?),
                _ => break,
            }
        }

        expect!(self, Token::EndOfFile);

        Ok(declarations)
    }

    fn function(&mut self) -> Result<TopLevelDeclaration> {
        self.advance()?;
        let return_type = self.get_type().or::<String>(Ok(Type::Void))?;

        let mut signature = Vec::new();
        let name = if let Token::Identifier(s) = self.advance()? {
            Ok(s)
        }
        else {
            err!("expected identifier")
        }?;

        expect!(self, Token::Operator(Operator::OpenParenthesis));

        while let Token::Keyword(Keyword::Var) = self.current {
            signature.push(self.delcaration()?);
        }

        expect!(self, Token::Operator(Operator::CloseParenthesis));
        let block = self.block(&[Token::Keyword(Keyword::End)])?;
        self.advance()?;
        Ok(TopLevelDeclaration::Function(name, return_type, signature, block))
    }

    fn block(&mut self, terminators: &[Token]) -> Result<Statement> {
        let mut statements = Vec::new();

        loop {
            if terminators.iter().any(|t| *t == self.current) {
                break
            }

            statements.push(self.statement()?);
        }

        Ok(Statement::Block(statements))
    }

    fn statement(&mut self) -> Result<Statement> {
        match self.current {
            Token::Keyword(Keyword::If) => self.if_else(),
            Token::Keyword(Keyword::While) => self.while_loop(),
            _ => {
                let res = match self.current {
                    Token::Keyword(Keyword::Var) => self.delcaration()?,
                    Token::Keyword(Keyword::Return) => self.return_statement()?,
                    Token::Keyword(Keyword::Break) => Statement::Break(self.depth()?),
                    Token::Keyword(Keyword::Continue) => Statement::Continue(self.depth()?),
                    _ => Statement::Expression(self.expression()?),
                };

                expect!(self, Token::Operator(Operator::Semicolon));
                Ok(res)
            }
        }
    }

    fn depth(&mut self) -> Result<usize> {
        self.advance()?;
        if let Token::IntegerLiteral(i) = self.current {
            self.advance()?;
            Ok(i as usize)
        }
        else {
            Ok(0)
        }
    }

    fn return_statement(&mut self) -> Result<Statement> {
        self.advance()?;
        if let Token::Operator(Operator::Semicolon) = self.current {
            Ok(Statement::Return(None))
        }
        else {
            Ok(Statement::Return(Some(self.expression()?)))
        }
    }

    fn if_else(&mut self) -> Result<Statement> {
        self.advance()?;
        let condition = self.expression()?;
        expect!(self, Token::Keyword(Keyword::Then));

        let on_if = self.block(&[Token::Keyword(Keyword::Else), Token::Keyword(Keyword::End), Token::Keyword(Keyword::Elif)])?;

        Ok(if let Token::Keyword(Keyword::Else) = self.current {
            self.advance()?;
            let on_else = self.block(&[Token::Keyword(Keyword::End)])?;
            self.advance()?;
            Statement::If(condition, Box::new(on_if), Box::new(on_else))
        }
        else if let Token::Keyword(Keyword::Elif) = self.current {
            let on_else = self.if_else()?;
            Statement::If(condition, Box::new(on_if), Box::new(on_else))
        }
        else {
            expect!(self, Token::Keyword(Keyword::End));
            Statement::If(condition, Box::new(on_if), Box::new(Statement::Block(Vec::new())))
        })
    }

    fn while_loop(&mut self) -> Result<Statement> {
        self.advance()?;
        let condition = self.expression()?;
        expect!(self, Token::Keyword(Keyword::Do));
        let block = self.block(&[Token::Keyword(Keyword::End)])?;
        self.advance()?;

        Ok(Statement::While(condition, Box::new(block)))
    }

    fn delcaration(&mut self) -> Result<Statement> {
        self.advance()?;
        let typ = self.get_type()?;
        let mut idents = Vec::new();
        while let Token::Identifier(_) = self.current {
            if let Token::Identifier(ident) = self.advance()? {
                idents.push(ident)
            }
        }
        if idents.len() < 1 {
            err!("expected at least one identifier {:?}", typ)
        }
        else {
            Ok(Statement::Declaration(typ, idents))
        }
    }

    fn expression(&mut self) -> Result<Expression> {
        self.assignment()
    }

    parse_rule!(
        name = assignment, parent = push,
        Operator::Assignment => Operation::Assign
    );
    parse_rule!(
        name = push, parent = logical_or,
        Operator::Push => Operation::Push
    );
    parse_rule!(
        name = logical_or, parent = logical_and,
        Operator::LogicalOr => Operation::LogicalOr
    );
    parse_rule!(
        name = logical_and, parent = bitwise_or,
        Operator::LogicalAnd => Operation::LogicalAnd
    );
    parse_rule!(
        name = bitwise_or, parent = bitwise_xor,
        Operator::BitwiseOr => Operation::BitwiseOr
    );
    parse_rule!(
        name = bitwise_xor, parent = bitwise_and,
        Operator::BitwiseXor => Operation::BitwiseXor
    );
    parse_rule!(
        name = bitwise_and, parent = equality,
        Operator::BitwiseAnd => Operation::BitwiseAnd
    );
    parse_rule!(
        name = equality, parent = relational,
        Operator::Equality => Operation::Equal,
        Operator::Inequality => Operation::Inequal
    );
    parse_rule!(
        name = relational, parent = shifting,
        Operator::Less => Operation::Less,
        Operator::Greater => Operation::Greater,
        Operator::LessOrEqual => Operation::LessOrEqual,
        Operator::GreaterOrEqual => Operation::GreaterOrEqual
    );
    parse_rule!(
        name = shifting, parent = additive,
        Operator::ShiftLeft => Operation::LeftShift,
        Operator::ShiftRight => Operation::RightShift

    );
    parse_rule!(
        name = additive, parent = multiplicative,
        Operator::Addition => Operation::Add,
        Operator::Subtraction => Operation::Subtract
    );
    parse_rule!(
        name = multiplicative, parent = cast,
        Operator::Multiplication => Operation::Multiply,
        Operator::Division => Operation::Divide,
        Operator::Remainder => Operation::Remainder
    );

    fn cast(&mut self) -> Result<Expression> {
        let mut node = self.post_group()?;

        loop {
            match self.current {
                Token::Operator(Operator::Casting) => {
                    self.advance()?;
                    node = Expression::BinaryOperation(Operation::Cast, Box::new(node), Box::new(Expression::Term(Symbol::Type(self.get_type()?))));
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn post_group(&mut self) -> Result<Expression> {
        let mut node = self.term()?;

        loop {
            match self.current {
                Token::Operator(Operator::OpenParenthesis) => {
                    self.advance()?;
                    let mut params = Vec::new();

                    if self.current != Token::Operator(Operator::CloseParenthesis) {
                        params.push(self.expression()?);
                    }

                    loop {
                        if let Token::Operator(Operator::CloseParenthesis) = self.current {
                            break;
                        }

                        expect!(self, Token::Operator(Operator::Comma));
                        params.push(self.expression()?);
                    }

                    expect!(self, Token::Operator(Operator::CloseParenthesis));
                    node = Expression::Call(Box::new(node), params);
                }
                Token::Operator(Operator::OpenBracket) => {
                    self.advance()?;
                    node = Expression::Index(Box::new(node), Box::new(self.expression()?));
                    expect!(self, Token::Operator(Operator::CloseBracket));
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn get_type(&mut self) -> Result<Type> {
        if let Token::Type(t) = &self.current {
            let res = t.clone();
            self.advance()?;
            if mem::discriminant(&res) == mem::discriminant(&Type::Array(Box::new(Type::Int))) {
                expect!(self, Token::Operator(Operator::OpenBracket));
                let res = self.get_type()?;
                expect!(self, Token::Operator(Operator::CloseBracket));
                Ok(Type::Array(Box::new(res)))
            }
            else {
                Ok(res)
            }
        }
        else {
            err!("expected a type")
        }
    }

    fn term(&mut self) -> Result<Expression> {
        match self.advance()? {
            Token::IntegerLiteral(i) => Ok(Expression::Term(Symbol::Int(i))),
            Token::FloatLiteral(f) => Ok(Expression::Term(Symbol::Float(f))),
            Token::StrLiteral(s) => Ok(Expression::Term(Symbol::Str(s))),
            Token::StringLiteral(s) => Ok(Expression::Term(Symbol::String(s))),
            Token::Identifier(s) => Ok(Expression::Term(Symbol::Identifier(s))),
            Token::Operator(Operator::Subtraction) => Ok(Expression::UnaryOperation(Operation::Negate, Box::new(self.term()?))),
            Token::Operator(Operator::LogicalNot) => Ok(Expression::UnaryOperation(Operation::LogicalNot, Box::new(self.term()?))),
            Token::Operator(Operator::BitwiseNot) => Ok(Expression::UnaryOperation(Operation::BitwiseNot, Box::new(self.term()?))),
            Token::Operator(Operator::Length) => Ok(Expression::UnaryOperation(Operation::Length, Box::new(self.term()?))),
            Token::Operator(Operator::Pop) => Ok(Expression::UnaryOperation(Operation::Pop, Box::new(self.term()?))),
            Token::Operator(Operator::OpenParenthesis) => {
                let expr = self.expression()?;
                expect!(self, Token::Operator(Operator::CloseParenthesis));
                Ok(expr)
            }
            Token::Operator(Operator::OpenBrace) => {
                let mut items = Vec::new();

                if self.current != Token::Operator(Operator::CloseBrace) {
                    items.push(self.expression()?);
                }

                loop {
                    if let Token::Operator(Operator::CloseBrace) = self.current {
                        break;
                    }

                    expect!(self, Token::Operator(Operator::Comma));
                    items.push(self.expression()?);
                }

                expect!(self, Token::Operator(Operator::CloseBrace));
                Ok(Expression::Term(Symbol::Array(items)))
            }
            t => err!("invalid token: {:?}", t),
        }
    }

    fn advance(&mut self) -> Result<Token> {
        Ok(mem::replace(&mut self.current, self.lexer.next()?))
    }
}
