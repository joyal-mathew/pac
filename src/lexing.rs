use crate::utils::Result;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Keyword {
    Var,

    If,
    Then,
    Else,
    Elif,

    While,
    Break,
    Continue,
    Do,

    Function,
    Return,

    End,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Remainder,
    Equality,
    Inequality,
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    Assignment,

    ShiftRight,
    ShiftLeft,

    BitwiseNot,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,

    Casting,

    OpenParenthesis,
    CloseParenthesis,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,

    Semicolon,
    Comma,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Float,
    Str,
    String,
    Array(Box<Type>),

    Void,
    ArrayInfer,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    IntegerLiteral(u64),
    FloatLiteral(f64),
    StrLiteral(u64),
    StringLiteral(String),

    Operator(Operator),
    Identifier(String),
    Keyword(Keyword),
    Type(Type),

    EndOfFile,
}

pub struct Lexer {
    chars: Vec<char>,
    current: char,

    operators: HashMap<&'static str, Operator>,
    keywords: HashMap<&'static str, Keyword>,
    types: HashMap<&'static str, Type>,
    escapes: HashMap<char, char>,
}

impl Lexer {
    const OPERATORS: &'static str = ",^~+-*/%()=!;<>&|[]{}";

    pub fn new(source: String) -> Self {
        let mut operators = HashMap::new();
        let mut keywords = HashMap::new();
        let mut types = HashMap::new();
        let mut escapes = HashMap::new();

        operators.insert("+", Operator::Addition);
        operators.insert("-", Operator::Subtraction);
        operators.insert("*", Operator::Multiplication);
        operators.insert("/", Operator::Division);
        operators.insert("%", Operator::Remainder);
        operators.insert("==", Operator::Equality);
        operators.insert("!=", Operator::Inequality);
        operators.insert("<", Operator::Less);
        operators.insert(">", Operator::Greater);
        operators.insert("<=", Operator::LessOrEqual);
        operators.insert(">=", Operator::GreaterOrEqual);
        operators.insert("=", Operator::Assignment);
        operators.insert("~>", Operator::Casting);
        operators.insert("!", Operator::LogicalNot);
        operators.insert("&&", Operator::LogicalAnd);
        operators.insert("||", Operator::LogicalOr);
        operators.insert(">>", Operator::ShiftRight);
        operators.insert("<<", Operator::ShiftLeft);
        operators.insert("~", Operator::BitwiseNot);
        operators.insert("&", Operator::BitwiseAnd);
        operators.insert("|", Operator::BitwiseOr);
        operators.insert("^", Operator::BitwiseXor);
        operators.insert("(", Operator::OpenParenthesis);
        operators.insert(")", Operator::CloseParenthesis);
        operators.insert("[", Operator::OpenBracket);
        operators.insert("]", Operator::CloseBracket);
        operators.insert("{", Operator::OpenBrace);
        operators.insert("}", Operator::CloseBrace);
        operators.insert(";", Operator::Semicolon);
        operators.insert(",", Operator::Comma);

        keywords.insert("var", Keyword::Var);
        keywords.insert("if", Keyword::If);
        keywords.insert("then", Keyword::Then);
        keywords.insert("else", Keyword::Else);
        keywords.insert("elif", Keyword::Elif);
        keywords.insert("while", Keyword::While);
        keywords.insert("break", Keyword::Break);
        keywords.insert("continue", Keyword::Continue);
        keywords.insert("do", Keyword::Do);
        keywords.insert("function", Keyword::Function);
        keywords.insert("return", Keyword::Return);
        keywords.insert("end", Keyword::End);

        types.insert("float", Type::Float);
        types.insert("int", Type::Int);
        types.insert("str", Type::Str);
        types.insert("string", Type::String);
        types.insert("array", Type::Array(Box::new(Type::Int)));

        escapes.insert('n', '\n');
        escapes.insert('t', '\t');
        escapes.insert('\'', '\'');
        escapes.insert('\\', '\\');

        Self {
            chars: source.chars().rev().collect(),
            current: ' ',

            operators,
            keywords,
            types,
            escapes,
        }
    }

    pub fn next(&mut self) -> Result<Token> {
        while self.current.is_ascii_whitespace() {
            self.advance();
        }

        match self.current {
            c if c.is_ascii_digit() || c == '.' => self.numeric_literal(),
            c if c.is_ascii_alphabetic() || c == '_' => self.alphabetic(),
            c if Self::OPERATORS.contains(c) => self.operator(),
            '\'' => self.str(),
            '"' => self.string(),
            '\0' => Ok(Token::EndOfFile),
            _ => err!("invalid character: {}", self.current),
        }
    }

    fn numeric_literal(&mut self) -> Result<Token> {
        let mut number = String::new();

        while self.current.is_ascii_digit() || self.current == '.' {
            number.push(self.current);
            self.advance();
        }

        if let Ok(i) = number.parse() {
            Ok(Token::IntegerLiteral(i))
        }
        else if let Ok(f) = number.parse() {
            Ok(Token::FloatLiteral(f))
        }
        else {
            err!("invalid numeric literal: {}", number)
        }
    }

    fn alphabetic(&mut self) -> Result<Token> {
        let mut alpha = String::new();

        while self.current.is_ascii_alphanumeric() || self.current == '_' {
            alpha.push(self.current);
            self.advance();
        }

        Ok(if let Some(&k) = self.keywords.get(&alpha[..]) {
            Token::Keyword(k)
        }
        else if let Some(t) = self.types.get(&alpha[..]) {
            Token::Type(t.clone())
        }
        else {
            Token::Identifier(alpha)
        })
    }

    fn operator(&mut self) -> Result<Token> {
        let op = vec![self.current, self.advance()];
        let op1: &str = &op.iter().collect::<String>();
        let op2: &str = &op[..1].iter().collect::<String>();

        if let Some(&o) = self.operators.get(&op1) {
            self.advance();
            Ok(Token::Operator(o))
        }
        else if let Some(&o) = self.operators.get(&op2) {
            Ok(Token::Operator(o))
        }
        else {
            err!("invalid operator: {}", op2)
        }
    }

    fn string(&mut self) -> Result<Token> {
        let mut data = String::new();
        let mut esc = false;

        self.advance();

        loop {
            if esc {
                data.push(*self.escapes.get(&self.current).ok_or("invalid escape code".to_string())?);
                esc = false;
            }
            else {
                match self.current {
                    '"' => {
                        self.advance();
                        return Ok(Token::StringLiteral(data));
                    }
                    '\\' => {
                        esc = true;
                    }
                    '\0' => {
                        return err!("eof in string literal");
                    }
                    c => {
                        data.push(c);
                    }
                }
            }

            self.advance();
        }
    }

    fn str(&mut self) -> Result<Token> {
        let mut data = 0;
        let mut ptr = 56;

        let mut esc = false;

        self.advance();

        loop {
            if esc {
                let escape = self.escapes.get(&self.current).ok_or("invalid escape code".to_string())?;
                data |= (*escape as u8 as u64) << ptr;
                ptr -= 8;
                esc = false;
            }
            else {
                match self.current {
                    '\'' => {
                        self.advance();
                        return Ok(Token::StrLiteral(data));
                    }
                    '\\' => {
                        if ptr < 0 {
                            return err!("overflowing str");
                        }

                        esc = true;
                    }
                    '\0' => {
                        return err!("eof in str literal")
                    }
                    c => {
                        if ptr < 0 {
                            return err!("overflowing str");
                        }

                        data |= (c as u8 as u64) << ptr;
                        ptr -= 8;
                    }
                }
            }

            self.advance();
        }
    }

    fn advance(&mut self) -> char {
        self.current = if let Some(c) = self.chars.pop() {
            c
        }
        else {
            '\0'
        };

        self.current
    }
}
