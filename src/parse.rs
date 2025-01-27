use crate::lex::{Lexer, Token, TokenType};
use miette::Error;
use std::borrow::Cow;
use thiserror::Error;

pub struct Parser<'a> {
    lexer: std::iter::Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let lexer = Lexer::new(input).peekable();
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<Ast, Error> {
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> Result<Ast, Error> {
        let tkn = match self.lexer.next() {
            Some(Ok(tkn)) => tkn,
            Some(Err(_)) => todo!(),
            None => return Ok(Ast::Atom(Atom::Null)),
        };
        todo!()
    }
}

pub enum Ast<'a> {
    Atom(Atom<'a>),
    Cons(Op, Vec<Ast<'a>>),
}

pub enum Atom<'a> {
    String(Cow<'a, str>),
    Number(f64),
    Ident(&'a str),
    Null,
    Map,
    Array,
    StringTy,
    NumberTy,
    Bool(bool),
    Colon,
}

pub enum Op {
    Bang,
    Equal,
    Greater,
    Less,
    GEqual,
    LEqual,
    BangEqual,
    Equality,
    And,
    Or,
    Fn,
    Struct,
    If,
    Else,
    Return,
}

impl<'a> std::fmt::Display for Ast<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ast::Atom(atom) => match atom {
                Atom::String(s) => write!(f, "{s}"),
                Atom::Number(n) => write!(f, "{n}"),
                Atom::Ident(i) => write!(f, "{i}"),
                Atom::Null => write!(f, "null"),
                Atom::Map => write!(f, "map"),
                Atom::Array => write!(f, "array"),
                Atom::StringTy => write!(f, "string"),
                Atom::NumberTy => write!(f, "number"),
                Atom::Bool(b) => write!(f, "{b}"),
                Atom::Colon => write!(f, ":"),
            },
            Ast::Cons(op, items) => {
                write!(f, "({op}")?;
                for i in items {
                    write!(f, " {i}")?
                }
                write!(f, ")")
            }
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Bang => write!(f, "!"),
            Op::Equal => write!(f, "="),
            Op::Greater => write!(f, ">"),
            Op::Less => write!(f, "<"),
            Op::GEqual => write!(f, ">="),
            Op::LEqual => write!(f, "<="),
            Op::BangEqual => write!(f, "!="),
            Op::Equality => write!(f, "=="),
            Op::And => write!(f, "and"),
            Op::Or => write!(f, "or"),
            Op::Fn => write!(f, "fn"),
            Op::Struct => write!(f, "struct"),
            Op::If => write!(f, "if"),
            Op::Else => write!(f, "else"),
            Op::Return => write!(f, "return"),
        }
    }
}
