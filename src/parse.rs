use crate::lex::{Lexer, Token, TokenType};
use miette::{Error, WrapErr};
use std::borrow::Cow;
use thiserror::Error;

pub struct Parser<'a> {
    src: &'a str,
    lexer: std::iter::Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let lexer = Lexer::new(input).peekable();
        Self { src: input, lexer }
    }

    pub fn parse(&mut self) -> Result<Ast, Error> {
        self.parse_expression(0)
    }

    fn parse_expression(&mut self, min_bp: u8) -> Result<Ast<'a>, Error> {
        let tkn = match self.lexer.next() {
            Some(Ok(tkn)) => tkn,
            Some(Err(e)) => return Err(e).wrap_err("error on the lhs"),
            None => return Ok(Ast::Atom(Atom::Null)),
        };

        let mut lhs = match tkn {
            Token {
                ty: TokenType::String,
                value,
            } => Ast::Atom(Atom::String(Cow::Borrowed(value))),
            Token {
                ty: TokenType::Number(value),
                ..
            } => Ast::Atom(Atom::Number(value)),
            Token {
                ty: TokenType::Ident,
                value,
            } => Ast::Atom(Atom::Ident(value)),
            Token {
                ty: TokenType::Null,
                ..
            } => Ast::Atom(Atom::Null),
            Token {
                ty: TokenType::Map, ..
            } => Ast::Atom(Atom::Map),
            Token {
                ty: TokenType::Array,
                ..
            } => Ast::Atom(Atom::Array),
            Token {
                ty: TokenType::StringTy,
                ..
            } => Ast::Atom(Atom::StringTy),
            Token {
                ty: TokenType::NumberTy,
                ..
            } => Ast::Atom(Atom::NumberTy),
            Token {
                ty: TokenType::True,
                ..
            } => Ast::Atom(Atom::Bool(true)),
            Token {
                ty: TokenType::False,
                ..
            } => Ast::Atom(Atom::Bool(false)),
            Token {
                ty: TokenType::Colon,
                ..
            } => Ast::Atom(Atom::Colon),
            Token {
                ty: TokenType::Semicolon,
                ..
            } => Ast::Atom(Atom::Semicolon),
            Token {
                ty: TokenType::Bang | TokenType::Minus,
                ..
            } => {
                let op = match tkn.ty {
                    TokenType::Bang => Op::Bang,
                    TokenType::Minus => Op::Minus,
                    _ => unreachable!("covered by outer match arm"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expression(r_bp).wrap_err("rhs error")?;

                Ast::Cons(op, vec![rhs])
            }
            _ => panic!("bad token: {tkn:?}"),
        };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                panic!("some error occurred");
            }

            let op = match op.map(|res| res.as_ref().expect("error captured above")) {
                None => break,
                Some(Token {
                    ty: TokenType::Plus,
                    ..
                }) => Op::Plus,
                Some(Token {
                    ty: TokenType::Minus,
                    ..
                }) => Op::Minus,
                Some(Token {
                    ty: TokenType::Star,
                    ..
                }) => Op::Star,
                Some(Token {
                    ty: TokenType::Slash,
                    ..
                }) => Op::Slash,
                Some(Token {
                    ty: TokenType::Dot, ..
                }) => Op::Dot,
                _ => todo!(),
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();

                lhs = Ast::Cons(op, vec![lhs]);
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();
                let rhs = self.parse_expression(r_bp)?;

                lhs = Ast::Cons(op, vec![lhs, rhs])
            }
        }

        Ok(lhs)
    }
}

fn infix_binding_power(op: Op) -> Option<(u8, u8)> {
    let res = match op {
        Op::Plus | Op::Minus => (1, 2),
        Op::Star | Op::Slash => (3, 4),
        Op::Dot => (10, 9),
        _ => return None,
    };

    Some(res)
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Bang | Op::Minus | Op::Plus => ((), 5),
        _ => panic!("bad prefix op: {op}"),
    }
}

fn postfix_binding_power(op: Op) -> Option<(u8, ())> {
    let res = match op {
        Op::Bang => (7, ()),
        _ => return None,
    };

    Some(res)
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Ast<'a> {
    Atom(Atom<'a>),
    Cons(Op, Vec<Ast<'a>>),
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
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
    Semicolon,
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
    Dot,
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
                Atom::Semicolon => write!(f, ";"),
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
            Op::Plus => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Star => write!(f, "*"),
            Op::Slash => write!(f, "/"),
            Op::Dot => write!(f, "."),
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

#[cfg(test)]
mod parse_tests {
    use super::*;

    #[test]
    fn one() -> Result<(), Error> {
        let mut p = Parser::new("1");
        let result = p.parse()?;
        assert_eq!(result.to_string(), "1");

        Ok(())
    }

    #[test]
    fn two() -> Result<(), Error> {
        let mut p = Parser::new("1 + 2 * 3");
        let result = p.parse()?;
        assert_eq!(result.to_string(), "(+ 1 (* 2 3))");

        Ok(())
    }

    #[test]
    fn three() -> Result<(), Error> {
        let mut p = Parser::new("f . g . h");
        let result = p.parse()?;
        assert_eq!(result.to_string(), "(. f (. g h))");

        Ok(())
    }

    #[test]
    fn four() -> Result<(), Error> {
        let mut p = Parser::new("1 + 2 + f . g . h * 3 * 4");
        let result = p.parse()?;
        assert_eq!(result.to_string(), "(+ (+ 1 2) (* (* (. f (. g h)) 3) 4))");

        Ok(())
    }

    #[test]
    fn five() -> Result<(), Error> {
        let mut p = Parser::new("--1 * 2");
        let result = p.parse()?;
        assert_eq!(result.to_string(), "(* (- (- 1)) 2)");

        Ok(())
    }

    #[test]
    fn six() -> Result<(), Error> {
        let mut p = Parser::new("--f . g");
        let result = p.parse()?;
        assert_eq!(result.to_string(), "(- (- (. f g)))");

        Ok(())
    }

    fn seven() -> Result<(), Error> {
        let mut p = Parser::new("-9!");
        let result = p.parse()?;
        assert_eq!(result.to_string(), "(- (! 9))");

        Ok(())
    }

    fn eight() -> Result<(), Error> {
        let mut p = Parser::new("f . g!");
        let result = p.parse()?;
        assert_eq!(result.to_string(), "(! (. f g))");

        Ok(())
    }
}
