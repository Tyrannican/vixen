use std::{iter::Peekable, str::Chars};

use miette::{Diagnostic, Error, LabeledSpan, SourceSpan};
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
#[error("Invalid token '{token}'")]
pub struct TokenError {
    #[source_code]
    src: String,

    token: char,

    #[label("This token")]
    err_span: SourceSpan,
}

#[derive(Debug, Diagnostic, Error)]
#[error("Unterminated string literal")]
pub struct StringTerminationError {
    #[source_code]
    src: String,

    #[label("Starting here")]
    err_span: SourceSpan,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Plus,
    Minus,
    Star,
    Dot,
    Comma,
    Semicolon,
    Colon,
    Slash,
    Bang,
    Equal,
    Greater,
    Less,
    GEqual,
    LEqual,
    BangEqual,
    Equality,
    Backslash,
    Number(f64),
    String,
    Ident,
    And,
    Or,
    Fn,
    Struct,
    If,
    Else,
    True,
    False,
    Return,
    StringTy,
    NumberTy,
    Map,
    Array,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Token<'a> {
    ty: TokenType,
    value: &'a str,
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

enum Lead {
    IfNextElse(char, TokenType, TokenType),
    String,
    Number,
    Ident,
    Slash,
}

pub struct Lexer<'a> {
    input: &'a str,
    byte: usize,
    iter: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            byte: 0,
            iter: input.chars().peekable(),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(c) = self.iter.next() {
            let c_len = c.len_utf8();
            let tok = &self.input[self.byte..self.byte + c_len];
            let single_token = move |ty: TokenType| {
                return Some(Ok(Token { ty, value: tok }));
            };
            self.byte += c.len_utf8();

            let started = match c {
                '(' => return single_token(TokenType::LParen),
                ')' => return single_token(TokenType::RParen),
                '[' => return single_token(TokenType::LBracket),
                ']' => return single_token(TokenType::RBracket),
                '{' => return single_token(TokenType::LBrace),
                '}' => return single_token(TokenType::RBrace),
                '.' => return single_token(TokenType::Dot),
                ';' => return single_token(TokenType::Semicolon),
                ':' => return single_token(TokenType::Colon),
                '+' => return single_token(TokenType::Plus),
                '-' => return single_token(TokenType::Minus),
                '*' => return single_token(TokenType::Star),
                ',' => return single_token(TokenType::Comma),
                '\\' => return single_token(TokenType::Backslash), // TODO: Deal with escaped chars
                '>' => Lead::IfNextElse('=', TokenType::GEqual, TokenType::Greater),
                '<' => Lead::IfNextElse('=', TokenType::LEqual, TokenType::Less),
                '!' => Lead::IfNextElse('=', TokenType::BangEqual, TokenType::Bang),
                '=' => Lead::IfNextElse('=', TokenType::Equality, TokenType::Equal),
                '"' => Lead::String,
                '/' => Lead::Slash,
                '0'..='9' => Lead::Number,
                'a'..='z' | 'A'..='Z' | '_' => Lead::Ident,
                c if c.is_whitespace() => continue,
                _ => {
                    return Some(Err(TokenError {
                        src: self.input.to_string(),
                        token: c,
                        err_span: (self.byte - c.len_utf8()..self.byte).into(),
                    }
                    .into()));
                }
            };

            match started {
                Lead::String => {
                    let start = self.byte - c_len;
                    let mut end = self.byte;
                    while let Some(t) = self.iter.next() {
                        end += t.len_utf8();
                        if t == '\\' && self.iter.peek() == Some(&'"') {
                            end += 1;
                            self.iter.next();
                            continue;
                        } else if t == '"' {
                            let literal = &self.input[start..end];
                            self.byte = end;
                            return Some(Ok(Token {
                                ty: TokenType::String,
                                value: literal,
                            }));
                        }
                    }

                    return Some(Err(StringTerminationError {
                        src: self.input.to_string(),
                        err_span: SourceSpan::from(self.byte..self.input.len()),
                    }
                    .into()));
                }
                Lead::Number => {
                    let start = self.byte - c_len;
                    let mut end = self.byte;

                    let mut is_frac = false;
                    while let Some(d) = self.iter.peek() {
                        let d_len = d.len_utf8();
                        match d {
                            '0'..='9' => {
                                end += d_len;
                                self.iter.next();
                            }
                            '.' if is_frac => break,
                            '.' => {
                                is_frac = true;
                                end += d_len;
                                self.iter.next();
                            }
                            _ => break,
                        }
                    }

                    let literal = &self.input[start..end];
                    let n = match literal.parse() {
                        Ok(n) => n,
                        Err(e) => return Some(Err(miette::miette! {
                            labels = vec![
                                LabeledSpan::at(self.byte-literal.len()..self.byte, "this numeric"),
                            ],
                            "{e}",
                        }
                        .with_source_code(self.input.to_string()))),
                    };

                    self.byte = end;
                    return Some(Ok(Token {
                        ty: TokenType::Number(n),
                        value: literal,
                    }));
                }
                Lead::Ident => {
                    let start = self.byte - c_len;
                    let mut end = self.byte;

                    while let Some(next) = self.iter.peek() {
                        if !matches!(next, 'a'..='z' | 'A'..='Z' | '_') {
                            break;
                        }

                        end += next.len_utf8();
                        self.iter.next();
                    }

                    let literal = &self.input[start..end];

                    let ty = match literal {
                        "and" => TokenType::And,
                        "or" => TokenType::Or,
                        "fn" => TokenType::Fn,
                        "struct" => TokenType::Struct,
                        "if" => TokenType::If,
                        "else" => TokenType::Else,
                        "true" => TokenType::True,
                        "false" => TokenType::False,
                        "return" => TokenType::Return,
                        "map" => TokenType::Map,
                        "array" => TokenType::Array,
                        "string" => TokenType::StringTy,
                        "number" => TokenType::NumberTy,
                        _ => TokenType::Ident,
                    };

                    self.byte = end;
                    return Some(Ok(Token { ty, value: literal }));
                }
                Lead::Slash => {
                    if self.iter.peek() == Some(&'/') {
                        while let Some(n) = self.iter.next() {
                            self.byte += n.len_utf8();
                            if n == '\n' {
                                break;
                            }
                        }
                    } else {
                        return Some(Ok(Token {
                            ty: TokenType::Slash,
                            value: &self.input[self.byte - c_len..self.byte],
                        }));
                    }
                }
                Lead::IfNextElse(ch, yes, no) => {
                    if let Some(next) = self.iter.peek() {
                        if *next != ch {
                            let lit = &self.input[self.byte - c_len..self.byte];
                            return Some(Ok(Token { ty: no, value: lit }));
                        }

                        let lit = &self.input[self.byte - c_len..self.byte + next.len_utf8()];
                        self.byte += next.len_utf8();
                        self.iter.next();

                        return Some(Ok(Token {
                            ty: yes,
                            value: lit,
                        }));
                    }
                }
            }
        }

        None
    }
}
