use std::fmt::{self, Write};

use anyhow::{anyhow, Result};
use thiserror::Error;

use crate::{lox_error_str, lox_token_error_str, scanner::{Scanner, Token, TokenType}};
use crate::{binary_expr, unary_expr, grouping_expr, literal_expr};

#[derive(Debug)]
#[derive(Error)]
pub enum ParserError {
    UnmatchedDelimiter {
        line: usize,
        token: Token,
        expected: char,
    },
    NoExpression {
        line: usize,
        token: Token,
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnmatchedDelimiter { line, token, expected } => {
                f.write_str(&lox_error_str!(line, "Unmatched delimiter: Expected {}", expected))
            },
            Self::NoExpression { line, token } => {
                f.write_str(&lox_token_error_str!(line, token.lexeme, "Expected expression"))
            }
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Literal {
        val: Option<Token>,
    },
    Grouping {
        lhs: Token,
        expr: Box<Expr>,
        rhs: Token,
    },
    Binary {
        lhs: Box<Expr>,
        op: Token,
        rhs: Box<Expr>,
    },
    Unary {
        lhs: Token,
        expr: Box<Expr>,
    },
}

pub trait Visitor<T> {
    fn visit(&mut self, e: &Expr) -> T;
}

pub struct AstPrinter;

impl AstPrinter {
    fn parenthesize(&mut self, name: &String, exprs: Vec<&Expr>) -> String {
        let mut s = String::new();
        s += "(";
        s += name;

        for expr in exprs {
            s += " ";
            s += &self.visit(expr);
        }
        s += ")";
        s
    }
}

impl Visitor<String> for AstPrinter {
    fn visit(&mut self, e: &Expr) -> String {
        match e {
            Expr::Literal { val } => match val {
                Some(t) => match &t.literal {
                    Some(l) => l.to_string(),
                    None => t.lexeme.clone(),
                },
                None => "nil".to_string(),
            },
            Expr::Grouping { lhs, ref expr, rhs } => self.parenthesize(&"group".to_string(), vec![&expr]),
            Expr::Binary { ref lhs, op, ref rhs } => self.parenthesize(&op.lexeme, vec![&lhs, &rhs]),
            Expr::Unary { lhs, ref expr } => self.parenthesize(&lhs.lexeme, vec![&expr]),
        }
    }
}

#[derive(Default)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    pub has_errored: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            ..Default::default()
        }
    }

    pub fn parse(&mut self) -> Result<Expr, ParserError> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        
        while self.match_type(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous();
            let rhs = self.comparison()?;
            expr = Expr::Binary { 
                lhs: Box::new(expr), 
                op: op.clone(), 
                rhs: Box::new(rhs) 
            };
        }
        Ok(expr)
    } 

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;

        while self.match_type(vec![TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let op = self.previous();
            let rhs = self.term()?;
            expr = Expr::Binary { lhs: Box::new(expr), op: op.clone(), rhs: Box::new(rhs) }
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;

        while self.match_type(vec![TokenType::Minus, TokenType::Plus]) {
            let op = self.previous();
            let rhs = self.factor()?;
            expr = Expr::Binary { lhs: Box::new(expr), op: op.clone(), rhs: Box::new(rhs) }
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;

        while self.match_type(vec![TokenType::Slash, TokenType::Star]) {
            let op = self.previous();
            let rhs = self.unary()?;
            expr = Expr::Binary { lhs: Box::new(expr), op: op.clone(), rhs: Box::new(rhs) }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {

        if self.match_type(vec![TokenType::Bang, TokenType::Minus]) {
            let op = self.previous();
            let rhs = self.unary()?;
            return Ok(Expr::Unary { lhs: op.clone(), expr: Box::new(rhs) });
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if self.match_type(vec![TokenType::True]) {
            return Ok(Expr::Literal { val: Some(Token::new("true".to_string(), TokenType::True, 1, None)) });
        }
        if self.match_type(vec![TokenType::False]) {
            return Ok(Expr::Literal { val: Some(Token::new("false".to_string(), TokenType::False, 1, None)) });
        }
        if self.match_type(vec![TokenType::Nil]) {
            return Ok(Expr::Literal { val: Some(Token::new("nil".to_string(), TokenType::Nil, 1, None)) });
        }

        if self.match_type(vec![TokenType::Number, TokenType::String]) {
            return Ok(Expr::Literal { val: Some(self.previous()) });
        }

        if self.match_type(vec![TokenType::LeftParen]) {
            let expr = self.expression()?;
            // TODO: report line
            match self.consume(TokenType::RightParen) {
                Ok(_) => (),
                Err(_) => {
                    let tok = self.peek();
                    let e = ParserError::UnmatchedDelimiter { line: 1, token: tok, expected: ')' };
                    self.has_errored = true;
                    return Err(e);
                },
            }
            return Ok(grouping_expr!("(", expr, ")"));
        }

        let tok = self.peek();
        let e = ParserError::NoExpression { line: 1, token: tok };
        self.has_errored = true;
        Err(e)
    }

    fn match_type(&mut self, types: Vec<TokenType>) -> bool {
        for t in types {
            if self.check(t) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn consume(&mut self, t: TokenType) -> Result<Token> {
        if self.check(t) {
            return Ok(self.advance());
        }

        Err(anyhow!("Failed to consume token"))
    }

    fn check(&mut self, t: TokenType) -> bool {
        if self.is_at_end() { 
            return false; 
        }
        self.peek().t_type == t
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() { 
            self.current += 1; 
        }
        self.previous().clone()
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().t_type == TokenType::Eof
    }

    fn peek(&mut self) -> Token {
        self.tokens.get(self.current).expect("Parser peek failed").clone()
    }

    fn previous(&mut self) -> Token {
        self.tokens.get(self.current - 1).expect("Parser previous peek failed").clone()
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().t_type == TokenType::Semicolon { return; }

            match self.peek().t_type {
                TokenType::Class => (),
                TokenType::Fun => (),
                TokenType::Var => (),
                TokenType::For => (),
                TokenType::If => (),
                TokenType::While => (),
                TokenType::Print => (),
                TokenType::Return => return,
                _ => (),
            }

            self.advance();
        }
    }
}

#[macro_export]
macro_rules! binary_expr {
    ($lhs:expr, $op:expr, $rhs:expr) => {
        Expr::Binary {
        lhs: Box::new($lhs),
        op: Token::new($op.to_string(), TokenType::parse($op).expect(
        "Failed to parse token type of operator in binary expression"
        ), 1, None),
        rhs: Box::new($rhs),
        }
    };
}

#[macro_export]
macro_rules! unary_expr {
    ($lhs:expr, $expr:expr) => {
        Expr::Unary {
        lhs: Token::new($lhs.to_string(), TokenType::parse($lhs).expect("Failed to parse token type of rhs in unary expression"), 1, None),
        expr: Box::new($expr),
        }
    };
}

#[macro_export]
macro_rules! grouping_expr {
    ($lhs:expr, $expr:expr, $rhs:expr) => {
        Expr::Grouping {
        lhs: Token::new($lhs.to_string(), TokenType::parse($lhs).expect("Failed to parse token type of lhs in grouping expression"), 1, None),
        expr: Box::new($expr),
        rhs: Token::new($rhs.to_string(), TokenType::parse($rhs).expect("Failed to parse token type of rhs in grouping expression"), 1, None),
        } 
    };
}

#[macro_export]
/// FIXME: This isn't really comprehensive for bools, nil, etc.
macro_rules! literal_expr {
    () => {
        Expr::Literal {
        val: None,
        }
    };

    ($val:expr, Literal::String) => {
        Expr::Literal {
        val: Some(Token::new($val.to_string(), TokenType::String, 1, Some(Literal::String($val.to_string())))),
        }  
    };

    ($val:expr, Literal::Number) => {
        Expr::Literal {
        val: Some(Token::new($val.to_string(), TokenType::Number, 1, Some(Literal::Number($val)))),
        }  
    };
}
