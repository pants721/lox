use std::{any::{Any, type_name_of_val}, fmt};

use anyhow::{anyhow, Context, Result};
use thiserror::Error;

use crate::{lox_error_str, parser::{Expr, Visitor}, scanner::{Literal, Token, TokenType}};

#[derive(Debug, Error)]
pub enum InterpreterError {
    TypeError {
        line: usize,
        msg: String,
    },
    ConversionError {
        line: usize,
        part: String,
        expr_type: String,
    }
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeError { line, msg } => f.write_str(&lox_error_str!(line, "{}", msg)),
            Self::ConversionError { line, part, expr_type } => f.write_str(&lox_error_str!(line, "Failed to convert {} of {}", part, expr_type)),
        }
    }
}

#[derive(Default)]
pub struct Interpreter {
    has_errored: bool,
}

impl Visitor<Result<Box<dyn Any>, InterpreterError>> for Interpreter {
    fn visit_expr(&mut self, e: &Expr) -> Result<Box<dyn Any>, InterpreterError> {
        match e {
            Expr::Literal { val } => {
                if let Some(t) = val {
                    match t.literal.clone() {
                        Some(Literal::String(s)) => return Ok(Box::new(s)),
                        Some(Literal::Number(n)) => return Ok(Box::new(n)),
                        None => {
                            match t.t_type {
                                TokenType::True => return Ok(Box::new(true)),
                                TokenType::False => return Ok(Box::new(false)),
                                TokenType::Nil => return Ok(Box::new(None::<()>)),
                                _ => unreachable!(),
                            }
                        }
                    };
                };
                unreachable!();
            },
            Expr::Grouping { lhs: _, expr, rhs: _ } => self.visit_expr(expr),
            Expr::Unary { lhs, expr } => {
                let rhs = self.visit_expr(expr)?;

                match lhs.t_type {
                    TokenType::Minus => {
                        if let Some(rhs) = rhs.downcast_ref() {
                            Ok(Box::new(-1.0 * rhs))
                        } else {
                            let err = InterpreterError::TypeError { line: 1, msg: "Operand must be a number".to_string() };
                            self.has_errored = true;
                            Err(err)
                        }
                    },
                    TokenType::Bang => {
                        Ok(Box::new(!is_truthy(rhs)))
                    },
                    _ => unreachable!("Unary expression lhs is token other than minus or bang"),
                }
            },
            Expr::Binary { lhs, op, rhs } => {
                let lhs = self.visit_expr(lhs)?;
                let rhs = self.visit_expr(rhs)?;

                // two numbers
                if lhs.is::<f64>() && rhs.is::<f64>() {
                    let lhs = *match lhs.downcast_ref::<f64>() {
                        Some(inner) => inner,
                        None => return Err(InterpreterError::ConversionError { line: 1, part: "lhs".to_string(), expr_type: "binary".to_string() }),
                    };
                    let rhs = *match rhs.downcast_ref::<f64>() {
                        Some(inner) => inner,
                        None => return Err(InterpreterError::ConversionError { line: 1, part: "rhs".to_string(), expr_type: "binary".to_string() }),
                    };

                    return match op.t_type {
                        TokenType::EqualEqual => Ok(Box::new(lhs == rhs)),
                        TokenType::BangEqual => Ok(Box::new(lhs != rhs)),
                        TokenType::Greater => Ok(Box::new(lhs > rhs)),
                        TokenType::GreaterEqual => Ok(Box::new(lhs >= rhs)),
                        TokenType::Less => Ok(Box::new(lhs < rhs)),
                        TokenType::LessEqual => Ok(Box::new(lhs <= rhs)),
                        TokenType::Minus => Ok(Box::new(lhs - rhs)),
                        TokenType::Slash => Ok(Box::new(lhs / rhs)),
                        TokenType::Star => Ok(Box::new(lhs * rhs)),
                        TokenType::Plus => Ok(Box::new(lhs + rhs)),
                        _ => Err(InterpreterError::TypeError { line: 1, msg: "Invalid operation on two numbers".to_string() })
                    };
                }

                // two strings
                if lhs.is::<String>() && rhs.is::<String>() {
                    let lhs = match lhs.downcast_ref::<String>() {
                        Some(inner) => inner.clone(),
                        None => return Err(InterpreterError::ConversionError { line: 1, part: "lhs".to_string(), expr_type: "binary".to_string() }),
                    };
                    let rhs = match rhs.downcast_ref::<String>() {
                        Some(inner) => inner.clone(),
                        None => return Err(InterpreterError::ConversionError { line: 1, part: "rhs".to_string(), expr_type: "binary".to_string() }),
                    };

                    return match op.t_type {
                        TokenType::EqualEqual => Ok(Box::new(lhs == rhs)),
                        TokenType::BangEqual => Ok(Box::new(lhs != rhs)),
                        TokenType::Plus => Ok(Box::new(lhs + &rhs)),
                        _ => Err(InterpreterError::TypeError { line: 1, msg: "Invalid operation on two strings".to_string() })
                    };
                }

                // two bools
                if lhs.is::<bool>() && rhs.is::<bool>() {
                    let lhs = *match lhs.downcast_ref::<bool>() {
                        Some(inner) => inner,
                        None => return Err(InterpreterError::ConversionError { line: 1, part: "lhs".to_string(), expr_type: "binary".to_string() }),
                    };
                    let rhs = *match rhs.downcast_ref::<bool>() {
                        Some(inner) => inner,
                        None => return Err(InterpreterError::ConversionError { line: 1, part: "rhs".to_string(), expr_type: "binary".to_string() }),
                    };

                    return match op.t_type {
                        TokenType::EqualEqual => Ok(Box::new(lhs == rhs)),
                        TokenType::BangEqual => Ok(Box::new(lhs != rhs)),
                        _ => Err(InterpreterError::TypeError { line: 1, msg: "Invalid operation on two bools".to_string() })
                    };
                }

                // mismatch types
                return match op.t_type {
                    TokenType::EqualEqual => Ok(Box::new(false)),
                    TokenType::BangEqual => Ok(Box::new(true)),
                    _ => Err(InterpreterError::TypeError { line: 1, msg: "Mismatched types in binary expression".to_string() })
                };
            }
            _ => unreachable!()
        }
    }
}

impl Interpreter {
    pub fn expr_as_string(&mut self, e: &Expr) -> Result<String> {
        let a = self.visit_expr(e)?;

        if let Some(s) = a.downcast_ref::<String>() {
            return Ok(s.clone()); 
        }

        if let Some(s) = a.downcast_ref::<f64>() {
            return Ok(s.to_string()); 
        }

        if let Some(s) = a.downcast_ref::<bool>() {
            return Ok(s.to_string()); 
        }

        // nil
        if let Some(s) = a.downcast_ref::<Option<()>>() {
            if s.is_none() {
                return Ok("nil".to_string());
            }
        }

        Err(anyhow!("Could not convert expression to string"))
    }
}

pub fn is_truthy(obj: Box<dyn Any>) -> bool {
    if let Some(b) = obj.downcast_ref::<bool>() {
        return *b;
    }
     
    if let Some(o) = obj.downcast_ref::<Option<()>>() {
        if o.is_none() {
            return false;
        }
    }

    true
}
