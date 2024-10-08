use std::any::Any;

use anyhow::{anyhow, Context, Result};

use crate::{parser::{Expr, Visitor}, scanner::{Literal, Token, TokenType}};

pub struct Interpreter {

}

impl Visitor<Result<Box<dyn Any>>> for Interpreter {
    fn visit_expr(&mut self, e: &Expr) -> Result<Box<dyn Any>> {
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
                let rhs = self.visit_expr(expr).context("Failed to parse rhs of unary expression")?;

                match lhs.t_type {
                    TokenType::Minus => {
                        let rhs: f64 = *rhs.downcast_ref().context("Failed to downcast")?;
                        Ok(Box::new(-1.0 * rhs))
                    },
                    TokenType::Bang => {
                        Ok(Box::new(!is_truthy(rhs)))
                    },
                    _ => unreachable!("Unary expression lhs is token other than minus or bang"),
                }
            },
            Expr::Binary { lhs, op, rhs } => {
                let lhs = self.visit_expr(lhs).context("Failed to parse lhs of binary expression")?;
                let rhs = self.visit_expr(rhs).context("Failed to parse rhs of binary expression")?;

                match op.t_type {
                    TokenType::EqualEqual => {
                        if let Some(lhs) = lhs.downcast_ref::<f64>() && let Some(rhs) = rhs.downcast_ref::<f64>() {
                            return Ok(Box::new(lhs == rhs));
                        }

                        if let Some(lhs) = lhs.downcast_ref::<String>() && let Some(rhs) = rhs.downcast_ref::<String>() {
                            return Ok(Box::new(lhs == rhs));
                        }

                        if let Some(lhs) = lhs.downcast_ref::<bool>() && let Some(rhs) = rhs.downcast_ref::<bool>() {
                            return Ok(Box::new(lhs == rhs));
                        }


                        Err(anyhow!("Unable to assert equality between lhs and rhs of binary expression"))
                    },
                    TokenType::BangEqual => {
                        if let Some(lhs) = lhs.downcast_ref::<f64>() && let Some(rhs) = rhs.downcast_ref::<f64>() {
                            return Ok(Box::new(lhs != rhs));
                        }

                        if let Some(lhs) = lhs.downcast_ref::<String>() && let Some(rhs) = rhs.downcast_ref::<String>() {
                            return Ok(Box::new(lhs != rhs));
                        }

                        if let Some(lhs) = lhs.downcast_ref::<bool>() && let Some(rhs) = rhs.downcast_ref::<bool>() {
                            return Ok(Box::new(lhs != rhs));
                        }


                        Err(anyhow!("Unable to assert inequality between lhs and rhs of binary expression"))
                    },
                    TokenType::Greater => {
                        let lhs = *lhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        let rhs = *rhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        Ok(Box::new(lhs > rhs))
                    },
                    TokenType::GreaterEqual => {
                        let lhs = *lhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        let rhs = *rhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        Ok(Box::new(lhs >= rhs))
                    },
                    TokenType::Less => {
                        let lhs = *lhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        let rhs = *rhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        Ok(Box::new(lhs < rhs))
                    },
                    TokenType::LessEqual => {
                        let lhs = *lhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        let rhs = *rhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        Ok(Box::new(lhs <= rhs))
                    },
                    TokenType::Minus => {
                        let lhs = *lhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        let rhs = *rhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        Ok(Box::new(lhs - rhs))
                    },
                    TokenType::Slash => {
                        let lhs = *lhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        let rhs = *rhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        Ok(Box::new(lhs / rhs))
                    },
                    TokenType::Star => {
                        let lhs = *lhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        let rhs = *rhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                        Ok(Box::new(lhs * rhs))
                    },
                    TokenType::Plus => {
                        if lhs.is::<f64>() && rhs.is::<f64>() {
                            let lhs = *lhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                            let rhs = *rhs.downcast_ref::<f64>().expect("Failed to downcast f64");
                            return Ok(Box::new(lhs + rhs));
                        }

                        if lhs.is::<String>() && rhs.is::<String>() {
                            let lhs = lhs.downcast_ref::<String>().expect("Failed to downcast string").clone();
                            let rhs = rhs.downcast_ref::<String>().expect("Failed to downcast string").clone();
                            return Ok(Box::new(lhs + &rhs));
                        }

                        unreachable!()
                    },
                    _ => unreachable!("Binary expression has operator of unrecognized token"),
                }
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
