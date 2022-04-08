use std::{collections::HashMap, fmt};

use crate::{
    lexer::{Token, TokenKind},
    parser::{Expr, Stmt},
    Error,
};

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;

        match self {
            Nil => write!(f, "nil"),
            Number(n) if n.fract() == 0.0 => write!(f, "{}", *n as i64),
            Number(n) => write!(f, "{}", n),
            String(s) => write!(f, "\"{}\"", s),
            Boolean(true) => write!(f, "true"),
            Boolean(false) => write!(f, "false"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;

        match (self, other) {
            (Number(l), Number(r)) => l == r,
            (String(l), String(r)) => l == r,
            (Boolean(l), Boolean(r)) => l == r,
            (Nil, Nil) => true,
            _ => false,
        }
    }
}

struct Environment {
    vars: HashMap<String, Value>,
}

impl Environment {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    fn define(&mut self, name: &str, value: Value) {
        self.vars.insert(name.to_string(), value);
    }

    fn assign(&mut self, name: &str, value: Value) -> Option<()> {
        if self.vars.contains_key(name) {
            self.vars.insert(name.to_string(), value);
            Some(())
        } else {
            None
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.vars.get(name).map(|v| v.clone())
    }
}

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    // TODO: later replace Error with RuntimeError that has a call stack and stuff
    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), Error> {
        for s in stmts {
            match s {
                Stmt::Expr(e) => {
                    self.visit_expr(e)?;
                }
                Stmt::Print(e) => {
                    self.visit_print_stmt(e)?;
                }
                Stmt::VarDecl { name, value } => {
                    self.visit_var_stmt(name, value)?;
                }
            }
        }
        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<Value, Error> {
        match expr {
            Expr::Variable(token) => self
                .env
                .get(token.literal_identifier())
                .ok_or(Error::new("Undefined variable", token.offset())),
            Expr::Assign { name, value } => {
                let value = self.visit_expr(value.as_ref())?;
                self.env
                    .assign(name.literal_identifier(), value.clone())
                    .ok_or(Error::new("Undefined variable", name.offset()))?;
                Ok(value)
            }
            Expr::Nil => Ok(Value::Nil),
            Expr::Number(n) => Ok(Value::Number(*n)),
            Expr::String(s) => Ok(Value::String(s.to_string())),
            Expr::Boolean(b) => Ok(Value::Boolean(*b)),
            Expr::Grouping(e) => self.visit_expr(e.as_ref()),
            Expr::Unary { operator, right } => self.visit_unary_expr(operator, right),
            Expr::Binary {
                left,
                operator,
                right,
            } => self.visit_binary_expr(left, operator, right),
        }
    }

    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<(), Error> {
        println!("{}", self.visit_expr(expr)?);
        Ok(())
    }

    fn visit_var_stmt(&mut self, name: &str, expr: &Expr) -> Result<(), Error> {
        let value = self.visit_expr(expr)?;
        self.env.define(name, value.clone());
        Ok(())
    }

    // HELPERS
    // =======

    #[inline]
    fn visit_unary_expr(&mut self, op: &Token, right: &Expr) -> Result<Value, Error> {
        use TokenKind::*;

        match (op.kind(), self.visit_expr(right)?) {
            // Numbers
            (Minus, Value::Number(f)) => Ok(Value::Number(-f)),
            // Booleans
            (Bang, Value::Nil) => Ok(Value::Boolean(true)),
            (Bang, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
            (Bang, _) => Ok(Value::Boolean(true)),
            // Incompatible
            (Minus, _) => Err(Error::new("Operand must be a number", op.offset())),
            // Should not happen™️
            _ => Err(Error::new("Unexpected unary operator", op.offset())),
        }
    }

    #[inline]
    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value, Error> {
        use TokenKind::*;

        // Evaluate left-to-right:
        let left = self.visit_expr(left)?;
        let right = self.visit_expr(right)?;

        match (left, op.kind(), right) {
            // Numbers
            (Value::Number(f1), Minus, Value::Number(f2)) => Ok(Value::Number(f1 - f2)),
            (Value::Number(f1), Plus, Value::Number(f2)) => Ok(Value::Number(f1 + f2)),
            (Value::Number(f1), Star, Value::Number(f2)) => Ok(Value::Number(f1 * f2)),
            (Value::Number(f1), Slash, Value::Number(f2)) => Ok(Value::Number(f1 / f2)),
            // Strings
            (Value::String(s1), Plus, Value::String(s2)) => Ok(Value::String(s1.clone() + &s2)),
            // Ordering
            (Value::Number(f1), Greater, Value::Number(f2)) => Ok(Value::Boolean(f1 > f2)),
            (Value::Number(f1), GreaterEqual, Value::Number(f2)) => Ok(Value::Boolean(f1 >= f2)),
            (Value::Number(f1), Less, Value::Number(f2)) => Ok(Value::Boolean(f1 < f2)),
            (Value::Number(f1), LessEqual, Value::Number(f2)) => Ok(Value::Boolean(f1 <= f2)),
            // Equality
            (l, EqualEqual, r) => Ok(Value::Boolean(l == r)),
            (l, BangEqual, r) => Ok(Value::Boolean(l != r)),
            // Incompatible
            (_, Minus, _) => Err(Error::new("Operands must be numbers", op.offset())),
            (_, Plus, _) => Err(Error::new(
                "Operands must be numbers or strings",
                op.offset(),
            )),
            (_, Star, _) => Err(Error::new("Operands must be numbers", op.offset())),
            (_, Slash, _) => Err(Error::new("Operands must be numbers", op.offset())),
            (_, Greater, _) => Err(Error::new("Operands must be numbers", op.offset())),
            (_, GreaterEqual, _) => Err(Error::new("Operands must be numbers", op.offset())),
            (_, Less, _) => Err(Error::new("Operands must be numbers", op.offset())),
            (_, LessEqual, _) => Err(Error::new("Operands must be numbers", op.offset())),
            // Should not happen™️
            _ => Err(Error::new("Unexpected operation", op.offset())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nil() {
        todo!()
    }
}
