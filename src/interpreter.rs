use std::fmt;

use crate::{
    lexer::{Token, TokenKind},
    parser::{Expr, Stmt},
    Error,
};

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

impl TryFrom<&Expr> for Value {
    type Error = Error;

    fn try_from(expr: &Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Nil => Ok(Value::Nil),
            Expr::Number(n) => Ok(Value::Number(*n)),
            Expr::String(s) => Ok(Value::String(s.to_string())),
            Expr::Boolean(b) => Ok(Value::Boolean(*b)),
            Expr::Grouping(e) => e.as_ref().try_into(),
            Expr::Unary { operator, right } => Value::from_unary(operator, right),
            Expr::Binary {
                left,
                operator,
                right,
            } => Value::from_binary(left, operator, right),
        }
    }
}

impl Value {
    #[inline]
    fn from_unary(op: &Token, right: &Expr) -> Result<Value, Error> {
        use TokenKind::*;

        match (op.kind(), right.try_into()?) {
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
    fn from_binary(left: &Expr, op: &Token, right: &Expr) -> Result<Value, Error> {
        use TokenKind::*;

        // Evaluate left-to-right:
        let left = left.try_into()?;
        let right = right.try_into()?;

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

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self{}
    }

    // TODO: later replace Error with RuntimeError that has a call stack and stuff
    pub fn interpret(&self, stmts: &[Stmt]) -> Result<(), Error> {
        for s in stmts {
            self.execute(s)?;
        }
        Ok(())
    }

    pub fn execute(&self, stmt: &Stmt) -> Result<Option<Value>, Error> {
        match stmt {
            Stmt::Expr(e) => Ok(Some(e.try_into()?)),
            Stmt::Print(e) => { self.print(e)?; Ok(None) },
        }
    }

    fn print(&self, expr: &Expr) -> Result<(), Error> {
        println!("{}", self.evaluate(expr)?);
        Ok(())
    }

    fn evaluate(&self, expr: &Expr) -> Result<Value, Error> {
        expr.try_into()
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
