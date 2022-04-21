use std::fmt;

use super::environment::Var;

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
            Number(n) => write!(f, "{n}"),
            String(s) => write!(f, "\"{s}\""),
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

impl TryFrom<Var> for Value {
    type Error = String;

    fn try_from(value: Var) -> Result<Self, Self::Error> {
        match value {
            Var::Value(v) => Ok(v),
            Var::Class(_) => Err("Cannot evaluate an class".to_string()),
            Var::Instance(_) => Err("Cannot evaluate an instance".to_string()),
            Var::Function(_) | Var::Native(_) => Err("Cannot evaluate a function".to_string()),
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        use Value::*;

        match self {
            Nil => false,
            Boolean(b) => *b,
            _ => true,
        }
    }
}
