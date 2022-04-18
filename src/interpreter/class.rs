use std::fmt;

use crate::lexer::Token;

pub struct Class {
    name: String,
    offset: usize,
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Class {
    pub fn new(name: &Token) -> Self {
        Self { name: name.to_string(), offset: name.offset() }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}
