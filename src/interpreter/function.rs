use std::{cell::RefCell, rc::Rc};

use crate::{parser::Stmt, Error, lexer::Token};

use super::{
    environment::{Environment, Var},
    interpreter::Interpreter,
    value::Value,
};

pub struct Function {
    name: String,
    params: Vec<String>,
    body: Vec<Stmt>,
    closure: Rc<RefCell<Environment>>,
    offset: usize,
}

impl Function {
    pub fn new(
        name: impl ToString,
        params: &[Token],
        body: &[Stmt],
        closure: &Rc<RefCell<Environment>>,
        offset: usize,
    ) -> Self {
        Self {
            name: name.to_string(),
            params: params.iter().map(|t| t.to_string()).collect(),
            body: body.into(),
            closure: Rc::clone(closure),
            offset,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn call(&self, it: &mut Interpreter, args: &[Var]) -> Result<Var, Error> {
        let env = Rc::new(RefCell::new(Environment::new(&self.closure)));
        for (i, param) in self.params.iter().enumerate() {
            let arg = args
                .get(i)
                .ok_or(Error::new("Arity mismatch", self.offset))?;

            env.as_ref()
                .borrow_mut()
                .define(&param, Some(arg.clone()));
        }

        match it.execute_block(&env, &self.body)? {
            Some(val) => Ok(val),
            None => Ok(Var::Value(Value::Nil))
        }
    }
}

pub struct Native {
    name: String,
    callback: Box<dyn Fn() -> Value>,
}

impl Native {
    pub fn new(name: &str, callback: Box<dyn Fn() -> Value>) -> Self {
        Self {
            name: name.to_string(),
            callback,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn offset(&self) -> usize {
        0
    }

    pub fn call(&self, _it: &mut Interpreter, _args: &[Var]) -> Result<Var, Error> {
        let cb = &self.callback;

        Ok(Var::Value(cb()))
    }
}
