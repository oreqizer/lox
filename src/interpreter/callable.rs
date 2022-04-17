use std::{cell::RefCell, rc::Rc};

use crate::{parser::Stmt, Error};

use super::{
    environment::{Environment, Var},
    interpreter::Interpreter,
    value::Value,
};

pub trait Callable {
    fn name(&self) -> &str;
    fn offset(&self) -> usize;
    fn call(&self, it: &mut Interpreter, args: &[Rc<Var>]) -> Result<Rc<Var>, Error>;
}

pub struct Function {
    name: String,
    params: Vec<String>,
    body: Vec<Stmt>,
    closure: Rc<RefCell<Environment>>,
    offset: usize,
}

impl Function {
    pub fn new(
        name: &str,
        params: &[String],
        body: &[Stmt],
        closure: &Rc<RefCell<Environment>>,
        offset: usize,
    ) -> Self {
        Self {
            name: name.to_string(),
            params: params.into(),
            body: body.into(),
            closure: Rc::clone(closure),
            offset,
        }
    }
}

impl Callable for Function {
    fn name(&self) -> &str {
        &self.name
    }

    fn offset(&self) -> usize {
        self.offset
    }

    fn call(&self, it: &mut Interpreter, args: &[Rc<Var>]) -> Result<Rc<Var>, Error> {
        let env = Rc::new(RefCell::new(Environment::new(&self.closure)));
        for (i, param) in self.params.iter().enumerate() {
            let arg = args
                .get(i)
                .ok_or(Error::new("Arity mismatch", self.offset))?;

            env.as_ref()
                .borrow_mut()
                .define(&param, Some(Rc::clone(arg)));
        }

        match it.execute_block(&env, &self.body)? {
            Some(val) => Ok(val),
            None => Ok(Rc::new(Var::Value(Value::Nil))),
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
}

impl Callable for Native {
    fn name(&self) -> &str {
        &self.name
    }

    fn offset(&self) -> usize {
        0
    }

    fn call(&self, _it: &mut Interpreter, _args: &[Rc<Var>]) -> Result<Rc<Var>, Error> {
        let cb = &self.callback;

        Ok(Rc::new(Var::Value(cb())))
    }
}
