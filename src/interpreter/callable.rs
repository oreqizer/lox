use std::{cell::RefCell, rc::Rc};

use crate::{parser::Stmt, Error};

use super::{
    environment::{Environment, Var},
    interpreter::Interpreter,
    value::Value,
};

pub trait Callable: CallableClone {
    fn name(&self) -> &str;
    fn offset(&self) -> usize;
    fn call(&self, it: &mut Interpreter, args: &[Rc<Var>]) -> Result<Rc<Var>, Error>;
}

pub trait CallableClone {
    fn clone_box(&self) -> Box<dyn Callable>;
}

impl<T> CallableClone for T
where
    T: 'static + Callable + Clone,
{
    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Callable> {
    fn clone(&self) -> Box<dyn Callable> {
        self.clone_box()
    }
}

#[derive(Clone)]
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
            closure: Rc::new(RefCell::new(Environment::clone(&closure.as_ref().borrow()))),
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

pub trait Callback: Fn() -> Value {
    fn clone_box(&self) -> Box<dyn Callback>;
}

impl<T> Callback for T
where
    T: 'static + Fn() -> Value + Clone,
{
    fn clone_box(&self) -> Box<dyn Callback> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Callback> {
    fn clone(&self) -> Box<dyn Callback> {
        self.clone_box()
    }
}

#[derive(Clone)]
pub struct Native {
    name: String,
    callback: Box<dyn Callback>,
}

impl Native {
    pub fn new(name: &str, callback: Box<dyn Callback>) -> Self {
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
