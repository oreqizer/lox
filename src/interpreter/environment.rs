use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use super::{callable::Callable, value::Value};

#[derive(Clone)]
pub enum Var {
    Value(Value),
    Function(Box<dyn Callable>),
}

impl Var {
    pub fn is_truthy(&self) -> bool {
        match self {
            Var::Value(v) => v.is_truthy(),
            Var::Function(_) => true,
        }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Var::Value(v) => v.fmt(f),
            Var::Function(fun) => write!(f, "<fn {}>", fun.name()),
        }
    }
}

#[derive(Clone)]
pub struct Environment {
    vars: HashMap<String, Option<Rc<Var>>>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            vars: HashMap::new(),
            enclosing: None,
        }
    }
}

impl Environment {
    pub fn new(enclosing: &Rc<RefCell<Environment>>) -> Self {
        Self {
            enclosing: Some(Rc::clone(enclosing)),
            ..Default::default()
        }
    }

    pub fn define(&mut self, name: &str, value: Option<Rc<Var>>) {
        self.vars.insert(name.to_string(), value);
    }

    pub fn assign(&mut self, name: &str, value: Rc<Var>) -> Result<(), String> {
        if self.vars.contains_key(name) {
            self.vars.insert(name.to_string(), Some(value));
            Ok(())
        } else {
            match &self.enclosing {
                Some(e) => e.as_ref().borrow_mut().assign(name, value),
                None => Err("Assign to undefined variable".to_string()),
            }
        }
    }

    pub fn get(&mut self, name: &str) -> Result<Rc<Var>, String> {
        match self.vars.get(name) {
            Some(Some(v)) => Ok(Rc::clone(v)),
            Some(None) => Err("Access of uninitialized variable".to_string()),
            None => match &self.enclosing {
                Some(e) => e.as_ref().borrow_mut().get(name),
                None => Err("Undefined variable".to_string()),
            },
        }
    }
}
