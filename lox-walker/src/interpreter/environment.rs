use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use super::{
    class::{Class, Instance},
    function::{Function, Native},
    value::Value,
};

#[derive(Clone)]
pub enum Var {
    Class(Rc<Class>),
    Function(Rc<Function>),
    Instance(Rc<RefCell<Instance>>),
    Native(Rc<Native>),
    Value(Value),
}

impl Var {
    pub fn is_truthy(&self) -> bool {
        match self {
            Var::Value(v) => v.is_truthy(),
            Var::Instance(_) | Var::Function(_) | Var::Native(_) | Var::Class(_) => true,
        }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Var::Class(c) => fmt::Display::fmt(c, f),
            Var::Instance(i) => fmt::Display::fmt(&i.borrow(), f),
            Var::Function(fun) => write!(f, "<fn {}>", fun.name()),
            Var::Native(fun) => write!(f, "<fn {}>", fun.name()),
            Var::Value(v) => fmt::Display::fmt(v, f),
        }
    }
}

pub struct Environment {
    vars: HashMap<String, Option<Var>>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
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
    pub fn new(enclosing: &Rc<RefCell<Self>>) -> Self {
        Self {
            enclosing: Some(Rc::clone(enclosing)),
            ..Default::default()
        }
    }

    pub fn enclosing(&self) -> Option<Rc<RefCell<Self>>> {
        self.enclosing.as_ref().map(|e| Rc::clone(e))
    }

    pub fn define(&mut self, name: &str, value: Option<Var>) {
        self.vars.insert(name.to_string(), value);
    }

    pub fn assign(from: &Rc<RefCell<Self>>, name: &str, value: &Var) -> Result<(), String> {
        let mut env = from.as_ref().borrow_mut();

        if env.vars.contains_key(name) {
            env.vars.insert(name.to_string(), Some(value.clone()));
            Ok(())
        } else {
            Err("Assign to undefined variable".to_string())
        }
    }

    pub fn assign_at(
        from: &Rc<RefCell<Self>>,
        name: &str,
        depth: usize,
        value: &Var,
    ) -> Result<(), String> {
        Environment::assign(&Environment::ancestor(from, depth), name, value)
    }

    pub fn get(from: &Rc<RefCell<Self>>, name: &str) -> Result<Var, String> {
        let env = from.as_ref().borrow();

        match env.vars.get(name) {
            Some(Some(v)) => Ok(v.clone()),
            Some(None) => Err("Access of uninitialized variable".to_string()),
            None => Err("Undefined variable".to_string()),
        }
    }

    pub fn get_at(from: &Rc<RefCell<Self>>, name: &str, depth: usize) -> Result<Var, String> {
        Environment::get(&Environment::ancestor(from, depth), name)
    }

    fn ancestor(from: &Rc<RefCell<Self>>, depth: usize) -> Rc<RefCell<Environment>> {
        (0..depth).fold(Rc::clone(&from), |next, _| {
            Rc::clone(
                next.as_ref()
                    .borrow()
                    .enclosing
                    .as_ref()
                    .expect("Failed to find environment ancestor"),
            )
        })
    }
}
