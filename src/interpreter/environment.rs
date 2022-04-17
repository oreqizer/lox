use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use super::{callable::Callable, value::Value};

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

    pub fn assign(from: &Rc<RefCell<Self>>, name: &str, value: &Rc<Var>) -> Result<(), String> {
        let mut env = from.as_ref().borrow_mut();

        if env.vars.contains_key(name) {
            env.vars.insert(name.to_string(), Some(Rc::clone(value)));
            Ok(())
        } else {
            Err("Assign to undefined variable".to_string())
        }
    }

    pub fn assign_at(
        from: &Rc<RefCell<Self>>,
        name: &str,
        depth: usize,
        value: &Rc<Var>,
    ) -> Result<(), String> {
        Environment::assign(&Environment::ancestor(from, depth), name, value)
    }

    pub fn get(from: &Rc<RefCell<Self>>, name: &str) -> Result<Rc<Var>, String> {
        let env = from.as_ref().borrow();

        match env.vars.get(name) {
            Some(Some(v)) => Ok(Rc::clone(v)),
            Some(None) => Err("Access of uninitialized variable".to_string()),
            None => Err("Undefined variable".to_string()),
        }
    }

    pub fn get_at(from: &Rc<RefCell<Self>>, name: &str, depth: usize) -> Result<Rc<Var>, String> {
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
