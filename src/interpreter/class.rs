use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::{lexer::Token, Error, Interpreter};

use super::{callable::Function, environment::Var};

pub struct Class {
    name: String,
    methods: HashMap<String, Rc<Function>>,
    offset: usize,
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Class {
    pub fn new(name: &Token, methods: HashMap<String, Rc<Function>>) -> Self {
        Self {
            name: name.to_string(),
            methods,
            offset: name.offset(),
        }
    }

    pub fn call(
        self: &Rc<Self>,
        _it: &mut Interpreter,
        _args: &[Rc<Var>],
    ) -> Result<Rc<Var>, Error> {
        Ok(Rc::new(Var::Instance(RefCell::new(Instance::new(&self)))))
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    fn find_method(&self, name: &str) -> Option<Rc<Function>> {
        self.methods.get(name).map(|f| Rc::clone(f))
    }
}

pub struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, Rc<Var>>,
}

impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", self.class.name())
    }
}

impl Instance {
    pub fn new(class: &Rc<Class>) -> Self {
        Self {
            class: Rc::clone(class),
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, name: &Token) -> Result<Rc<Var>, Error> {
        let ident = name.literal_identifier();

        self.fields
            .get(ident)
            .map_or_else(
                || {
                    self.class
                        .find_method(ident)
                        .map(|m| Rc::new(Var::Function(Box::new(Rc::clone(&m)))))
                },
                |v| Some(Rc::clone(&v)),
            )
            .ok_or(Error::new("Unknown property", name.offset()))
    }

    pub fn set(&mut self, name: &Token, value: &Rc<Var>) {
        self.fields.insert(name.to_string(), Rc::clone(value));
    }
}
