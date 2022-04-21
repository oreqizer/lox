use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::{lexer::Token, Error, Interpreter};

use super::{environment::Var, function::Function};

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

    pub fn call(self: &Rc<Self>, _it: &mut Interpreter, _args: &[Var]) -> Result<Var, Error> {
        Ok(Var::Instance(Rc::new(RefCell::new(Instance::new(&self)))))
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
    fields: HashMap<String, Var>,
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

    pub fn get(from: &Rc<RefCell<Self>>, name: &Token) -> Result<Var, Error> {
        let ident = name.literal_identifier();
        let i = from.as_ref().borrow();
        
        i.fields
            .get(ident)
            .map_or_else(
                || {
                    i.class
                        .find_method(ident)
                        .map(|m| Var::Function(Rc::new(m.bind(from))))
                },
                |v| Some(v.clone()),
            )
            .ok_or(Error::new("Unknown property", name.offset()))
    }

    pub fn set(&mut self, name: &Token, value: &Var) {
        self.fields.insert(name.to_string(), value.clone());
    }
}
