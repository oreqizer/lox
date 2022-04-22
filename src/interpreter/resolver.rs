use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    lexer::Token,
    parser::{Expr, Function, Stmt},
    Error, Interpreter,
};

#[derive(Clone, Copy)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Clone, Copy, PartialEq)]
enum ClassType {
    None,
    Class,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    interpreter: Rc<RefCell<Interpreter>>,
    current_fn: FunctionType,
    current_class: ClassType,
}

impl Resolver {
    pub fn new(interpreter: &Rc<RefCell<Interpreter>>) -> Self {
        Self {
            scopes: Vec::new(),
            interpreter: Rc::clone(interpreter),
            current_fn: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn resolve(&mut self, stmts: &[Stmt]) -> Result<(), Error> {
        for s in stmts {
            self.visit_stmt(s)?;
        }
        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<(), Error> {
        match expr {
            Expr::Assign { id, name, value } => self.visit_assign_expr(*id, name, value),
            Expr::Binary { left, right, .. } => {
                self.visit_expr(left)?;
                self.visit_expr(right)
            }
            Expr::Call { callee, args, .. } => {
                self.visit_expr(callee)?;
                for a in args {
                    self.visit_expr(a)?;
                }
                Ok(())
            }
            Expr::Get { object, .. } => self.visit_expr(object),
            Expr::Grouping(expr) => self.visit_expr(expr),
            Expr::Logical { left, right, .. } => {
                self.visit_expr(left)?;
                self.visit_expr(right)
            }
            Expr::Set { object, value, .. } => {
                self.visit_expr(object)?;
                self.visit_expr(value)
            }
            Expr::This { id, keyword } => {
                if self.current_class == ClassType::None {
                    return Err(Error::new(
                        "Can't use 'this' outside of a class",
                        keyword.offset(),
                    ));
                }

                self.resolve_local(*id, keyword);
                Ok(())
            }
            Expr::Unary { right, .. } => self.visit_expr(right),
            Expr::Variable { id, name } => self.visit_var_expr(*id, name),
            _ => Ok(()),
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Block(body) => self.visit_block_stmt(body),
            Stmt::Class { name, methods } => self.visit_class_stmt(name, methods),
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::Function(Function { name, params, body }) => {
                self.visit_fun_decl(name, params, body)
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.visit_expr(cond)?;
                self.visit_stmt(then_branch)?;
                if let Some(s) = else_branch {
                    self.visit_stmt(s)?;
                }
                Ok(())
            }
            Stmt::Print(expr) => self.visit_expr(expr),
            Stmt::Return { token, value } => {
                match self.current_fn {
                    FunctionType::None => {
                        return Err(Error::new(
                            "Cannot return from top-level code",
                            token.offset(),
                        ))
                    }
                    FunctionType::Initializer if value != &Expr::Nil => {
                        return Err(Error::new(
                            "Can't return a value from an initializer",
                            token.offset(),
                        ))
                    }
                    _ => (),
                };

                self.visit_expr(value)
            }
            Stmt::VarDecl { name, value } => self.visit_var_decl(name, value),
            Stmt::While { cond, body } => {
                self.visit_expr(cond)?;
                self.visit_stmt(body)
            }
        }
    }

    fn visit_block_stmt(&mut self, body: &[Stmt]) -> Result<(), Error> {
        self.scope_start();
        self.resolve(body)?;
        self.scope_end();
        Ok(())
    }

    fn visit_class_stmt(&mut self, name: &Token, methods: &[Function]) -> Result<(), Error> {
        let enclosing_class = self.current_class;
        self.current_class = ClassType::Class;

        self.declare(name);
        self.define(name);

        self.scope_start();
        if let Some(s) = self.scopes.last_mut() {
            s.insert("this".to_string(), true);
        }

        for m in methods {
            let kind = if m.name.literal_identifier() == "init" {
                FunctionType::Method
            } else {
                FunctionType::Initializer
            };

            self.resolve_function(&m.params, &m.body, kind)?;
        }
        self.scope_end();

        self.current_class = enclosing_class;
        Ok(())
    }

    fn visit_fun_decl(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<(), Error> {
        self.declare(name);
        self.define(name);
        self.resolve_function(params, body, FunctionType::Function)?;
        Ok(())
    }

    fn visit_var_decl(&mut self, name: &Token, value: &Option<Expr>) -> Result<(), Error> {
        self.declare(name);
        if let Some(e) = value {
            self.visit_expr(e)?;
        }
        self.define(name);
        Ok(())
    }

    fn visit_var_expr(&mut self, id: usize, name: &Token) -> Result<(), Error> {
        let var = self
            .scopes
            .last()
            .and_then(|s| s.get(name.literal_identifier()));

        if let Some(false) = var {
            return Err(Error::new(
                "Cannot read local variable in its own initializer",
                name.offset(),
            ));
        }

        self.resolve_local(id, name);
        Ok(())
    }

    fn visit_assign_expr(&mut self, id: usize, name: &Token, value: &Expr) -> Result<(), Error> {
        self.visit_expr(value)?;
        self.resolve_local(id, name);
        Ok(())
    }

    // === Operations ===

    fn scope_start(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn scope_end(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) {
        if let Some(s) = self.scopes.last_mut() {
            s.insert(name.to_string(), false);
        }
    }

    fn define(&mut self, name: &Token) {
        if let Some(s) = self.scopes.last_mut() {
            s.insert(name.to_string(), true);
        }
    }

    fn resolve_local(&mut self, id: usize, name: &Token) {
        self.scopes.iter().rev().enumerate().for_each(|(i, s)| {
            if s.contains_key(name.literal_identifier()) {
                self.interpreter.as_ref().borrow_mut().resolve(id, i)
            }
        });
    }

    // === Helpers ===

    fn resolve_function(
        &mut self,
        params: &[Token],
        body: &[Stmt],
        kind: FunctionType,
    ) -> Result<(), Error> {
        let enclosing_fn = self.current_fn;
        self.current_fn = kind;

        self.scope_start();
        for p in params {
            self.declare(p);
            self.define(p);
        }
        self.resolve(body)?;
        self.scope_end();

        self.current_fn = enclosing_fn;
        Ok(())
    }
}
