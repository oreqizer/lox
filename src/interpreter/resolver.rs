use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    lexer::Token,
    parser::{Expr, Stmt},
    Error, Interpreter,
};

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    interpreter: Rc<RefCell<Interpreter>>,
}

impl Resolver {
    pub fn new(interpreter: &Rc<RefCell<Interpreter>>) -> Self {
        Self {
            scopes: Vec::new(),
            interpreter: Rc::clone(interpreter),
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
            Expr::Grouping(expr) => self.visit_expr(expr),
            Expr::Logical { left, right, .. } => {
                self.visit_expr(left)?;
                self.visit_expr(right)
            }
            Expr::Unary { right, .. } => self.visit_expr(right),
            Expr::Variable { id, name } => self.visit_var_expr(*id, name),
            _ => Ok(()),
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Block(body) => self.visit_block_stmt(body),
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::Function { name, params, body } => self.visit_fun_decl(name, params, body),
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
            Stmt::Return(expr) => self.visit_expr(expr),
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

    fn visit_fun_decl(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<(), Error> {
        self.declare(name);
        self.define(name);
        self.resolve_function(params, body)?;
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
                self.interpreter
                    .as_ref()
                    .borrow_mut()
                    .resolve(id, self.scopes.len() - i - 1)
            }
        });
    }

    // === Helpers ===

    fn resolve_function(&mut self, params: &[Token], body: &[Stmt]) -> Result<(), Error> {
        self.scope_start();
        for p in params {
            self.declare(p);
            self.define(p);
        }
        self.resolve(body)?;
        self.scope_end();
        Ok(())
    }
}
