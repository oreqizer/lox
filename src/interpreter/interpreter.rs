use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    lexer::{Token, TokenKind},
    parser::{Expr, Stmt},
    Error,
};

use super::{
    callable::{Function, Native},
    environment::{Environment, Var},
    value::Value,
};

type Return<T> = Option<T>;

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    locals: HashMap<usize, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::default()));

        globals.borrow_mut().define(
            "clock",
            Some(Rc::new(Var::Function(Box::new(Native::new(
                "clock",
                Box::new(|| {
                    Value::Number(
                        SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_millis() as f64,
                    )
                }),
            ))))),
        );

        Self {
            env: Rc::clone(&globals),
            globals,
            locals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), Error> {
        for s in stmts {
            self.visit_stmt(s)?;
        }
        Ok(())
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<Value, Error> {
        match self.visit_expr(expr)?.as_ref() {
            Var::Value(v) => Ok(v.clone()),
            Var::Function(f) => Err(Error::new("Cannot evaluate a function", f.offset())),
        }
    }

    pub fn resolve(&mut self, id: usize, depth: usize) {
        self.locals.insert(id, depth);
    }

    pub fn execute_block(
        &mut self,
        env: &Rc<RefCell<Environment>>,
        stmts: &[Stmt],
    ) -> Result<Return<Rc<Var>>, Error> {
        let mut res = Ok(None);

        let previous = Rc::clone(&self.env);
        self.env = Rc::clone(env);
        for stmt in stmts {
            match self.visit_stmt(stmt) {
                Ok(Some(v)) => {
                    res = Ok(Some(v));
                    break;
                }
                Err(e) => {
                    res = Err(e);
                    break;
                }
                _ => continue,
            }
        }
        self.env = previous;

        res
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<Return<Rc<Var>>, Error> {
        match stmt {
            Stmt::Block(stmts) => {
                let env = Rc::new(RefCell::new(Environment::new(&self.env)));

                self.execute_block(&env, stmts)
            }
            Stmt::Expr(e) => {
                self.visit_expr(e)?;
                Ok(None)
            }
            Stmt::Function { name, params, body } => {
                let fun = Function::new(
                    name.literal_identifier(),
                    &params.iter().map(|t| t.to_string()).collect::<Vec<_>>(),
                    body,
                    &self.env,
                    name.offset(),
                );

                self.env.as_ref().borrow_mut().define(
                    name.literal_identifier(),
                    Some(Rc::new(Var::Function(Box::new(fun)))),
                );
                Ok(None)
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
            } => self.visit_if(cond, then_branch, else_branch.as_deref()),
            Stmt::Print(e) => {
                self.visit_print_stmt(e)?;
                Ok(None)
            }
            Stmt::Return { value, .. } => Ok(Some(self.visit_expr(value)?)),
            Stmt::VarDecl { name, value } => {
                self.visit_var_stmt(name.literal_identifier(), value.as_ref())?;
                Ok(None)
            }
            Stmt::While { cond, body } => self.visit_while(cond, body),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<Rc<Var>, Error> {
        match expr {
            Expr::Assign { id, name, value } => {
                let value = self.visit_expr(value)?;

                let ident = name.literal_identifier();
                let map_err = |msg: String| Error::new(&msg, name.offset());
                if let Some(&depth) = self.locals.get(id) {
                    Environment::assign_at(&self.env, ident, depth, &value).map_err(map_err)?;
                } else {
                    Environment::assign(&self.globals, ident, &value).map_err(map_err)?;
                }

                Ok(value)
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => Ok(Rc::new(Var::Value(
                self.visit_binary_expr(left, operator, right)?,
            ))),
            Expr::Boolean(b) => Ok(Rc::new(Var::Value(Value::Boolean(*b)))),
            Expr::Call {
                callee,
                paren,
                args,
            } => self.visit_call_expr(callee, paren, args),
            Expr::Grouping(e) => self.visit_expr(e.as_ref()),
            Expr::Lambda { fun, params, body } => {
                let lambda = Function::new(
                    "<lambda>",
                    &params.iter().map(|t| t.to_string()).collect::<Vec<_>>(),
                    body,
                    &self.env,
                    fun.offset(),
                );

                Ok(Rc::new(Var::Function(Box::new(lambda))))
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => self.visit_logical_expr(left, operator, right),
            Expr::Nil => Ok(Rc::new(Var::Value(Value::Nil))),
            Expr::Number(n) => Ok(Rc::new(Var::Value(Value::Number(*n)))),
            Expr::String(s) => Ok(Rc::new(Var::Value(Value::String(s.to_string())))),
            Expr::Unary { operator, right } => {
                Ok(Rc::new(Var::Value(self.visit_unary_expr(operator, right)?)))
            }
            Expr::Variable { id, name } => self.look_up_var(*id, name),
        }
    }

    fn visit_if(
        &mut self,
        cond: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Stmt>,
    ) -> Result<Return<Rc<Var>>, Error> {
        if self.visit_expr(cond)?.is_truthy() {
            if let Some(v) = self.visit_stmt(then_branch)? {
                return Ok(Some(v));
            }
        } else if let Some(stmt) = else_branch {
            if let Some(v) = self.visit_stmt(stmt)? {
                return Ok(Some(v));
            }
        }

        Ok(None)
    }

    fn visit_while(&mut self, cond: &Expr, body: &Stmt) -> Result<Return<Rc<Var>>, Error> {
        while self.visit_expr(cond)?.is_truthy() {
            if let Some(v) = self.visit_stmt(body)? {
                return Ok(Some(v));
            }
        }
        Ok(None)
    }

    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<(), Error> {
        println!("{}", self.visit_expr(expr)?);
        Ok(())
    }

    fn visit_var_stmt(&mut self, name: &str, expr: Option<&Expr>) -> Result<(), Error> {
        let value = match expr {
            Some(expr) => Some(self.visit_expr(expr)?),
            None => None,
        };

        self.env.as_ref().borrow_mut().define(name, value);
        Ok(())
    }

    // HELPERS
    // =======

    #[inline]
    fn visit_logical_expr(
        &mut self,
        left: &Expr,
        op: &Token,
        right: &Expr,
    ) -> Result<Rc<Var>, Error> {
        use TokenKind::{And, Or};

        let left = self.visit_expr(left)?;

        match (op.kind(), left.as_ref()) {
            (Or, Var::Value(val)) => {
                if val.is_truthy() {
                    Ok(left)
                } else {
                    self.visit_expr(right)
                }
            }
            (Or, _) => Ok(left),
            (And, Var::Value(val)) => {
                if !val.is_truthy() {
                    Ok(left)
                } else {
                    self.visit_expr(right)
                }
            }
            (And, _) => self.visit_expr(right),
            // Should not happen™️
            _ => Err(Error::new("Unexpected logical operator", op.offset())),
        }
    }

    #[inline]
    fn visit_call_expr(
        &mut self,
        callee: &Expr,
        paren: &Token,
        args: &[Expr],
    ) -> Result<Rc<Var>, Error> {
        let expr = self.visit_expr(callee)?;
        let callee = match expr.as_ref() {
            Var::Function(f) => f,
            Var::Value(_) => return Err(Error::new("Expression not callable", paren.offset())),
        };

        let args: Vec<_> = args
            .iter()
            .map(|t| self.visit_expr(t))
            .collect::<Result<_, _>>()?;

        callee.call(self, &args)
    }

    #[inline]
    fn visit_unary_expr(&mut self, op: &Token, right: &Expr) -> Result<Value, Error> {
        use TokenKind::{Bang, Minus};

        match (op.kind(), self.visit_expr(right)?.as_ref()) {
            // Numbers
            (Minus, Var::Value(Value::Number(f))) => Ok(Value::Number(-f)),
            // Booleans
            (Bang, Var::Value(Value::Nil)) => Ok(Value::Boolean(true)),
            (Bang, Var::Value(Value::Boolean(b))) => Ok(Value::Boolean(!b)),
            (Bang, _) => Ok(Value::Boolean(true)),
            // Incompatible
            (Minus, _) => Err(Error::new("Operand must be a number", op.offset())),
            // Should not happen™️
            _ => Err(Error::new("Unexpected unary operator", op.offset())),
        }
    }

    #[inline]
    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value, Error> {
        use TokenKind::{
            BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Plus, Slash, Star,
        };

        // Evaluate left-to-right:
        let left = self.visit_expr(left)?;
        let right = self.visit_expr(right)?;

        match (left.as_ref(), op.kind(), right.as_ref()) {
            // Numbers
            (Var::Value(Value::Number(f1)), Minus, Var::Value(Value::Number(f2))) => {
                Ok(Value::Number(f1 - f2))
            }
            (Var::Value(Value::Number(f1)), Plus, Var::Value(Value::Number(f2))) => {
                Ok(Value::Number(f1 + f2))
            }
            (Var::Value(Value::Number(f1)), Star, Var::Value(Value::Number(f2))) => {
                Ok(Value::Number(f1 * f2))
            }
            (Var::Value(Value::Number(f1)), Slash, Var::Value(Value::Number(f2))) => {
                Ok(Value::Number(f1 / f2))
            }
            // Strings
            (Var::Value(Value::String(s1)), Plus, Var::Value(Value::String(s2))) => {
                Ok(Value::String(s1.clone() + &s2))
            }
            // Ordering
            (Var::Value(Value::Number(f1)), Greater, Var::Value(Value::Number(f2))) => {
                Ok(Value::Boolean(f1 > f2))
            }
            (Var::Value(Value::Number(f1)), GreaterEqual, Var::Value(Value::Number(f2))) => {
                Ok(Value::Boolean(f1 >= f2))
            }
            (Var::Value(Value::Number(f1)), Less, Var::Value(Value::Number(f2))) => {
                Ok(Value::Boolean(f1 < f2))
            }
            (Var::Value(Value::Number(f1)), LessEqual, Var::Value(Value::Number(f2))) => {
                Ok(Value::Boolean(f1 <= f2))
            }
            // Equality
            (Var::Value(l), EqualEqual, Var::Value(r)) => Ok(Value::Boolean(l == r)),
            (Var::Value(l), BangEqual, Var::Value(r)) => Ok(Value::Boolean(l != r)),
            // Incompatible
            (_, Minus, _) => Err(Error::new("Operands must be numbers", op.offset())),
            (_, Plus, _) => Err(Error::new(
                "Operands must be numbers or strings",
                op.offset(),
            )),
            (_, Star, _) => Err(Error::new("Operands must be numbers", op.offset())),
            (_, Slash, _) => Err(Error::new("Operands must be numbers", op.offset())),
            (_, Greater, _) => Err(Error::new("Operands must be numbers", op.offset())),
            (_, GreaterEqual, _) => Err(Error::new("Operands must be numbers", op.offset())),
            (_, Less, _) => Err(Error::new("Operands must be numbers", op.offset())),
            (_, LessEqual, _) => Err(Error::new("Operands must be numbers", op.offset())),
            // Should not happen™️
            _ => Err(Error::new("Unexpected operation", op.offset())),
        }
    }

    #[inline]
    fn look_up_var(&self, id: usize, token: &Token) -> Result<Rc<Var>, Error> {
        let name = token.literal_identifier();
        let map_err = |e: String| Error::new(&e, token.offset());

        match self.locals.get(&id) {
            Some(&depth) => Environment::get_at(&self.env, name, depth).map_err(map_err),
            None => Environment::get(&self.globals, name).map_err(map_err),
        }
    }
}
