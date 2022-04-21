use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    lexer::{Token, TokenKind},
    parser::{Expr, Function as FunctionStmt, Stmt},
    Error,
};

use super::{
    class::Class,
    environment::{Environment, Var},
    function::{Function, Native},
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
            Some(Var::Native(Rc::new(Native::new(
                "clock",
                Box::new(|| {
                    Value::Number(
                        SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_millis() as f64,
                    )
                }),
            )))),
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
        match self.visit_expr(expr)? {
            Var::Value(v) => Ok(v.clone()),
            Var::Class(_) => Err(Error::new("Cannot evaluate a class", 0)),
            Var::Instance(_) => Err(Error::new("Cannot evaluate an instance", 0)),
            Var::Function(_) | Var::Native(_) => Err(Error::new("Cannot evaluate a function", 0)),
        }
    }

    pub fn resolve(&mut self, id: usize, depth: usize) {
        self.locals.insert(id, depth);
    }

    pub fn execute_block(
        &mut self,
        env: &Rc<RefCell<Environment>>,
        stmts: &[Stmt],
    ) -> Result<Return<Var>, Error> {
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

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<Return<Var>, Error> {
        match stmt {
            Stmt::Block(stmts) => {
                let env = Rc::new(RefCell::new(Environment::new(&self.env)));

                self.execute_block(&env, stmts)
            }
            Stmt::Class { name, methods } => {
                self.visit_class_stmt(name, methods)?;
                Ok(None)
            }
            Stmt::Expr(e) => {
                self.visit_expr(e)?;
                Ok(None)
            }
            Stmt::Function(FunctionStmt { name, params, body }) => {
                let fun = Function::new(name, &params, body, &self.env, name.offset());

                self.env
                    .as_ref()
                    .borrow_mut()
                    .define(name.literal_identifier(), Some(Var::Function(Rc::new(fun))));
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

    fn visit_expr(&mut self, expr: &Expr) -> Result<Var, Error> {
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
            } => Ok(Var::Value(self.visit_binary_expr(left, operator, right)?)),
            Expr::Boolean(b) => Ok(Var::Value(Value::Boolean(*b))),
            Expr::Call {
                callee,
                paren,
                args,
            } => self.visit_call_expr(callee, paren, args),
            Expr::Get { object, name } => match self.visit_expr(object)? {
                Var::Instance(i) => Ok(i.borrow().get(name)?),
                _ => Err(Error::new("Only instances have fields", name.offset())),
            },
            Expr::Grouping(e) => self.visit_expr(e.as_ref()),
            Expr::Lambda { fun, params, body } => {
                let lambda = Function::new("<lambda>", &params, body, &self.env, fun.offset());

                Ok(Var::Function(Rc::new(lambda)))
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => self.visit_logical_expr(left, operator, right),
            Expr::Nil => Ok(Var::Value(Value::Nil)),
            Expr::Number(n) => Ok(Var::Value(Value::Number(*n))),
            Expr::Set {
                object,
                name,
                value,
            } => {
                let object = self.visit_expr(object)?;
                let instance = match object {
                    Var::Instance(i) => Ok(i),
                    _ => Err(Error::new("Only instances have fields", name.offset())),
                }?;
                let value = self.visit_expr(value)?;

                instance.borrow_mut().set(name, &value);

                Ok(value)
            }
            Expr::String(s) => Ok(Var::Value(Value::String(s.to_string()))),
            Expr::Unary { operator, right } => {
                Ok(Var::Value(self.visit_unary_expr(operator, right)?))
            }
            Expr::Variable { id, name } => self.look_up_var(*id, name),
        }
    }

    fn visit_class_stmt(&mut self, name: &Token, methods: &[FunctionStmt]) -> Result<(), Error> {
        let methods = methods
            .iter()
            .map(|m| {
                let name = m.name.to_string();
                let fun = Rc::new(Function::new(
                    &m.name,
                    &m.params,
                    &m.body,
                    &self.env,
                    m.name.offset(),
                ));

                (name, fun)
            })
            .collect::<HashMap<String, Rc<Function>>>();

        let class = Class::new(name, methods);

        self.env
            .as_ref()
            .borrow_mut()
            .define(name.literal_identifier(), Some(Var::Class(Rc::new(class))));
        Ok(())
    }

    fn visit_if(
        &mut self,
        cond: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Stmt>,
    ) -> Result<Return<Var>, Error> {
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

    fn visit_while(&mut self, cond: &Expr, body: &Stmt) -> Result<Return<Var>, Error> {
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
    fn visit_logical_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Var, Error> {
        use TokenKind::{And, Or};

        let left = self.visit_expr(left)?;

        match (op.kind(), &left) {
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
    ) -> Result<Var, Error> {
        let expr = self.visit_expr(callee)?;
        match expr {
            Var::Instance(_) => return Err(Error::new("Instance not callable", paren.offset())),
            Var::Value(_) => return Err(Error::new("Expression not callable", paren.offset())),
            _ => (),
        };

        let args = args
            .iter()
            .map(|t| self.visit_expr(t))
            .collect::<Result<Vec<_>, _>>()?;

        match expr {
            Var::Class(c) => c.call(self, &args),
            Var::Function(c) => c.call(self, &args),
            Var::Native(c) => c.call(self, &args),
            // Should not happen™️
            _ => Err(Error::new("Not callable", paren.offset())),
        }
    }

    #[inline]
    fn visit_unary_expr(&mut self, op: &Token, right: &Expr) -> Result<Value, Error> {
        use TokenKind::{Bang, Minus};

        match (op.kind(), self.visit_expr(right)?) {
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

        match (left, op.kind(), right) {
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
    fn look_up_var(&self, id: usize, token: &Token) -> Result<Var, Error> {
        let name = token.literal_identifier();
        let map_err = |e: String| Error::new(&e, token.offset());

        match self.locals.get(&id) {
            Some(&depth) => Environment::get_at(&self.env, name, depth).map_err(map_err),
            None => Environment::get(&self.globals, name).map_err(map_err),
        }
    }
}
