use std::{
    cell::RefCell,
    collections::HashMap,
    fmt,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    lexer::{Token, TokenKind},
    parser::{Expr, Stmt},
    Error,
};

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;

        match self {
            Nil => write!(f, "nil"),
            Number(n) if n.fract() == 0.0 => write!(f, "{}", *n as i64),
            Number(n) => write!(f, "{n}"),
            String(s) => write!(f, "\"{s}\""),
            Boolean(true) => write!(f, "true"),
            Boolean(false) => write!(f, "false"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;

        match (self, other) {
            (Number(l), Number(r)) => l == r,
            (String(l), String(r)) => l == r,
            (Boolean(l), Boolean(r)) => l == r,
            (Nil, Nil) => true,
            _ => false,
        }
    }
}

impl TryFrom<Var> for Value {
    type Error = String;

    fn try_from(value: Var) -> Result<Self, Self::Error> {
        match value {
            Var::Value(v) => Ok(v),
            Var::Function(_) => Err("Cannot evaluate a function reference".to_string()),
        }
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        use Value::*;

        match self {
            Nil => false,
            Boolean(b) => *b,
            _ => true,
        }
    }
}

trait Callable: CallableClone {
    fn name(&self) -> &str;
    fn offset(&self) -> usize;
    fn call(&self, it: &mut Interpreter, args: &[Rc<Var>]) -> Result<Rc<Var>, Error>;
}

trait CallableClone {
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
struct Function {
    name: String,
    params: Vec<String>,
    body: Vec<Stmt>,
    closure: Rc<RefCell<Environment>>,
    offset: usize,
}

impl Function {
    fn new(
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
        closure: &Rc<RefCell<Environment>>,
        offset: usize,
    ) -> Self {
        Self {
            name,
            params,
            body,
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

        match it.visit_block(&env, &self.body)? {
            Some(val) => Ok(val),
            None => Ok(Rc::new(Var::Value(Value::Nil))),
        }
    }
}

trait Callback: Fn() -> Value {
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
struct Native {
    name: String,
    callback: Box<dyn Callback>,
}

impl Native {
    fn new(name: &str, callback: Box<dyn Callback>) -> Self {
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

#[derive(Clone)]
enum Var {
    Value(Value),
    Function(Box<dyn Callable>),
}

impl Var {
    fn is_truthy(&self) -> bool {
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

type Return<T> = Option<T>;

#[derive(Clone)]
struct Environment {
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
    fn new(enclosing: &Rc<RefCell<Environment>>) -> Self {
        Self {
            enclosing: Some(Rc::clone(enclosing)),
            ..Default::default()
        }
    }

    fn define(&mut self, name: &str, value: Option<Rc<Var>>) {
        self.vars.insert(name.to_string(), value);
    }

    fn assign(&mut self, name: &str, value: Rc<Var>) -> Result<(), String> {
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

    fn get(&mut self, name: &str) -> Result<Rc<Var>, String> {
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

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Rc::new(RefCell::new(Environment::default()));

        env.borrow_mut().define(
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

        Self { env }
    }

    // TODO: later replace Error with RuntimeError that has a call stack and stuff
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

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<Return<Rc<Var>>, Error> {
        match stmt {
            Stmt::Block(stmts) => {
                let env = Rc::new(RefCell::new(Environment::new(&self.env)));

                self.visit_block(&env, stmts)
            }
            Stmt::Expr(e) => {
                self.visit_expr(e)?;
                Ok(None)
            }
            Stmt::Function { name, params, body } => {
                let fun = Function::new(
                    name.to_string(),
                    params.iter().map(|t| t.to_string()).collect(),
                    body.clone(),
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
            Stmt::Return(e) => Ok(Some(self.visit_expr(e)?)),
            Stmt::VarDecl { name, value } => {
                self.visit_var_stmt(name, value.as_ref())?;
                Ok(None)
            }
            Stmt::While { cond, body } => self.visit_while(cond, body),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<Rc<Var>, Error> {
        match expr {
            Expr::Assign { name, value } => {
                let value = self.visit_expr(value.as_ref())?;
                self.env
                    .as_ref()
                    .borrow_mut()
                    .assign(name.literal_identifier(), Rc::clone(&value))
                    .map_err(|msg| Error::new(&msg, name.offset()))?;
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
                    "<lambda>".to_string(),
                    params.iter().map(|t| t.to_string()).collect(),
                    body.clone(),
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
            Expr::Variable(token) => self
                .env
                .as_ref()
                .borrow_mut()
                .get(token.literal_identifier())
                .map_err(|msg| Error::new(&msg, token.offset())),
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

    fn visit_block(
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nil() {
        todo!()
    }
}
