use std::{fmt, iter::Peekable, rc::Rc, slice::Iter};

use crate::lexer::{Token, TokenKind};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
    Grouping(Rc<Expr>),
    Unary {
        operator: Token,
        right: Rc<Expr>,
    },
    Binary {
        left: Rc<Expr>,
        operator: Token,
        right: Rc<Expr>,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;

        match self {
            Boolean(b) => write!(f, "{}", b),
            Nil => write!(f, "nil"),
            Number(n) => write!(f, "{}", n),
            String(ref s) => write!(f, "{}", s),
            Grouping(expr) => write!(f, "({})", expr),
            Unary { operator, right } => write!(f, "{}{}", operator, right),
            Binary {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator, right),
        }
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Expr, String> {
        self.expression()
    }

    fn synchronize(&mut self) {
        use TokenKind::*;

        while let Some(token) = self.tokens.next() {
            if let Semicolon = token.kind() {
                return;
            };

            match self.tokens.peek().map(|t| t.kind()) {
                Some(Class) => return,
                Some(Fun) => return,
                Some(Var) => return,
                Some(For) => return,
                Some(If) => return,
                Some(While) => return,
                Some(Print) => return,
                Some(Return) => return,
                _ => (),
            };
        }
    }

    // HELPERS
    // =======

    #[inline]
    fn match_token(&mut self, kinds: &[TokenKind]) -> Option<&Token> {
        if let Some(&e) = self.tokens.peek() {
            if kinds.iter().any(|&k| k == e.kind()) {
                return self.tokens.next();
            }
        }

        None
    }

    #[inline]
    fn make_binary(
        &mut self,
        kinds: &[TokenKind],
        gen: impl Fn(&mut Self) -> Result<Expr, String>,
    ) -> Result<Expr, String> {
        let mut expr = gen(self)?;

        while let Some(op) = self.match_token(kinds) {
            expr = Expr::Binary {
                left: Rc::new(expr),
                operator: op.clone(),
                right: Rc::new(gen(self)?),
            };
        }

        Ok(expr)
    }

    #[inline]
    fn next_ok(&mut self, expr: Expr) -> Result<Expr, String> {
        self.tokens.next();

        Ok(expr)
    }

    #[inline]
    fn next_assert(&mut self, kind: TokenKind, msg: &str) -> Result<&Token, String> {
        if let Some(&e) = self.tokens.peek() {
            if e.kind() == kind {
                return Ok(self.tokens.next().unwrap());
            }
        }

        Err(msg.to_string())
    }

    // GRAMMAR
    // =======

    // expression → equality;
    fn expression(&mut self) -> Result<Expr, String> {
        return self.equality();
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Expr, String> {
        use TokenKind::*;

        self.make_binary(&[BangEqual, EqualEqual], Parser::comparison)
    }

    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Expr, String> {
        use TokenKind::*;

        self.make_binary(&[Greater, GreaterEqual, Less, LessEqual], Parser::term)
    }

    // term → factor ( ( "+" | "-" ) factor )* ;
    fn term(&mut self) -> Result<Expr, String> {
        use TokenKind::*;

        self.make_binary(&[Plus, Minus], Parser::factor)
    }

    // factor → unary ( ( "*" | "/" ) unary )* ;
    fn factor(&mut self) -> Result<Expr, String> {
        use TokenKind::*;

        self.make_binary(&[Star, Slash], Parser::unary)
    }

    // unary → ( "!" | "-" ) unary
    //       | primary ;
    fn unary(&mut self) -> Result<Expr, String> {
        use TokenKind::*;

        if let Some(op) = self.match_token(&[Bang, Minus]) {
            Ok(Expr::Unary {
                operator: op.clone(),
                right: Rc::new(self.unary()?),
            })
        } else {
            self.primary()
        }
    }

    // primary → NUMBER | STRING | "true" | "false" | "nil"
    //         | "(" expression ")" ;
    fn primary(&mut self) -> Result<Expr, String> {
        use TokenKind::*;

        if let Some(&token) = self.tokens.peek() {
            return match token.kind() {
                False => self.next_ok(Expr::Boolean(false)),
                True => self.next_ok(Expr::Boolean(true)),
                Nil => self.next_ok(Expr::Nil),
                Number => self.next_ok(Expr::Number(token.literal_number())),
                String => self.next_ok(Expr::String(token.literal_string())),
                LeftParen => {
                    self.tokens.next();

                    let expr = self.expression()?;
                    self.next_assert(RightParen, "Expect ')' after expression")?;

                    Ok(Expr::Grouping(Rc::new(expr)))
                }
                _ => Err("Not implemented yet".to_string()),
            };
        }

        Err("Not implemented yet".to_string())
    }
}
