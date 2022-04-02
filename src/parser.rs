use std::{iter::Peekable, rc::Rc, slice::Iter};

use crate::lexer::{Token, TokenKind};

#[derive(Debug, PartialEq)]
pub enum Expr {
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

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
        }
    }

    // HELPERS
    // =======

    fn make_binary(&mut self, kinds: &[TokenKind], gen: impl Fn(&mut Self) -> Result<Expr, String>) -> Result<Expr, String> {
        let mut expr = gen(self)?;

        while let Some(&e) = self.tokens.peek() {
            if !kinds.iter().any(|k| k == e.kind()) {
                break;
            }

            if let Some(op) = self.tokens.next() {
                expr = Expr::Binary {
                    left: Rc::new(expr),
                    operator: op.clone(),
                    right: Rc::new(gen(self)?),
                };
            }
        }

        Ok(expr)
    }

    // GRAMMAR
    // =======

    // expression → equality;
    #[inline]
    fn expression(&mut self) -> Result<Expr, String> {
        return self.equality();
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    #[inline]
    fn equality(&mut self) -> Result<Expr, String> {
        use TokenKind::*;

        self.make_binary(&[BangEqual, EqualEqual], Parser::comparison)
    }

    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    #[inline]
    fn comparison(&mut self) -> Result<Expr, String> {
        use TokenKind::*;

        self.make_binary(&[Greater, GreaterEqual, Less, LessEqual], Parser::term)
    }

    // term → factor ( ( "+" | "-" ) factor )* ;
    #[inline]
    fn term(&mut self) -> Result<Expr, String> {
        use TokenKind::*;

        self.make_binary(&[Plus, Minus], Parser::factor)
    }

    // factor → unary ( ( "*" | "/" ) unary )* ;
    #[inline]
    fn factor(&mut self) -> Result<Expr, String> {
        use TokenKind::*;

        self.make_binary(&[Star, Slash], Parser::unary)
    }

    // unary → ( "!" | "-" ) unary
    //       | primary ;
    #[inline]
    fn unary(&mut self) -> Result<Expr, String> {
        use TokenKind::*;

        while let Some(&e) = self.tokens.peek() {
            let op = match e.kind() {
                Bang | Minus => self.tokens.next(),
                _ => None,
            };

            if let Some(op) = op {
                return Ok(Expr::Unary {
                    operator: op.clone(),
                    right: Rc::new(self.unary()?),
                })
            }
        }

        self.primary()
    }

    // primary → NUMBER | STRING | "true" | "false" | "nil"
    //         | "(" expression ")" ;
    #[inline]
    fn primary(&mut self) -> Result<Expr, String> {
        use TokenKind::*;

        Err("not implemented".to_string())
    }
}
