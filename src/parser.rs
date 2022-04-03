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
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
    Grouping(Rc<Expr>),
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

    fn match_token(&mut self, kinds: &[TokenKind]) -> Option<&Token> {
        if let Some(&e) = self.tokens.peek() {
            if kinds.iter().any(|&k| k == e.kind()) {
                return self.tokens.next()
            }
        }

        None
    }

    fn make_binary(&mut self, kinds: &[TokenKind], gen: impl Fn(&mut Self) -> Result<Expr, String>) -> Result<Expr, String> {
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

    fn next_ok(&mut self, expr: Expr) -> Result<Expr, String> {
        self.tokens.next();

        Ok(expr)
    }

    fn next_assert(&mut self, kind: TokenKind, msg: &str) -> Result<(), String> {
        if let Some(&e) = self.tokens.peek() {
            if e.kind() == kind {
                return Ok(());
            }
        }

        Err(msg.to_string())
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
    #[inline]
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
                },
                _ => Err("Not implemented yet".to_string()),
            };
        }

        Err("Not implemented yet".to_string())
    }
}
