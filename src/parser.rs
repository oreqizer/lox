use std::{fmt, iter::Peekable, slice::Iter};

use crate::error::Error;
use crate::lexer::{Token, TokenKind};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Variable(Token),
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
    Grouping(Box<Expr>),
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;

        match self {
            Variable(ref s) => write!(f, "{}", s),
            Assign { ref name, value } => write!(f, "{} = {}", name, value),
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

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    If { cond: Expr, then_branch: Box<Stmt>, else_branch: Option<Box<Stmt>> },
    Print(Expr),
    VarDecl { name: String, value: Option<Expr> },
    Block(Vec<Stmt>),
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

    // program → declaration* EOF ;
    pub fn parse(&mut self) -> (Vec<Stmt>, Vec<Error>) {
        let mut stmts = Vec::new();
        let mut errors = Vec::new();

        while let Some(&token) = self.tokens.peek() {
            if token.kind() == TokenKind::Eof {
                break;
            }

            match self.declaration() {
                Ok(s) => stmts.push(s),
                Err(e) => {
                    errors.push(e);
                    self.synchronize()
                }
            };
        }

        (stmts, errors)
    }

    // program → expression? EOF ;
    pub fn parse_expr(&mut self) -> Result<Expr, Error> {
        let expr = self.expression()?;
        self.next_assert(TokenKind::Eof, "Expect EOF")?;
        Ok(expr)
    }

    // declaration → varDecl
    //             | statement ;
    fn declaration(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        if let Some(_) = self.match_token(&[Var]) {
            self.var_decl()
        } else {
            self.statement()
        }
    }

    // varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn var_decl(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        // "var" already matched in Parser::declaration
        let name = self
            .next_assert(Identifier, "Expect identifier after 'var'")?
            .literal_identifier()
            .to_string();

        let value = match self.match_token(&[Equal]) {
            Some(_) => Some(self.expression()?),
            None => None,
        };

        self.next_assert(Semicolon, "Expect ';' after value")?;

        Ok(Stmt::VarDecl { name, value })
    }

    // statement → exprStmt
    //           | ifStmt
    //           | printStmt
    //           | block ;
    fn statement(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        match self.match_token(&[If, Print, LeftBrace]).map(|t| t.kind()) {
            Some(If) => self.if_stmt(),
            Some(Print) => self.print_stmt(),
            Some(LeftBrace) => self.block(),
            _ => self.expr_stmt(),
        }
    }

    // exprStmt → expression ";" ;
    fn expr_stmt(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        let expr = self.expression()?;
        self.next_assert(Semicolon, "Expect ';' after value")?;

        Ok(Stmt::Expr(expr))
    }

    // ifStmt → "if" "(" expression ")" statement
    //        ( "else" statement )? ;
    fn if_stmt(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        // "if" already matched in Parser::statement
        self.next_assert(LeftParen, "Expect '(' after if")?;
        let cond = self.expression()?;
        self.next_assert(RightParen, "Expect ')' after condition")?;

        let then_branch = Box::new(self.statement()?);
        let else_branch = match self.match_token(&[Else]) {
            Some(_) => Some(Box::new(self.statement()?)),
            None => None,
        };

        Ok(Stmt::If { cond, then_branch, else_branch })
    }

    // printStmt → "print" expression ";" ;
    fn print_stmt(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        // "print" already matched in Parser::statement
        let expr = self.expression()?;
        self.next_assert(Semicolon, "Expect ';' after value")?;

        Ok(Stmt::Print(expr))
    }

    // block → "{" declaration* "}" ;
    fn block(&mut self) -> Result<Stmt, Error> {
        // "{" already matched in Parser::statement
        Ok(Stmt::Block(self.make_block()?))
    }

    // expression → assignment ;
    fn expression(&mut self) -> Result<Expr, Error> {
        self.assignment()
    }

    // assignment → IDENTIFIER "=" assignment
    //            | equality ;
    fn assignment(&mut self) -> Result<Expr, Error> {
        use TokenKind::*;

        let expr = self.equality()?;

        if let Some(eq) = self.match_token(&[Equal]) {
            let offset = eq.offset();
            let value = self.assignment()?;

            match expr {
                Expr::Variable(t) => Ok(Expr::Assign {
                    name: t,
                    value: Box::new(value),
                }),
                // Compile-time error on purpose, opposed to the book
                _ => Err(Error::new("Invalid assignment target", offset)),
            }
        } else {
            Ok(expr)
        }
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Expr, Error> {
        use TokenKind::*;

        self.make_binary(&[BangEqual, EqualEqual], Parser::comparison)
    }

    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Expr, Error> {
        use TokenKind::*;

        self.make_binary(&[Greater, GreaterEqual, Less, LessEqual], Parser::term)
    }

    // term → factor ( ( "+" | "-" ) factor )* ;
    fn term(&mut self) -> Result<Expr, Error> {
        use TokenKind::*;

        self.make_binary(&[Plus, Minus], Parser::factor)
    }

    // factor → unary ( ( "*" | "/" ) unary )* ;
    fn factor(&mut self) -> Result<Expr, Error> {
        use TokenKind::*;

        self.make_binary(&[Star, Slash], Parser::unary)
    }

    // unary → ( "!" | "-" ) unary
    //       | primary ;
    fn unary(&mut self) -> Result<Expr, Error> {
        use TokenKind::*;

        if let Some(op) = self.match_token(&[Bang, Minus]) {
            Ok(Expr::Unary {
                operator: op.clone(),
                right: Box::new(self.unary()?),
            })
        } else {
            self.primary()
        }
    }

    // primary → "true" | "false" | "nil"
    //         | NUMBER | STRING
    //         | "(" expression ")"
    //         | IDENTIFIER ;
    fn primary(&mut self) -> Result<Expr, Error> {
        use TokenKind::*;

        if let Some(&token) = self.tokens.peek() {
            return match token.kind() {
                True => self.next_ok(Expr::Boolean(true)),
                False => self.next_ok(Expr::Boolean(false)),
                Nil => self.next_ok(Expr::Nil),
                Number => self.next_ok(Expr::Number(token.literal_number())),
                String => self.next_ok(Expr::String(token.literal_string().to_string())),
                LeftParen => {
                    self.tokens.next();

                    let expr = self.expression()?;
                    self.next_assert(RightParen, "Expect ')' after expression")?;

                    Ok(Expr::Grouping(Box::new(expr)))
                }
                Identifier => self.next_ok(Expr::Variable(token.clone())),
                _ => Err(Error::new("Unexpected token", token.offset())),
            };
        }

        Err(Error::new("Unexpected EOF", 0))
    }

    // HELPERS
    // =======

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
        gen: impl Fn(&mut Self) -> Result<Expr, Error>,
    ) -> Result<Expr, Error> {
        let mut expr = gen(self)?;

        while let Some(op) = self.match_token(kinds) {
            expr = Expr::Binary {
                left: Box::new(expr),
                operator: op.clone(),
                right: Box::new(gen(self)?),
            };
        }

        Ok(expr)
    }

    #[inline]
    fn make_block(&mut self) -> Result<Vec<Stmt>, Error> {
        use TokenKind::*;

        let mut stmts = Vec::new();

        while let Some(&token) = self.tokens.peek() {
            match token.kind() {
                RightBrace => break,
                _ => stmts.push(self.declaration()?),
            }
        }

        self.next_assert(RightBrace, "Expect '}' after block")?;
        Ok(stmts)
    }

    #[inline]
    fn next_ok(&mut self, expr: Expr) -> Result<Expr, Error> {
        self.tokens.next();

        Ok(expr)
    }

    #[inline]
    fn next_assert(&mut self, kind: TokenKind, msg: &str) -> Result<&Token, Error> {
        if let Some(&e) = self.tokens.peek() {
            if e.kind() == kind {
                return Ok(self.tokens.next().unwrap());
            }

            return Err(Error::new(msg, e.offset()));
        }

        Err(Error::new(
            "Unexpected end of input",
            self.tokens.clone().last().map_or(0, |t| t.offset()),
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Literal;

    use super::*;

    #[test]
    fn primary_primitive() {
        let inputs = [
            ((TokenKind::False, None), Expr::Boolean(false)),
            ((TokenKind::True, None), Expr::Boolean(true)),
            ((TokenKind::Nil, None), Expr::Nil),
            (
                (TokenKind::Number, Some(Literal::Number(1337.0))),
                Expr::Number(1337.0),
            ),
            (
                (TokenKind::String, Some(Literal::String("kek".to_string()))),
                Expr::String("kek".to_string()),
            ),
        ];

        for ((kind, literal), want) in inputs.into_iter() {
            let tokens = [
                Token::new(kind, literal, "".to_string(), 0),
                Token::new(TokenKind::Semicolon, None, "".to_string(), 0),
            ];
            let mut parser = Parser::new(&tokens);
            let (stmts, errors) = parser.parse();

            assert_eq!(errors.len(), 0);
            assert_eq!(stmts, vec![Stmt::Expr(want)]);
        }
    }

    #[test]
    fn primary_grouping() {
        let tokens = [
            Token::new(TokenKind::LeftParen, None, "".to_string(), 0),
            Token::new(TokenKind::Nil, None, "".to_string(), 0),
            Token::new(TokenKind::RightParen, None, "".to_string(), 0),
            Token::new(TokenKind::Semicolon, None, "".to_string(), 0),
        ];

        let mut parser = Parser::new(&tokens);
        let (stmts, errors) = parser.parse();

        assert_eq!(errors.len(), 0);
        assert_eq!(stmts, vec![Stmt::Expr(Expr::Grouping(Box::new(Expr::Nil)))]);
    }

    // #[test]
    // fn primary_grouping_unclosed() {
    //     todo!();
    // }
}
