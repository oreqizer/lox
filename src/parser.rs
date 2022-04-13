use std::{iter::Peekable, slice::Iter};

use crate::error::Error;
use crate::lexer::{Token, TokenKind};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Boolean(bool),
    Call {
        callee: Box<Expr>,
        paren: Token,
        args: Vec<Expr>,
    },
    Grouping(Box<Expr>),
    Lambda {
        fun: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Nil,
    Number(f64),
    String(String),
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Variable(Token),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Expr(Expr),
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
    },
    If {
        cond: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Print(Expr),
    Return(Expr),
    VarDecl {
        name: String,
        value: Option<Expr>,
    },
    While {
        cond: Expr,
        body: Box<Stmt>,
    },
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

    // declaration → funDecl
    //             | varDecl
    //             | statement ;
    fn declaration(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        match self.match_token(&[Fun, Var]).map(|t| t.kind()) {
            Some(Fun) => self.fun_decl(),
            Some(Var) => self.var_decl(),
            _ => self.statement(),
        }
    }

    // funDecl → "fun" function ;
    fn fun_decl(&mut self) -> Result<Stmt, Error> {
        // "fun" already matched
        self.make_function("function")
    }

    // varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn var_decl(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        // "var" already matched
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
    //           | forStmt
    //           | ifStmt
    //           | whileStmt
    //           | returnStmt
    //           | printStmt
    //           | block ;
    fn statement(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        match self
            .match_token(&[For, If, Return, Print, While, LeftBrace])
            .map(|t| t.kind())
        {
            Some(For) => self.for_stmt(),
            Some(If) => self.if_stmt(),
            Some(While) => self.while_stmt(),
            Some(Return) => self.return_stmt(),
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

        // "if" already matched
        self.next_assert(LeftParen, "Expect '(' after if")?;
        let cond = self.expression()?;
        self.next_assert(RightParen, "Expect ')' after condition")?;

        let then_branch = Box::new(self.statement()?);
        let else_branch = match self.match_token(&[Else]) {
            Some(_) => Some(Box::new(self.statement()?)),
            None => None,
        };

        Ok(Stmt::If {
            cond,
            then_branch,
            else_branch,
        })
    }

    // forStmt → "for" "(" ( varDecl | exprStmt | ";" )
    //            expression? ";"
    //            expression? ")" statement ;
    fn for_stmt(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        // "for" already matched
        self.next_assert(LeftParen, "Expect '(' after for")?;
        let init = match self.match_token(&[Semicolon, Var]).map(|t| t.kind()) {
            Some(Semicolon) => None,
            Some(Var) => Some(self.var_decl()?),
            _ => Some(self.expr_stmt()?),
        };

        let cond = match self.tokens.peek().map(|t| t.kind()) {
            Some(Semicolon) => Expr::Boolean(true),
            _ => self.expression()?,
        };
        self.next_assert(Semicolon, "Expect ';' after loop condition")?;

        let incr = match self.tokens.peek().map(|t| t.kind()) {
            Some(RightParen) => None,
            _ => Some(self.expression()?),
        };
        self.next_assert(RightParen, "Expect ')' after loop increment")?;
        let mut body = self.statement()?;

        if let Some(e) = incr {
            body = Stmt::Block(vec![body, Stmt::Expr(e)]);
        }

        body = Stmt::Block(vec![Stmt::While {
            cond,
            body: Box::new(body),
        }]);

        if let Some(s) = init {
            body = Stmt::Block(vec![s, body]);
        }

        Ok(body)
    }

    // whileStmt -> "while" "(" expression ")" statement ;
    fn while_stmt(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        // "while" already matched
        self.next_assert(LeftParen, "Expect '(' after while")?;
        let cond = self.expression()?;
        self.next_assert(RightParen, "Expect ')' after condition")?;
        let body = Box::new(self.statement()?);

        Ok(Stmt::While { cond, body })
    }

    // returnStmt -> "return" expression? ";" ;
    fn return_stmt(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        // "return" already matched
        let expr = match self.next_peek() {
            Some(Semicolon) => Expr::Nil,
            _ => self.expression()?,
        };
        self.next_assert(Semicolon, "Expect ';' after return")?;

        Ok(Stmt::Return(expr))
    }

    // printStmt → "print" expression ";" ;
    fn print_stmt(&mut self) -> Result<Stmt, Error> {
        use TokenKind::*;

        // "print" already matched
        let expr = self.expression()?;
        self.next_assert(Semicolon, "Expect ';' after value")?;

        Ok(Stmt::Print(expr))
    }

    // block → "{" declaration* "}" ;
    fn block(&mut self) -> Result<Stmt, Error> {
        // "{" already matched
        Ok(Stmt::Block(self.make_block()?))
    }

    // expression → assignment ;
    fn expression(&mut self) -> Result<Expr, Error> {
        self.assignment()
    }

    // assignment → IDENTIFIER "=" assignment
    //            | logic_or ;
    fn assignment(&mut self) -> Result<Expr, Error> {
        use TokenKind::*;

        let expr = self.logic_or()?;

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

    // logic_or → logic_and ( "or" logic_and )* ;
    fn logic_or(&mut self) -> Result<Expr, Error> {
        use TokenKind::*;

        let mut expr = self.logic_and()?;

        while let Some(t) = self.match_token(&[Or]) {
            expr = Expr::Logical {
                left: Box::new(expr),
                operator: t.clone(),
                right: Box::new(self.logic_and()?),
            }
        }

        Ok(expr)
    }

    // logic_and → equality ( "and" equality )* ;
    fn logic_and(&mut self) -> Result<Expr, Error> {
        use TokenKind::*;

        let mut expr = self.equality()?;

        while let Some(t) = self.match_token(&[And]) {
            expr = Expr::Logical {
                left: Box::new(expr),
                operator: t.clone(),
                right: Box::new(self.equality()?),
            }
        }

        Ok(expr)
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
    //       | call ;
    fn unary(&mut self) -> Result<Expr, Error> {
        use TokenKind::*;

        if let Some(op) = self.match_token(&[Bang, Minus]) {
            Ok(Expr::Unary {
                operator: op.clone(),
                right: Box::new(self.unary()?),
            })
        } else {
            self.call()
        }
    }

    // call      → primary ( "(" arguments? ")" )* ;
    // arguments → expression ( "," expression )* ;
    fn call(&mut self) -> Result<Expr, Error> {
        use TokenKind::*;

        let mut expr = self.primary()?;

        loop {
            match self.match_token(&[LeftParen]) {
                Some(t) => {
                    let paren = t.clone();
                    let mut args = Vec::new();

                    if !self.next_check(RightParen) {
                        args.push(self.expression()?);

                        if args.len() >= 255 {
                            // FIXME:
                            // don't bail here, just report error, need a way for non-fatal errors,
                            // like self.errors.push(Error)
                            return Err(Error::new(
                                "Can't have more than 255 arguments",
                                self.tokens.peek().map(|t| t.offset()).unwrap_or_default(),
                            ));
                        }

                        while let Some(Comma) = self.match_token(&[Comma]).map(|t| t.kind()) {
                            args.push(self.expression()?);
                        }
                    }

                    self.next_assert(RightParen, "Expect ')' after argument list")?;

                    expr = Expr::Call {
                        callee: Box::new(expr),
                        paren,
                        args,
                    };
                }
                None => break,
            }
        }

        Ok(expr)
    }

    // primary → "true" | "false" | "nil"
    //         | NUMBER | STRING
    //         | "fun" functionDecl
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
                Fun => {
                    self.tokens.next();

                    let (params, body) = self.make_function_decl("lambda")?;

                    Ok(Expr::Lambda { fun: token.clone(), params, body })
                }
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

    // function   → IDENTIFIER functionDecl ;
    // parameters → IDENTIFIER ( "," IDENTIFIER )* ;
    #[inline]
    fn make_function(&mut self, kind: &str) -> Result<Stmt, Error> {
        use TokenKind::*;

        let name = self
            .next_assert(Identifier, &format!("Expect {kind} name"))?
            .clone();

        let (params, body) = self.make_function_decl(kind)?;

        Ok(Stmt::Function { name, params, body })
    }

    // functionDecl → "(" parameters? ")" block
    #[inline]
    fn make_function_decl(&mut self, kind: &str) -> Result<(Vec<Token>, Vec<Stmt>), Error> {
        use TokenKind::*;

        self.next_assert(LeftParen, &format!("Expect '(' after {kind} name"))?;

        let mut params = Vec::new();
        if !self.next_check(RightParen) {
            params.push(
                self.next_assert(Identifier, "Expect parameter name")?
                    .clone(),
            );

            while let Some(t) = self.match_token(&[Comma]) {
                match t.kind() {
                    Comma => {
                        params.push(
                            self.next_assert(Identifier, "Expect parameter name")?
                                .clone(),
                        );
                    }
                    _ => break,
                }

                if params.len() >= 255 {
                    return Err(Error::new(
                        "Can't have more than 255 parameters",
                        self.tokens.peek().map(|t| t.offset()).unwrap_or_default(),
                    ));
                }
            }
        }

        self.next_assert(RightParen, "Expect ')' after parameters")?;
        self.next_assert(LeftBrace, &format!("Expect '{{' before {kind} body"))?;

        let body = match self.block()? {
            Stmt::Block(v) => Ok(v),
            _ => Err(Error::new(
                "Expect block",
                self.tokens.peek().map(|t| t.offset()).unwrap_or_default(),
            )),
        }?;

        Ok((params, body))
    }

    #[inline]
    fn next_ok(&mut self, expr: Expr) -> Result<Expr, Error> {
        self.tokens.next();

        Ok(expr)
    }

    #[inline]
    fn next_peek(&mut self) -> Option<TokenKind> {
        self.tokens.peek().map(|t| t.kind())
    }

    #[inline]
    fn next_check(&mut self, kind: TokenKind) -> bool {
        match self.tokens.peek() {
            Some(t) => t.kind() == kind,
            None => false,
        }
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
