use std::{str::Chars, iter::Peekable};

use crate::token::{Token, TokenKind};

pub type Error = (u32, String);

pub struct Lexer {
    start: u32,
    current: u32,
    lexeme: String,
    line: u32,
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            start: 0,
            current: 0,
            lexeme: String::new(),
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self, src: String) -> (Vec<Token>, Vec<Error>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        let mut chars = src.chars().into_iter().peekable();
        while let Some(c) = chars.next() {
            self.start = self.current;
            self.current += 1;
            self.lexeme.push(c);

            if let Some(res) = self.scan_token(&mut chars, c) {
                match res {
                    Ok(token) => tokens.push(token),
                    Err(e) => errors.push((self.line, e)),
                };

                self.lexeme = String::new();
            }
        }

        tokens.push(Token::new(TokenKind::Eof, "".to_string(), self.line));

        (tokens, errors)
    }

    fn scan_token(&mut self, chars: &mut Peekable<Chars>, c: char) -> Option<Result<Token, String>> {
        use TokenKind::*;

        match c {
            // Single character
            '(' => Some(Ok(self.make_token(LeftParen))),
            ')' => Some(Ok(self.make_token(RightParen))),
            '{' => Some(Ok(self.make_token(LeftBrace))),
            '}' => Some(Ok(self.make_token(RightBrace))),
            ',' => Some(Ok(self.make_token(Comma))),
            '.' => Some(Ok(self.make_token(Dot))),
            '-' => Some(Ok(self.make_token(Minus))),
            '+' => Some(Ok(self.make_token(Plus))),
            ';' => Some(Ok(self.make_token(Semicolon))),
            '*' => Some(Ok(self.make_token(Star))),

            // Operators
            '!' => Some(self.scan_operator(chars, Bang)),
            '=' => Some(self.scan_operator(chars, Equal)),
            '>' => Some(self.scan_operator(chars, Less)),
            '<' => Some(self.scan_operator(chars, Greater)),

            // Whitespace
            ' ' => None,
            '\t' => None,
            '\r' => None,
            '\n' => { self.line += 1; None },

            // Unknown
            _ => Some(Err("Unexpected character".to_string())),
        }
    }

    fn scan_operator(&mut self, chars: &mut Peekable<Chars>, base: TokenKind) -> Result<Token, String> {
        use TokenKind::*;

        if let Some('=') = chars.peek() {
            chars.next();

            self.current += 1;
            self.lexeme.push('=');

            match base {
                Bang => Ok(self.make_token(BangEqual)),
                Equal => Ok(self.make_token(EqualEqual)),
                Less => Ok(self.make_token(LessEqual)),
                Greater => Ok(self.make_token(GreaterEqual)),
                _ => Err("Unexpected token kind".to_string()),
            }
        } else {
            Ok(self.make_token(base))
        }
    }

    fn scan_identifier(&mut self, chars: &mut Chars, c: char) -> Result<Token, String> {
        use TokenKind::*;

        match c {
            _ => Err("".to_string()),
        }
    }

    fn scan_number(&mut self, chars: &mut Chars, c: char) -> Result<Token, String> {
        use TokenKind::*;

        match c {
            _ => Err("".to_string()),
        }
    }

    fn scan_string(&mut self, chars: &mut Chars, c: char) -> Result<Token, String> {
        use TokenKind::*;

        match c {
            _ => Err("".to_string()),
        }
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token::new(kind, self.lexeme.clone(), self.line)
    }
}
