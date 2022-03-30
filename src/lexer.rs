use crate::token::{Token, TokenKind};

pub type Error = (u32, String);

pub struct Lexer {
    src: String,
    start: u32,
    current: u32,
    lexeme: String,
    line: u32,
}

impl Lexer {
    pub fn new(src: String) -> Self {
        Self {
            src,
            start: 0,
            current: 0,
            lexeme: String::new(),
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> (Vec<Token>, Vec<Error>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        for c in self.src.chars() {
            self.start = self.current;
            self.current += 1;
            self.lexeme.push(c);

            if let Some(res) = self.scan_token(c) {
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

    fn scan_token(&self, c: char) -> Option<Result<Token, String>> {
        use TokenKind::*;

        match c {
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
            _ => Some(Err("Unexpected character".to_string())),
        }
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token::new(kind, self.lexeme.clone(), self.line)
    }
}
