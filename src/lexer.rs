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
        while let Some(c) = self.advance(&mut chars) {
            if let Some(res) = self.scan_token(&mut chars, c) {
                match res {
                    Ok(token) => tokens.push(token),
                    Err(e) => errors.push((self.line, e)),
                };

                self.start = self.current;
                self.lexeme = String::new();
            }
        }

        tokens.push(Token::new(TokenKind::Eof, "".to_string(), self.line));

        (tokens, errors)
    }

    fn advance(&mut self, chars: &mut Peekable<Chars>) -> Option<char> {
        self.current += 1;

        chars.next()
    }

    fn scan_token(&mut self, chars: &mut Peekable<Chars>, c: char) -> Option<Result<Token, String>> {
        use TokenKind::*;

        match c {
            // Single character
            '(' => Some(Ok(self.scan_single(c, LeftParen))),
            ')' => Some(Ok(self.scan_single(c, RightParen))),
            '{' => Some(Ok(self.scan_single(c, LeftBrace))),
            '}' => Some(Ok(self.scan_single(c, RightBrace))),
            ',' => Some(Ok(self.scan_single(c, Comma))),
            '.' => Some(Ok(self.scan_single(c, Dot))),
            '-' => Some(Ok(self.scan_single(c, Minus))),
            '+' => Some(Ok(self.scan_single(c, Plus))),
            ';' => Some(Ok(self.scan_single(c, Semicolon))),
            '*' => Some(Ok(self.scan_single(c, Star))),

            // Operators
            '!' => Some(self.scan_operator(c, chars, Bang)),
            '=' => Some(self.scan_operator(c, chars, Equal)),
            '<' => Some(self.scan_operator(c, chars, Less)),
            '>' => Some(self.scan_operator(c, chars, Greater)),

            // Whitespace
            ' ' => None,
            '\t' => None,
            '\r' => None,
            '\n' => { self.line += 1; None },

            // Unknown
            _ => Some(Err("Unexpected character".to_string())),
        }
    }

    fn scan_single(&mut self, c: char, kind: TokenKind) -> Token {
        self.lexeme.push(c);

        self.make_token(kind)
    }

    fn scan_operator(&mut self, c: char, chars: &mut Peekable<Chars>, base: TokenKind) -> Result<Token, String> {
        use TokenKind::*;

        self.lexeme.push(c);

        if let Some('=') = chars.peek() {
            self.advance(chars);
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn single_character() {
        let pairs = [
            // Single-character operators
            ('(', TokenKind::LeftParen),
            (')', TokenKind::RightParen),
            ('{', TokenKind::LeftBrace),
            ('}', TokenKind::RightBrace),
            (',', TokenKind::Comma),
            ('.', TokenKind::Dot),
            ('-', TokenKind::Minus),
            ('+', TokenKind::Plus),
            (';', TokenKind::Semicolon),
            ('*', TokenKind::Star),

            // Operators followed by EOF
            ('!', TokenKind::Bang),
            ('=', TokenKind::Equal),
            ('<', TokenKind::Less),
            ('>', TokenKind::Greater),
        ];

        for (c, kind) in pairs {
            let mut lexer = Lexer::new();
            let (tokens, errors) = lexer.scan_tokens(c.to_string());

            assert_eq!(errors.len(), 0);
            assert_eq!(tokens.len(), 2);
            
            let token = &tokens[0];
            
            assert_eq!(token.kind, kind);
        }
    }
}
