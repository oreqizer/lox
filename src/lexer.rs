use std::{str::Chars, iter::Peekable};

use crate::token::{Token, TokenKind};

pub type Error = (u32, String);

pub struct Lexer {
    lexeme: String,
    line: u32,
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            lexeme: String::new(),
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self, src: String) -> (Vec<Token>, Vec<Error>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        let mut chars = src.chars().into_iter().peekable();
        while let Some(c) = chars.next() {
            if let Some(res) = self.scan_token(&mut chars, c) {
                match res {
                    Ok(token) => tokens.push(token),
                    Err(e) => errors.push((self.line, e)),
                };

                self.lexeme.clear();
            }
        }

        tokens.push(Token::new(TokenKind::Eof, "".to_string(), self.line));

        (tokens, errors)
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

            // Division / Comment
            '/' => self.scan_slash(chars),

            // String
            '"' => Some(self.scan_string(chars)),

            // Whitespace
            ' ' | '\t' | '\r' => None,
            '\n' => { self.line += 1; None },

            // Number
            '0'..='9' => Some(Ok(self.scan_number(chars, c))),

            // Identifier
            'a'..='z' | 'A'..='Z' | '_' => Some(Ok(self.scan_identifier(chars, c))),

            // Unknown
            _ => Some(Err("Unexpected character".to_string())),
        }
    }

    fn write_next(&mut self, chars: &mut Peekable<Chars>) {
        if let Some(c) = chars.next() {
            self.lexeme.push(c);
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
            self.write_next(chars);

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

    fn scan_slash(&mut self, chars: &mut Peekable<Chars>) -> Option<Result<Token, String>> {
        use TokenKind::*;

        if let Some('/') = chars.peek() {
            chars.next();

            while let Some(&c) = chars.peek() {
                if c == '\n' {
                    break;
                }

                chars.next();
            }
            None
        } else {
            self.lexeme.push('/');

            Some(Ok(self.make_token(Slash)))
        }
    }

    fn scan_string(&mut self, chars: &mut Peekable<Chars>) -> Result<Token, String> {
        use TokenKind::*;

        self.lexeme.push('"');
        let mut value = "".to_string();

        while let Some(&c) = chars.peek() {
            self.write_next(chars);

            if c == '"' {
                return Ok(self.make_token(String(value)));
            }

            value.push(c);
        }

        Err("Unterminated string".to_string())
    }

    fn scan_number(&mut self, chars: &mut Peekable<Chars>, c: char) -> Token {
        use TokenKind::*;

        self.lexeme.push(c);

        while let Some(&c) = chars.peek() {
            match c {
                '0'..='9' | '.' => self.write_next(chars),
                _ => break,
            }
        }

        self.make_token(Number(self.lexeme.parse().unwrap()))
    }

    fn scan_identifier(&mut self, chars: &mut Peekable<Chars>, c: char) -> Token {
        use TokenKind::*;

        self.lexeme.push(c);

        while let Some(&c) = chars.peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => self.write_next(chars),
                _ => break,
            }
        }

        let kind = match self.lexeme.as_str() {
            // Keywords
            "and" => And,
            "class" => Class,
            "else" => Else,
            "false" => False,
            "fun" => Fun,
            "for" => For,
            "if" => If,
            "nil" => Nil,
            "or" => Or,
            "print" => Print,
            "return" => Return,
            "super" => Super,
            "this" => This,
            "true" => True,
            "var" => Var,
            "while" => While,
            _ => Identifier,
        };

        self.make_token(kind)
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
            
            assert_eq!(token.line, 1);
            assert_eq!(token.kind, kind);
            assert_eq!(token.lexeme, c.to_string());
        }
    }

    #[test]
    fn double_character() {
        let pairs = [
            ("!=", TokenKind::BangEqual),
            ("==", TokenKind::EqualEqual),
            ("<=", TokenKind::LessEqual),
            (">=", TokenKind::GreaterEqual),
        ];

        for (s, kind) in pairs {
            let mut lexer = Lexer::new();
            let (tokens, errors) = lexer.scan_tokens(s.to_string());

            assert_eq!(errors.len(), 0);
            assert_eq!(tokens.len(), 2);
            
            let token = &tokens[0];
            
            assert_eq!(token.line, 1);
            assert_eq!(token.kind, kind);
            assert_eq!(token.lexeme, s);
        }
    }

    #[test]
    fn division() {
        let src = "/";

        let expected = vec![
            Token::new(TokenKind::Slash, "/".to_string(), 1),
            Token::new(TokenKind::Eof, "".to_string(), 1),
        ];

        let mut lexer = Lexer::new();
        let (tokens, errors) = lexer.scan_tokens(src.to_string());

        assert_eq!(errors.len(), 0);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn comment() {
        let src = "// lol omg wtf";

        let expected = vec![
            Token::new(TokenKind::Eof, "".to_string(), 1),
        ];

        let mut lexer = Lexer::new();
        let (tokens, errors) = lexer.scan_tokens(src.to_string());

        assert_eq!(errors.len(), 0);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn string() {
        let src = r#""raw string""#;

        let expected = vec![
            Token::new(TokenKind::String("raw string".to_string()), r#""raw string""#.to_string(), 1),
            Token::new(TokenKind::Eof, "".to_string(), 1),
        ];

        let mut lexer = Lexer::new();
        let (tokens, errors) = lexer.scan_tokens(src.to_string());

        assert_eq!(errors.len(), 0);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn string_unterminated() {
        let src = r#""bad string"#;

        let expected = vec![
            Token::new(TokenKind::Eof, "".to_string(), 1),
        ];
        let expected_errors = vec![
            (1, "Unterminated string".to_string()),
        ];

        let mut lexer = Lexer::new();
        let (tokens, errors) = lexer.scan_tokens(src.to_string());

        assert_eq!(errors, expected_errors);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn number_int() {
        let src = "1337";

        let expected = vec![
            Token::new(TokenKind::Number(1337.0), "1337".to_string(), 1),
            Token::new(TokenKind::Eof, "".to_string(), 1),
        ];

        let mut lexer = Lexer::new();
        let (tokens, errors) = lexer.scan_tokens(src.to_string());

        assert_eq!(errors.len(), 0);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn number_float() {
        let src = "13.37";

        let expected = vec![
            Token::new(TokenKind::Number(13.37), "13.37".to_string(), 1),
            Token::new(TokenKind::Eof, "".to_string(), 1),
        ];

        let mut lexer = Lexer::new();
        let (tokens, errors) = lexer.scan_tokens(src.to_string());

        assert_eq!(errors.len(), 0);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn number_dangling_dot() {
        let src = "1337.";

        let expected = vec![
            Token::new(TokenKind::Number(1337.0), "1337.".to_string(), 1),
            Token::new(TokenKind::Eof, "".to_string(), 1),
        ];

        let mut lexer = Lexer::new();
        let (tokens, errors) = lexer.scan_tokens(src.to_string());

        assert_eq!(errors.len(), 0);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn identifiers() {
        let pairs = [
            // Keywords
            ("and", TokenKind::And),
            ("class", TokenKind::Class),
            ("else", TokenKind::Else),
            ("false", TokenKind::False),
            ("fun", TokenKind::Fun),
            ("for", TokenKind::For),
            ("if", TokenKind::If),
            ("nil", TokenKind::Nil),
            ("or", TokenKind::Or),
            ("print", TokenKind::Print),
            ("return", TokenKind::Return),
            ("super", TokenKind::Super),
            ("this", TokenKind::This),
            ("true", TokenKind::True),
            ("var", TokenKind::Var),
            ("while", TokenKind::While),

            // Identifiers
            ("_", TokenKind::Identifier),
            ("a", TokenKind::Identifier),
            ("A", TokenKind::Identifier),
            ("_asd_", TokenKind::Identifier),
            ("_as1347d", TokenKind::Identifier),
            ("as1347d", TokenKind::Identifier),
            ("As1347d", TokenKind::Identifier),
        ];

        for (s, kind) in pairs {
            let mut lexer = Lexer::new();
            let (tokens, errors) = lexer.scan_tokens(s.to_string());

            assert_eq!(errors.len(), 0);
            assert_eq!(tokens.len(), 2);
            
            let token = &tokens[0];
            
            assert_eq!(token.line, 1);
            assert_eq!(token.kind, kind);
            assert_eq!(token.lexeme, s.to_string());
        }
    }
}
