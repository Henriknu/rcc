use std::str::Chars;

use crate::token::{Token, TokenKind, TokenSpan};

pub struct Lexer<'s> {
    source: &'s str,
    chars: Chars<'s>,
    start: usize,
    current: usize,
}

impl<'s> Lexer<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            source,
            chars: source.chars(),
            start: 0,
            current: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        self.start = self.current;

        let Some(next) = self.consume() else {
            return self.token(TokenKind::Eof);
        };

        match next {
            '(' => self.token(TokenKind::LParen),
            ')' => self.token(TokenKind::RParen),
            '{' => self.token(TokenKind::LBrace),
            '}' => self.token(TokenKind::RBrace),
            ',' => self.token(TokenKind::Comma),
            ';' => self.token(TokenKind::Semicolon),
            '=' => {
                if let Some('=') = self.peek() {
                    self.consume();
                    self.token(TokenKind::EqualsEquals)
                } else {
                    self.token(TokenKind::Equals)
                }
            }
            '!' => {
                if let Some('=') = self.peek() {
                    self.consume();
                    self.token(TokenKind::NotEquals)
                } else {
                    self.token(TokenKind::Bang)
                }
            }
            '>' => {
                if let Some('=') = self.peek() {
                    self.consume();
                    self.token(TokenKind::GreaterOrEquals)
                } else {
                    self.token(TokenKind::Greater)
                }
            }
            '<' => {
                if let Some('=') = self.peek() {
                    self.consume();
                    self.token(TokenKind::LessOrEquals)
                } else {
                    self.token(TokenKind::Less)
                }
            }
            '+' => self.token(TokenKind::Plus),
            '-' => self.token(TokenKind::Minus),
            '*' => self.token(TokenKind::Star),
            '/' => self.token(TokenKind::Slash),

            c if c.is_ascii_digit() => self.number(),
            c if c.is_ascii_alphabetic() || c == '_' => self.identifier(),
            _ => self.token(TokenKind::Invalid),
        }
    }

    fn identifier(&mut self) -> Token {
        while let Some(next) = self.peek() {
            if !next.is_ascii_alphanumeric() && next != '_' {
                break;
            }
            self.consume();
        }

        if let Some(keyword) = self.keyword() {
            return keyword;
        }

        self.token(TokenKind::Ident)
    }

    fn keyword(&mut self) -> Option<Token> {
        let token_type = match &self.source[self.start..self.current] {
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "print" => TokenKind::Print,
            "int" => TokenKind::Int,
            _ => return None,
        };

        Some(self.token(token_type))
    }

    fn number(&mut self) -> Token {
        while let Some(next) = self.peek() {
            if !next.is_ascii_digit() {
                break;
            }
            self.consume();
        }

        self.token(TokenKind::Number)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if !c.is_ascii_whitespace() {
                break;
            }
            self.consume();
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn consume(&mut self) -> Option<char> {
        if let Some(next) = self.chars.next() {
            self.current += 1;
            Some(next)
        } else {
            None
        }
    }

    fn token(&mut self, kind: TokenKind) -> Token {
        let token = Token::new(kind, TokenSpan::new(self.start, self.current - self.start));

        self.start = self.current;

        token
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn add() {
        const SOURCE: &str = "4 + 4";
        let mut lexer = Lexer::new(SOURCE);

        assert_next_token_with_literal(&mut lexer, TokenKind::Number, "4", SOURCE);
        assert_next_token(&mut lexer, TokenKind::Plus);
        assert_next_token_with_literal(&mut lexer, TokenKind::Number, "4", SOURCE);
    }

    #[test]
    fn cmp() {
        const SOURCE: &str = "5 >= 4";
        let mut lexer = Lexer::new(SOURCE);

        assert_next_token_with_literal(&mut lexer, TokenKind::Number, "5", SOURCE);
        assert_next_token(&mut lexer, TokenKind::GreaterOrEquals);
        assert_next_token_with_literal(&mut lexer, TokenKind::Number, "4", SOURCE);
    }

    #[test]
    fn if_else() {
        const SOURCE: &str = "int a; if(5 == 5) a = 5; ";
        let mut lexer = Lexer::new(SOURCE);

        assert_next_token(&mut lexer, TokenKind::Int);
        assert_next_token_with_literal(&mut lexer, TokenKind::Ident, "a", SOURCE);
        assert_next_token(&mut lexer, TokenKind::Semicolon);
        assert_next_token(&mut lexer, TokenKind::If);
        assert_next_token(&mut lexer, TokenKind::LParen);
        assert_next_token_with_literal(&mut lexer, TokenKind::Number, "5", SOURCE);
        assert_next_token(&mut lexer, TokenKind::EqualsEquals);
        assert_next_token_with_literal(&mut lexer, TokenKind::Number, "5", SOURCE);
        assert_next_token(&mut lexer, TokenKind::RParen);
        assert_next_token_with_literal(&mut lexer, TokenKind::Ident, "a", SOURCE);
        assert_next_token(&mut lexer, TokenKind::Equals);
        assert_next_token_with_literal(&mut lexer, TokenKind::Number, "5", SOURCE);
        assert_next_token(&mut lexer, TokenKind::Semicolon);
    }

    fn assert_next_token(lexer: &mut Lexer, kind: TokenKind) {
        let token = lexer.next_token();

        assert_eq!(token.kind, kind);
    }

    fn assert_next_token_with_literal(
        lexer: &mut Lexer,
        kind: TokenKind,
        literal: &str,
        source: &str,
    ) {
        let token = lexer.next_token();

        assert_eq!(token.kind, kind);
        assert_eq!(token.literal(source), literal);
    }
}
