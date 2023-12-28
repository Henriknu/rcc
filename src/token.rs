#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub kind: TokenKind,
    pub span: TokenSpan,
}

impl Token {
    pub fn new(kind: TokenKind, span: TokenSpan) -> Self {
        Self { kind, span }
    }

    pub fn is_kind(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }

    pub fn literal<'s>(&self, source: &'s str) -> &'s str {
        &source[self.span.start..self.span.start + self.span.len]
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct TokenSpan {
    pub start: usize,
    pub len: usize,
}

impl TokenSpan {
    pub fn new(start: usize, len: usize) -> Self {
        Self { start, len }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semicolon,

    Equals,
    Plus,
    Minus,
    Star,
    Slash,

    Print,

    Number,
    Ident,

    Int,

    Eof,
    #[default]
    Invalid,
}
