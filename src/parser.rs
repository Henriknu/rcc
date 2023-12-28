use std::fmt::Display;

use thiserror::Error;

use crate::{
    ast::{
        BinaryExpr, BinaryOp, Call, Expr, ExprStmt, FnDecl, Item, Literal, LocalDecl, Stmt, Var,
    },
    lexer::Lexer,
    token::{Token, TokenKind},
};

pub type ParserResult<T> = Result<T, ParsingError>;

pub fn parse(source: &str) -> ParserResult<Expr> {
    let mut parser = Parser::new(source);
    parser.parse()
}

pub struct Parser<'s> {
    source: &'s str,
    lexer: Lexer<'s>,
    current: Token,
    next: Token,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        let mut lexer = Lexer::new(source);
        let current = lexer.next_token();
        let next = lexer.next_token();

        Self {
            source,
            lexer,
            current,
            next,
        }
    }

    pub fn parse(&mut self) -> ParserResult<Expr> {
        self.expr()
    }

    /*  fn item(&mut self) -> ParserResult<Item> {
        if self.consume_if(TokenKind::Fn) {
            return self.fn_decl();
        }
        panic!()
    }

    fn fn_decl(&mut self) -> ParserResult<Item> {
        let name = self.expect(TokenKind::Ident)?;

        self.expect(TokenKind::LParen)?;

        let mut parameters = Vec::new();

        while !self.current.is_kind(TokenKind::RParen) && !self.is_at_end() {
            let parameter = self.expect(TokenKind::Ident)?;
            parameters.push(parameter);
        }

        self.expect(TokenKind::RParen)?;

        self.expect(TokenKind::LBrace)?;

        let mut body = Vec::new();

        while !self.current.is_kind(TokenKind::RBrace) && !self.is_at_end() {
            let stmt = self.stmt()?;
            body.push(stmt);
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Item::FnDecl(FnDecl {
            name,
            parameters,
            body,
        }))
    }

    fn stmt(&mut self) -> ParserResult<Stmt> {
        if self.consume_if(TokenKind::Let) {
            self.local_decl()
        } else {
            self.expr_stmt()
        }
    }

    fn local_decl(&mut self) -> ParserResult<Stmt> {
        let name = self.expect(TokenKind::Ident)?;

        let initializer = if self.consume_if(TokenKind::Equals) {
            let expr = self.expr()?;
            Some(expr)
        } else {
            None
        };

        self.expect(TokenKind::Semicolon)?;

        let vardecl = LocalDecl { name, initializer };

        Ok(Stmt::LocalDecl(vardecl))
    }

    fn expr_stmt(&mut self) -> ParserResult<Stmt> {
        let expr = self.expr()?;
        self.expect(TokenKind::Semicolon)?;

        let expr_stmt = ExprStmt { expr };

        Ok(Stmt::ExprStmt(expr_stmt))
    } */

    pub fn expr(&mut self) -> ParserResult<Expr> {
        self.term()
    }

    fn term(&mut self) -> ParserResult<Expr> {
        let mut expr = self.factor()?;

        while self.current.kind == TokenKind::Plus || self.current.kind == TokenKind::Minus {
            let op = match self.current.kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            };

            self.consume();

            let right = self.factor()?;
            expr = Expr::BinaryExpression(Box::new(BinaryExpr {
                left: expr,
                right,
                op,
            }))
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ParserResult<Expr> {
        let mut expr = self.call()?;

        while self.current.kind == TokenKind::Star || self.current.kind == TokenKind::Slash {
            let op = match self.current.kind {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                _ => unreachable!(),
            };

            self.consume();

            let right = self.call()?;
            expr = Expr::BinaryExpression(Box::new(BinaryExpr {
                left: expr,
                right,
                op,
            }))
        }

        Ok(expr)
    }

    fn call(&mut self) -> ParserResult<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.consume_if(TokenKind::LParen) {
                expr = self._finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn _finish_call(&mut self, callee: Expr) -> ParserResult<Expr> {
        let mut arguments = Vec::new();

        if !self.current.is_kind(TokenKind::RParen) {
            loop {
                arguments.push(self.expr()?);

                if !self.consume_if(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.expect(TokenKind::RParen)?;

        let call = Call { callee, arguments };

        Ok(Expr::Call(Box::new(call)))
    }

    fn primary(&mut self) -> ParserResult<Expr> {
        match self.current.kind {
            TokenKind::LParen => {
                self.consume();
                let expr = self.expr()?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }

            TokenKind::Ident => {
                let name = self.current;
                self.consume();

                Ok(Expr::Var(Var { name }))
            }

            TokenKind::Number => {
                let parsed: isize = self.current.literal(self.source).parse().unwrap();

                self.consume();

                Ok(Expr::Literal(Literal::Int(parsed)))
            }

            _ => panic!(),
        }
    }

    fn is_at_end(&self) -> bool {
        self.current.is_kind(TokenKind::Eof)
    }

    fn expect(&mut self, kind: TokenKind) -> ParserResult<Token> {
        if self.current.kind != kind {
            return Err(ParsingError::UnexpectedToken {
                actual: self.current.kind,
                expected: Some(kind),
            });
        }

        let current = self.current;
        self.consume();

        Ok(current)
    }

    fn consume_if(&mut self, kind: TokenKind) -> bool {
        if self.current.kind == kind {
            self.consume();
            true
        } else {
            false
        }
    }

    fn consume(&mut self) {
        self.current = self.next;
        self.next = self.lexer.next_token();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Error)]
pub enum ParsingError {
    UnexpectedToken {
        actual: TokenKind,
        expected: Option<TokenKind>,
    },
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingError::UnexpectedToken { actual, expected } => {
                if let Some(expected) = expected {
                    write!(
                        f,
                        "Unexpected token found: {:?}, expected: {:?}",
                        actual, expected
                    )
                } else {
                    write!(f, "Unexpected token found: {:?}", actual)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenSpan;

    use super::*;

    #[test]
    fn add() {
        const SOURCE: &str = "4 + 4";

        let expr = Expr::BinaryExpression(Box::new(BinaryExpr {
            left: Expr::Literal(Literal::Int(4)),
            right: Expr::Literal(Literal::Int(4)),
            op: BinaryOp::Add,
        }));

        assert_eq!(Parser::new(SOURCE).expr().unwrap(), expr);
    }

    #[test]
    fn call() {
        const SOURCE: &str = "print(a)";

        let expr = Expr::Call(Box::new(Call {
            callee: Expr::Var(Var {
                name: Token {
                    kind: TokenKind::Ident,
                    span: TokenSpan { start: 0, len: 5 },
                },
            }),
            arguments: vec![Expr::Var(Var {
                name: Token {
                    kind: TokenKind::Ident,
                    span: TokenSpan { start: 6, len: 1 },
                },
            })],
        }));

        assert_eq!(Parser::new(SOURCE).expr().unwrap(), expr);
    }
    /*
    #[test]
    fn local_decl() {
        const SOURCE: &str = "let a = 4 + 4;";

        let stmt = Stmt::LocalDecl(LocalDecl {
            name: Token::new(TokenKind::Ident, TokenSpan::new(4, 1)),
            initializer: Some(Expr::BinaryExpression(Box::new(BinaryExpr {
                left: Expr::Literal(Literal::Int(4)),
                right: Expr::Literal(Literal::Int(4)),
                op: BinaryOp::Add,
            }))),
        });

        assert_eq!(Parser::new(SOURCE).stmt().unwrap(), stmt);
    }

    #[test]
    fn fn_decl() {
        const SOURCE: &str = "fn main() { let a = 4 + 4; }";

        let fn_decl = vec![Item::FnDecl(FnDecl {
            name: Token::new(TokenKind::Ident, TokenSpan::new(3, 4)),
            parameters: Vec::new(),
            body: vec![Stmt::LocalDecl(LocalDecl {
                name: Token::new(TokenKind::Ident, TokenSpan::new(16, 1)),
                initializer: Some(Expr::BinaryExpression(Box::new(BinaryExpr {
                    left: Expr::Literal(Literal::Int(4)),
                    right: Expr::Literal(Literal::Int(4)),
                    op: BinaryOp::Add,
                }))),
            })],
        })];

        assert_eq!(Parser::new(SOURCE).parse().unwrap(), fn_decl);
    } */
}
