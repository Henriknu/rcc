use thiserror::Error;

use crate::{
    ast::{
        AssignmentStmt, BinaryExpr, BinaryOp, Call, Expr, ExprStmt, Literal, PrintStmt, Stmt, Var,
        VarDecl,
    },
    lexer::Lexer,
    sym::SymbolTable,
    token::{Token, TokenKind},
};

pub struct Program<'s> {
    pub source: &'s str,
    pub stmts: Vec<Stmt>,
    pub symbol_table: SymbolTable,
}

pub type ParserResult<T> = Result<T, ParsingError>;

pub fn parse(source: &str) -> ParserResult<Program> {
    let parser = Parser::new(source);

    parser.parse()
}

pub struct Parser<'s> {
    source: &'s str,
    lexer: Lexer<'s>,
    current: Token,
    next: Token,
    symbol_table: SymbolTable,
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
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn parse(mut self) -> ParserResult<Program<'s>> {
        let mut stmts = Vec::new();

        while !self.is_at_end() {
            dbg!("parsing stmt");

            stmts.push(self.stmt()?);
        }

        Ok(Program {
            source: self.source,
            stmts,
            symbol_table: self.symbol_table,
        })
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

    */

    fn stmt(&mut self) -> ParserResult<Stmt> {
        if self.consume_if(TokenKind::Print) {
            self.print_stmt()
        } else if self.consume_if(TokenKind::Int) {
            self.vardecl_stmt()
        } else if self.current.is_kind(TokenKind::Ident) {
            self.assignment_stmt()
        } else {
            self.expr_stmt()
        }
    }

    fn vardecl_stmt(&mut self) -> ParserResult<Stmt> {
        let name = self.expect(TokenKind::Ident)?;

        self.expect(TokenKind::Semicolon)?;

        let vardecl = VarDecl { name };

        // add name to symbol table

        self.symbol_table.add(name.literal(self.source).to_owned());

        Ok(Stmt::VarDecl(vardecl))
    }

    fn assignment_stmt(&mut self) -> ParserResult<Stmt> {
        let name = self.expect(TokenKind::Ident)?;

        if !self.symbol_table.contains(name.literal(self.source)) {
            return Err(ParsingError::UndeclaredVariable(
                name.literal(self.source).to_owned(),
            ));
        }

        self.expect(TokenKind::Equals)?;

        let expr = self.expr()?;

        self.expect(TokenKind::Semicolon)?;

        let assignment_stmt = AssignmentStmt { name, expr };

        Ok(Stmt::AssignmentStmt(assignment_stmt))
    }

    fn print_stmt(&mut self) -> ParserResult<Stmt> {
        let expr = self.expr()?;
        self.expect(TokenKind::Semicolon)?;

        let print_stmt = PrintStmt { expr };

        Ok(Stmt::PrintStmt(print_stmt))
    }

    fn expr_stmt(&mut self) -> ParserResult<Stmt> {
        let expr = self.expr()?;
        self.expect(TokenKind::Semicolon)?;

        let expr_stmt = ExprStmt { expr };

        Ok(Stmt::ExprStmt(expr_stmt))
    }

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

                if !self.symbol_table.contains(name.literal(self.source)) {
                    return Err(ParsingError::UndeclaredVariable(
                        name.literal(self.source).to_owned(),
                    ));
                }

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
    #[error("Unexpected token (expected {expected:?}, found {actual:?})")]
    UnexpectedToken {
        actual: TokenKind,
        expected: Option<TokenKind>,
    },
    #[error("Undeclared variable: {0}")]
    UndeclaredVariable(String),
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

    /*     #[test]
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
    */
    #[test]
    fn vardecl() {
        const SOURCE: &str = "int a; a = 4 + 4;";

        let stmts = vec![
            Stmt::VarDecl(VarDecl {
                name: Token::new(TokenKind::Ident, TokenSpan::new(4, 1)),
            }),
            Stmt::AssignmentStmt(AssignmentStmt {
                name: Token::new(TokenKind::Ident, TokenSpan::new(7, 1)),
                expr: Expr::BinaryExpression(Box::new(BinaryExpr {
                    left: Expr::Literal(Literal::Int(4)),
                    right: Expr::Literal(Literal::Int(4)),
                    op: BinaryOp::Add,
                })),
            }),
        ];

        let parsed = parse(SOURCE).unwrap();

        assert_eq!(parsed.symbol_table.iter().count(), 1);
        assert_eq!(parsed.stmts, stmts);
    }

    /*
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
