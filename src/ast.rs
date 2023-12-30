use crate::token::{Token, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Item {
    FnDecl(FnDecl),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FnDecl {
    pub name: Token,
    pub parameters: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stmt {
    IfStmt(IfStmt),
    VarDecl(VarDecl),
    AssignmentStmt(AssignmentStmt),
    ExprStmt(ExprStmt),
    PrintStmt(PrintStmt),
    BlockStmt(BlockStmt),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IfStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
    pub else_: Option<Box<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockStmt {
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PrintStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AssignmentStmt {
    pub name: Token,
    pub expr: Expr,
}

// Int datatype is implicit
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarDecl {
    pub name: Token,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    BinaryExpression(Box<BinaryExpr>),
    Literal(Literal),
    Var(Var),
    Call(Box<Call>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Call {
    pub callee: Expr,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Var {
    pub name: Token,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BinaryExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: BinaryOp,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Equals,
    NotEquals,
    GreaterOrEquals,
    Greater,
    LessOrEquals,
    Less,
}

impl BinaryOp {
    pub fn from_token_kind(token_kind: &TokenKind) -> Self {
        match token_kind {
            TokenKind::EqualsEquals => BinaryOp::Equals,
            TokenKind::NotEquals => BinaryOp::NotEquals,
            TokenKind::Greater => BinaryOp::Greater,
            TokenKind::Less => BinaryOp::Less,
            TokenKind::GreaterOrEquals => BinaryOp::GreaterOrEquals,
            TokenKind::LessOrEquals => BinaryOp::LessOrEquals,
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Sub,
            TokenKind::Star => BinaryOp::Mul,
            TokenKind::Slash => BinaryOp::Div,
            _ => unreachable!(),
        }
    }

    pub fn is_comparison(&self) -> bool {
        use BinaryOp::*;
        matches!(
            self,
            Equals | NotEquals | Greater | GreaterOrEquals | Less | LessOrEquals
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Literal {
    Int(isize),
}
