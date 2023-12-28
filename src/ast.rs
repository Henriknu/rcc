use crate::token::Token;

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
    LocalDecl(LocalDecl),
    ExprStmt(ExprStmt),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalDecl {
    pub name: Token,
    pub initializer: Option<Expr>,
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
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Literal {
    Int(isize),
}
