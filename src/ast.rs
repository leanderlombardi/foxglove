use crate::Spanned;

pub type Ast = Spanned<Vec<Spanned<Statement>>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Error,
    Block(Spanned<Vec<Spanned<Self>>>),
    Expr(Spanned<Expr>),
    VarAssign(Spanned<AssignmentTarget>, Spanned<Expr>),
    VarDeclare(Spanned<Ident>, Option<Spanned<Type>>, Spanned<Expr>),
    Function {
        name: Spanned<Ident>,
        args: Spanned<Vec<(Spanned<Ident>, Spanned<Type>)>>,
        returns: Spanned<Type>,
        body: Box<Spanned<Statement>>,
    },
    Return(Option<Spanned<Expr>>),
    For(Spanned<Ident>, Spanned<Expr>, Box<Spanned<Statement>>),
    IfElse {
        if_: (Spanned<Expr>, Box<Spanned<Statement>>),
        else_: Option<Box<Spanned<Statement>>>,
    },
    While(Spanned<Expr>, Box<Spanned<Statement>>),
    Break,
    Continue,
    Loop(Box<Spanned<Statement>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Error,
    Var(Spanned<Ident>),
    Value(Box<Spanned<Value>>),
    Postfix(Box<Spanned<Self>>, Box<Spanned<PostfixOperator>>),
    Unary(Spanned<UnaryOperator>, Box<Spanned<Self>>),
    Binary(
        Box<Spanned<Self>>,
        Spanned<BinaryOperator>,
        Box<Spanned<Self>>,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentTarget {
    Error,
    Var(Spanned<Ident>),
    Index(Spanned<Ident>, Spanned<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Ident(Spanned<Ident>),
    List(Box<Spanned<Self>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    List(Spanned<Vec<Spanned<Expr>>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PostfixOperator {
    Error,
    Call(Spanned<Vec<Spanned<Expr>>>),
    Index(Spanned<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Pos,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Range,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
}

pub type Ident = String;
