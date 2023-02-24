use crate::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct NodeId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Ast {
    pub id: NodeId,
    pub span: Span,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub id: NodeId,
    pub span: Span,
    pub kind: ItemKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Fn(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: NodeId,
    pub span: Span,
    pub sig: FunctionSignature,
    pub body: Statement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub id: NodeId,
    pub span: Span,
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret_ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub id: NodeId,
    pub span: Span,
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub id: NodeId,
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Block(Vec<Statement>),
    Expr(Expr),
    VarDecl(Ident, Option<Type>, Expr),
    Assign(AssignmentTarget, Expr),
    Return(Option<Expr>),
    IfElse {
        cond: Expr,
        then: Box<Statement>,
        else_: Option<Box<Statement>>,
    },
    While {
        cond: Expr,
        stmt: Box<Statement>,
    },
    For {
        var: Ident,
        in_: Expr,
        stmt: Box<Statement>,
    },
    Break,
    Continue,
    Loop(Box<Statement>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub id: NodeId,
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Error,
    Literal(Literal),
    Var(Ident),
    List(Vec<Expr>),
    Binary {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
    },
    Prefix {
        op: PrefixOp,
        expr: Box<Expr>,
    },
    Postfix {
        expr: Box<Expr>,
        op: PostfixOp,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentTarget {
    pub id: NodeId,
    pub span: Span,
    pub kind: AssignmentTargetKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentTargetKind {
    Var(Ident),
    Index(Box<AssignmentTarget>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub id: NodeId,
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub id: NodeId,
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Unit,
    Ident(Ident),
    Int,
    Float,
    Bool,
    List(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOp {
    pub id: NodeId,
    pub span: Span,
    pub kind: BinaryOpKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOpKind {
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

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixOp {
    pub id: NodeId,
    pub span: Span,
    pub kind: PrefixOpKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOpKind {
    Pos,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PostfixOp {
    pub id: NodeId,
    pub span: Span,
    pub kind: PostfixOpKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PostfixOpKind {
    Error,
    Call(Vec<Expr>),
    Index(Box<Expr>),
}
