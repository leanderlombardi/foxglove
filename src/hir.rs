use crate::ast::{self, Ast};
use crate::error::ErrorReport;
use crate::Span;
use std::cell::RefCell;

pub fn lower(ast: &Ast) -> (Option<Package>, Vec<ErrorReport>) {
    let lowerer = Lowerer::new();

    match lowerer.lower(ast) {
        Ok(hir) => (Some(hir), Vec::new()),
        Err(errs) => (None, errs),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lowerer {
    current_id: RefCell<usize>,
}

impl Lowerer {
    pub fn new() -> Self {
        Self {
            current_id: RefCell::new(0),
        }
    }

    fn next_id(&self) -> HirId {
        let id = *self.current_id.borrow();
        *self.current_id.borrow_mut() += 1;
        HirId(id)
    }

    pub fn lower(&self, ast: &Ast) -> Result<Package, Vec<ErrorReport>> {
        let mut package = Package {
            id: self.next_id(),
            span: ast.span.clone(),
            modules: Vec::new(),
        };

        let mut module = Module {
            id: self.next_id(),
            span: ast.span.clone(),
            items: Vec::new(),
        };

        for item in &ast.items {
            module.items.push(self.lower_item(item));
        }

        package.modules.push(module);

        Ok(package)
    }

    fn lower_item(&self, item: &ast::Item) -> Item {
        let kind = match &item.kind {
            ast::ItemKind::Fn(f) => ItemKind::Fn(self.lower_function(f)),
        };

        Item {
            id: self.next_id(),
            span: item.span.clone(),
            kind,
        }
    }

    fn lower_function(&self, func: &ast::Function) -> Function {
        let sig = self.lower_function_signature(&func.sig);

        let body = self.lower_statement(&func.body);

        Function {
            id: self.next_id(),
            span: func.span.clone(),
            sig,
            body,
        }
    }

    fn lower_function_signature(&self, sig: &ast::FunctionSignature) -> FunctionSignature {
        let name = self.lower_ident(&sig.name);

        let params = sig
            .clone()
            .params
            .into_iter()
            .map(|param| self.lower_param(&param))
            .collect();

        let ret_ty = self.lower_type(&sig.ret_ty);

        FunctionSignature {
            id: self.next_id(),
            span: sig.span.clone(),
            name,
            params,
            ret_ty,
        }
    }

    fn lower_param(&self, param: &ast::Param) -> Param {
        let name = self.lower_ident(&param.name);

        let ty = self.lower_type(&param.ty);

        Param {
            id: self.next_id(),
            span: param.span.clone(),
            name,
            ty,
        }
    }

    fn lower_statement(&self, stmt: &ast::Statement) -> Statement {
        let kind = match &stmt.kind {
            ast::StatementKind::Block(stmts) => {
                let stmts = stmts
                    .iter()
                    .map(|stmt| self.lower_statement(stmt))
                    .collect();
                StatementKind::Block(stmts)
            }
            ast::StatementKind::Expr(expr) => StatementKind::Expr(self.lower_expr(expr)),
            ast::StatementKind::VarDecl(name, ty, expr) => {
                let name = self.lower_ident(name);
                let ty = ty.as_ref().map(|ty| self.lower_type(ty));
                let expr = self.lower_expr(expr);

                StatementKind::VarDecl(name, ty, expr)
            }
            ast::StatementKind::Assign(target, expr) => {
                let target = self.lower_assignment_target(target);
                let expr = self.lower_expr(expr);

                StatementKind::Assign(target, expr)
            }
            ast::StatementKind::Return(expr) => {
                let expr = expr.as_ref().map(|expr| self.lower_expr(expr));

                StatementKind::Return(expr)
            }
            ast::StatementKind::IfElse { cond, then, else_ } => {
                let cond = self.lower_expr(cond);
                let then = Box::new(self.lower_statement(then));
                let else_ = else_
                    .as_ref()
                    .map(|else_| Box::new(self.lower_statement(else_)));

                StatementKind::IfElse { cond, then, else_ }
            }
            ast::StatementKind::While { cond, stmt } => {
                let cond = self.lower_expr(cond);
                let stmt = Box::new(self.lower_statement(stmt));

                StatementKind::While { cond, stmt }
            }
            ast::StatementKind::For { var, in_, stmt } => {
                let var = self.lower_ident(var);
                let in_ = self.lower_expr(in_);
                let stmt = Box::new(self.lower_statement(stmt));

                StatementKind::For { var, in_, stmt }
            }
            ast::StatementKind::Break => StatementKind::Break,
            ast::StatementKind::Continue => StatementKind::Continue,
            ast::StatementKind::Loop(stmt) => {
                let stmt = Box::new(self.lower_statement(stmt));

                StatementKind::Loop(stmt)
            }
        };

        Statement {
            id: self.next_id(),
            span: stmt.span.clone(),
            kind,
        }
    }

    fn lower_expr(&self, expr: &ast::Expr) -> Expr {
        let kind = match &expr.kind {
            ast::ExprKind::Error => unreachable!(),
            ast::ExprKind::Literal(l) => ExprKind::Literal(self.lower_literal(l)),
            ast::ExprKind::Var(name) => ExprKind::Var(self.lower_ident(name)),
            ast::ExprKind::List(exprs) => {
                let exprs = exprs.iter().map(|expr| self.lower_expr(expr)).collect();
                ExprKind::List(exprs)
            }
            ast::ExprKind::Binary { lhs, op, rhs } => {
                let lhs = Box::new(self.lower_expr(lhs));
                let op = self.lower_binary_op(op);
                let rhs = Box::new(self.lower_expr(rhs));

                ExprKind::Binary { lhs, op, rhs }
            }
            ast::ExprKind::Prefix { op, expr } => {
                let op = self.lower_prefix_op(op);
                let expr = Box::new(self.lower_expr(expr));

                ExprKind::Prefix { op, expr }
            }
            ast::ExprKind::Postfix { expr, op } => {
                let expr = Box::new(self.lower_expr(expr));
                let op = self.lower_postfix_op(op);

                ExprKind::Postfix { expr, op }
            }
        };

        Expr {
            id: self.next_id(),
            span: expr.span.clone(),
            kind,
        }
    }

    fn lower_binary_op(&self, op: &ast::BinaryOp) -> BinaryOp {
        let kind = match &op.kind {
            ast::BinaryOpKind::Add => BinaryOpKind::Add,
            ast::BinaryOpKind::Sub => BinaryOpKind::Sub,
            ast::BinaryOpKind::Mul => BinaryOpKind::Mul,
            ast::BinaryOpKind::Div => BinaryOpKind::Div,
            ast::BinaryOpKind::Range => BinaryOpKind::Range,
            ast::BinaryOpKind::Eq => BinaryOpKind::Eq,
            ast::BinaryOpKind::Neq => BinaryOpKind::Neq,
            ast::BinaryOpKind::Lt => BinaryOpKind::Lt,
            ast::BinaryOpKind::Gt => BinaryOpKind::Gt,
            ast::BinaryOpKind::Lte => BinaryOpKind::Lte,
            ast::BinaryOpKind::Gte => BinaryOpKind::Gte,
        };

        BinaryOp {
            id: self.next_id(),
            span: op.span.clone(),
            kind,
        }
    }

    fn lower_prefix_op(&self, op: &ast::PrefixOp) -> PrefixOp {
        let kind = match &op.kind {
            ast::PrefixOpKind::Pos => PrefixOpKind::Pos,
            ast::PrefixOpKind::Neg => PrefixOpKind::Neg,
        };

        PrefixOp {
            id: self.next_id(),
            span: op.span.clone(),
            kind,
        }
    }

    fn lower_postfix_op(&self, op: &ast::PostfixOp) -> PostfixOp {
        let kind = match &op.kind {
            ast::PostfixOpKind::Error => unreachable!(),
            ast::PostfixOpKind::Call(exprs) => {
                let exprs = exprs.iter().map(|expr| self.lower_expr(expr)).collect();
                PostfixOpKind::Call(exprs)
            }
            ast::PostfixOpKind::Index(expr) => {
                let expr = Box::new(self.lower_expr(expr));
                PostfixOpKind::Index(expr)
            }
        };

        PostfixOp {
            id: self.next_id(),
            span: op.span.clone(),
            kind,
        }
    }

    fn lower_literal(&self, lit: &ast::Literal) -> Literal {
        match &lit {
            ast::Literal::Int(i) => Literal::Int(*i),
            ast::Literal::Float(f) => Literal::Float(*f),
            ast::Literal::Bool(b) => Literal::Bool(*b),
        }
    }

    fn lower_ident(&self, ident: &ast::Ident) -> Ident {
        Ident {
            id: self.next_id(),
            span: ident.span.clone(),
            name: ident.name.clone(),
        }
    }

    fn lower_assignment_target(&self, target: &ast::AssignmentTarget) -> AssignmentTarget {
        let kind = match &target.kind {
            ast::AssignmentTargetKind::Error => unreachable!(),
            ast::AssignmentTargetKind::Var(i) => AssignmentTargetKind::Var(self.lower_ident(i)),
            ast::AssignmentTargetKind::Index(target, expr) => {
                let target = self.lower_assignment_target(target);
                let expr = self.lower_expr(expr);

                AssignmentTargetKind::Index(Box::new(target), Box::new(expr))
            }
        };

        AssignmentTarget {
            id: self.next_id(),
            span: target.span.clone(),
            kind,
        }
    }

    fn lower_type(&self, ty: &ast::Type) -> Type {
        let kind = match &ty.kind {
            ast::TypeKind::Unit => TypeKind::Unit,
            ast::TypeKind::Ident(i) => TypeKind::Ident(self.lower_ident(i)),
            ast::TypeKind::Int => TypeKind::Int,
            ast::TypeKind::Float => TypeKind::Float,
            ast::TypeKind::Bool => TypeKind::Bool,
            ast::TypeKind::List(l) => TypeKind::List(Box::new(self.lower_type(l))),
        };

        Type {
            id: self.next_id(),
            span: ty.span.clone(),
            kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Package {
    pub id: HirId,
    pub span: Span,
    pub modules: Vec<Module>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub id: HirId,
    pub span: Span,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub id: HirId,
    pub span: Span,
    pub kind: ItemKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Fn(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: HirId,
    pub span: Span,
    pub sig: FunctionSignature,
    pub body: Statement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret_ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub id: HirId,
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
    pub id: HirId,
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
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
    pub id: HirId,
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
pub struct Type {
    pub id: HirId,
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
    pub id: HirId,
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
    pub id: HirId,
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
    pub id: HirId,
    pub span: Span,
    pub kind: PostfixOpKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PostfixOpKind {
    Call(Vec<Expr>),
    Index(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub id: HirId,
    pub span: Span,
    pub name: String,
}
