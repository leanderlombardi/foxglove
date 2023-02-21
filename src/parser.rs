use std::cell::RefCell;

use crate::ast::{
    AssignmentTarget, AssignmentTargetKind, Ast, BinaryOp, BinaryOpKind, Expr, ExprKind, Function,
    FunctionSignature, Ident, Item, ItemKind, Literal, NodeId, Param, PostfixOp, PostfixOpKind,
    PrefixOp, PrefixOpKind, Statement, StatementKind, Type, TypeKind,
};
use crate::lexer::{Control, Keyword, Operator, Token};
use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    current_id: RefCell<usize>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            current_id: RefCell::new(0),
        }
    }

    fn next_id(&self) -> NodeId {
        let id = *self.current_id.borrow();
        *self.current_id.borrow_mut() += 1;
        NodeId(id)
    }

    pub fn parser(&self) -> impl ChumskyParser<Token, Ast, Error = Simple<Token>> + '_ {
        self.item_parser()
            .repeated()
            .then_ignore(end())
            .recover_with(skip_then_retry_until([]).consume_end())
            .map_with_span(|items, span| Ast {
                id: self.next_id(),
                span,
                items,
            })
            .boxed()
    }

    fn item_parser(&self) -> impl ChumskyParser<Token, Item, Error = Simple<Token>> + '_ {
        let function = self.function_parser().map(ItemKind::Fn);

        choice((function,))
            .map_with_span(|kind, span| Item {
                id: self.next_id(),
                span,
                kind,
            })
            .boxed()
    }

    fn function_parser(&self) -> impl ChumskyParser<Token, Function, Error = Simple<Token>> + '_ {
        just(Token::Keyword(Keyword::Fn))
            .then(self.function_signature_parser())
            .then(self.statement_parser())
            .map_with_span(|((_, sig), body), span| Function {
                id: self.next_id(),
                span,
                sig,
                body,
            })
            .boxed()
    }

    fn statement_parser(&self) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + '_ {
        recursive(|stmt| {
            let block = stmt
                .clone()
                .repeated()
                .delimited_by(
                    just(Token::Control(Control::OpenCurlyBrace)),
                    just(Token::Control(Control::CloseCurlyBrace)),
                )
                .map(StatementKind::Block)
                .boxed();

            let expr = self
                .expr_parser()
                .then_ignore(just(Token::Control(Control::Semicolon)))
                .map(StatementKind::Expr)
                .boxed();

            let var_decl = just(Token::Keyword(Keyword::Var))
                .ignore_then(self.ident_parser())
                .then(
                    (just(Token::Control(Control::Colon)))
                        .ignore_then(self.type_parser())
                        .or_not(),
                )
                .then_ignore(just(Token::Control(Control::Equals)))
                .then(self.expr_parser())
                .then_ignore(just(Token::Control(Control::Semicolon)))
                .map(|((name, ty), expr)| StatementKind::VarDecl(name, ty, expr))
                .boxed();

            let assign = self
                .assignment_target_parser()
                .then_ignore(just(Token::Control(Control::Equals)))
                .then(self.expr_parser())
                .then_ignore(just(Token::Control(Control::Semicolon)))
                .map(|(target, expr)| StatementKind::Assign(target, expr))
                .boxed();

            let return_stmt = just(Token::Keyword(Keyword::Return))
                .then(self.expr_parser().or_not())
                .then_ignore(just(Token::Control(Control::Semicolon)))
                .map(|(_, expr)| StatementKind::Return(expr))
                .boxed();

            let if_else = just(Token::Keyword(Keyword::If))
                .ignore_then(self.expr_parser())
                .then(stmt.clone().map(Box::new))
                .then(
                    just(Token::Keyword(Keyword::Else))
                        .ignore_then(stmt.clone())
                        .map(Box::new)
                        .or_not(),
                )
                .map(|((cond, then), else_)| StatementKind::IfElse { cond, then, else_ })
                .boxed();

            let while_ = just(Token::Keyword(Keyword::While))
                .ignore_then(self.expr_parser())
                .then(stmt.clone().map(Box::new))
                .map(|(cond, stmt)| StatementKind::While { cond, stmt })
                .boxed();

            let for_ = just(Token::Keyword(Keyword::For))
                .ignore_then(self.ident_parser())
                .then_ignore(just(Token::Keyword(Keyword::In)))
                .then(self.expr_parser())
                .then(stmt.clone().map(Box::new))
                .map(|((var, in_), stmt)| StatementKind::For { var, in_, stmt })
                .boxed();

            let break_ = just(Token::Keyword(Keyword::Break))
                .then_ignore(just(Token::Control(Control::Semicolon)))
                .map(|_| StatementKind::Break)
                .boxed();

            let continue_ = just(Token::Keyword(Keyword::Continue))
                .then_ignore(just(Token::Control(Control::Semicolon)))
                .map(|_| StatementKind::Continue)
                .boxed();

            let loop_ = just(Token::Keyword(Keyword::Loop))
                .ignore_then(stmt.clone().map(Box::new))
                .map(StatementKind::Loop)
                .boxed();

            choice((
                block,
                assign,
                var_decl,
                expr,
                return_stmt,
                if_else,
                while_,
                for_,
                break_,
                continue_,
                loop_,
            ))
            .map_with_span(|kind, span| Statement {
                id: self.next_id(),
                span,
                kind,
            })
            .boxed()
        })
    }

    fn expr_parser(&self) -> impl ChumskyParser<Token, Expr, Error = Simple<Token>> + '_ {
        recursive(|expr| {
            let literal = select! {
                Token::Int(n) => Literal::Int(n.parse().unwrap_or(0)),
                Token::Float(n) => Literal::Float(n.parse().unwrap_or(0.0)),
                Token::Bool(b) => Literal::Bool(b),
            }
            .map(ExprKind::Literal)
            .map_with_span(|kind, span| Expr {
                id: self.next_id(),
                span,
                kind,
            });

            let var = self
                .ident_parser()
                .map(ExprKind::Var)
                .map_with_span(|kind, span| Expr {
                    id: self.next_id(),
                    span,
                    kind,
                });

            let parenthesized_expr = expr
                .clone()
                .delimited_by(
                    just(Token::Control(Control::OpenParen)),
                    just(Token::Control(Control::CloseParen)),
                )
                .recover_with(nested_delimiters(
                    Token::Control(Control::OpenParen),
                    Token::Control(Control::CloseParen),
                    [
                        (
                            Token::Control(Control::OpenCurlyBrace),
                            Token::Control(Control::CloseCurlyBrace),
                        ),
                        (
                            Token::Control(Control::OpenSquareBracket),
                            Token::Control(Control::CloseSquareBracket),
                        ),
                    ],
                    |span| Expr {
                        id: self.next_id(),
                        span,
                        kind: ExprKind::Error,
                    },
                ))
                .boxed();

            let list = expr
                .clone()
                .separated_by(just(Token::Control(Control::Comma)))
                .allow_trailing()
                .delimited_by(
                    just(Token::Control(Control::OpenSquareBracket)),
                    just(Token::Control(Control::CloseSquareBracket)),
                )
                .map(ExprKind::List)
                .map_with_span(|kind, span| Expr {
                    id: self.next_id(),
                    span,
                    kind,
                })
                .boxed();

            let atom = choice((literal, var, parenthesized_expr, list)).boxed();

            let call_args = expr
                .clone()
                .separated_by(just(Token::Control(Control::Comma)))
                .allow_trailing()
                .delimited_by(
                    just(Token::Control(Control::OpenParen)),
                    just(Token::Control(Control::CloseParen)),
                )
                .map(PostfixOpKind::Call)
                .recover_with(nested_delimiters(
                    Token::Control(Control::OpenParen),
                    Token::Control(Control::CloseParen),
                    [
                        (
                            Token::Control(Control::OpenCurlyBrace),
                            Token::Control(Control::CloseCurlyBrace),
                        ),
                        (
                            Token::Control(Control::OpenSquareBracket),
                            Token::Control(Control::CloseSquareBracket),
                        ),
                    ],
                    |_| PostfixOpKind::Error,
                ))
                .boxed();

            let index = expr
                .clone()
                .delimited_by(
                    just(Token::Control(Control::OpenSquareBracket)),
                    just(Token::Control(Control::CloseSquareBracket)),
                )
                .map(|index| PostfixOpKind::Index(Box::new(index)))
                .recover_with(nested_delimiters(
                    Token::Control(Control::OpenSquareBracket),
                    Token::Control(Control::CloseSquareBracket),
                    [
                        (
                            Token::Control(Control::OpenParen),
                            Token::Control(Control::CloseParen),
                        ),
                        (
                            Token::Control(Control::OpenCurlyBrace),
                            Token::Control(Control::CloseCurlyBrace),
                        ),
                    ],
                    |_| PostfixOpKind::Error,
                ))
                .boxed();

            let op = choice((call_args, index)).map_with_span(|op, span| PostfixOp {
                id: self.next_id(),
                span,
                kind: op,
            });

            let postfix = atom
                .then(op.repeated())
                .foldl(|expr, op| {
                    let span = expr.span.start..op.span.end;

                    Expr {
                        id: self.next_id(),
                        span,
                        kind: ExprKind::Postfix {
                            expr: Box::new(expr),
                            op,
                        },
                    }
                })
                .boxed();

            let op = choice((
                just(Token::Operator(Operator::Plus)).to(PrefixOpKind::Pos),
                just(Token::Operator(Operator::Minus)).to(PrefixOpKind::Neg),
            ))
            .map_with_span(|op, span| PrefixOp {
                id: self.next_id(),
                span,
                kind: op,
            });

            let prefix = op
                .repeated()
                .then(postfix)
                .foldr(|op, a| {
                    let span = op.span.start..a.span.end;

                    Expr {
                        id: self.next_id(),
                        span,
                        kind: ExprKind::Prefix {
                            op,
                            expr: Box::new(a),
                        },
                    }
                })
                .boxed();

            let op = choice((
                just(Token::Operator(Operator::Star)).to(BinaryOpKind::Mul),
                just(Token::Operator(Operator::Slash)).to(BinaryOpKind::Div),
            ))
            .map_with_span(|kind, span| BinaryOp {
                id: self.next_id(),
                span,
                kind,
            });

            let product = prefix
                .clone()
                .then(op.then(prefix).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.span.start..b.span.end;

                    Expr {
                        id: self.next_id(),
                        span,
                        kind: ExprKind::Binary {
                            lhs: Box::new(a),
                            op,
                            rhs: Box::new(b),
                        },
                    }
                })
                .boxed();

            let op = choice((
                just(Token::Operator(Operator::Plus)).to(BinaryOpKind::Add),
                just(Token::Operator(Operator::Minus)).to(BinaryOpKind::Sub),
            ))
            .map_with_span(|op, span| BinaryOp {
                id: self.next_id(),
                span,
                kind: op,
            });

            let sum = product
                .clone()
                .then(op.then(product).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.span.start..b.span.end;

                    Expr {
                        id: self.next_id(),
                        span,
                        kind: ExprKind::Binary {
                            lhs: Box::new(a),
                            op,
                            rhs: Box::new(b),
                        },
                    }
                })
                .boxed();

            let op = choice((
                just(Token::Operator(Operator::Lt)).to(BinaryOpKind::Lt),
                just(Token::Operator(Operator::Lte)).to(BinaryOpKind::Lte),
                just(Token::Operator(Operator::Gt)).to(BinaryOpKind::Gt),
                just(Token::Operator(Operator::Gte)).to(BinaryOpKind::Gte),
            ))
            .map_with_span(|op, span| BinaryOp {
                id: self.next_id(),
                span,
                kind: op,
            });

            let relational = sum
                .clone()
                .then(op.then(sum).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.span.start..b.span.end;

                    Expr {
                        id: self.next_id(),
                        span,
                        kind: ExprKind::Binary {
                            lhs: Box::new(a),
                            op,
                            rhs: Box::new(b),
                        },
                    }
                })
                .boxed();

            let op = choice((
                just(Token::Operator(Operator::Eq)).to(BinaryOpKind::Eq),
                just(Token::Operator(Operator::Neq)).to(BinaryOpKind::Neq),
            ))
            .map_with_span(|op, span| BinaryOp {
                id: self.next_id(),
                span,
                kind: op,
            });

            let equality = relational
                .clone()
                .then(op.then(relational).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.span.start..b.span.end;

                    Expr {
                        id: self.next_id(),
                        span,
                        kind: ExprKind::Binary {
                            lhs: Box::new(a),
                            op,
                            rhs: Box::new(b),
                        },
                    }
                })
                .boxed();

            let op = choice((just(Token::Operator(Operator::Range)).to(BinaryOpKind::Range),))
                .map_with_span(|op, span| BinaryOp {
                    id: self.next_id(),
                    span,
                    kind: op,
                });

            let range = equality
                .clone()
                .then(op.then(equality).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.span.start..b.span.end;

                    Expr {
                        id: self.next_id(),
                        span,
                        kind: ExprKind::Binary {
                            lhs: Box::new(a),
                            op,
                            rhs: Box::new(b),
                        },
                    }
                })
                .boxed();

            range
        })
    }

    fn function_signature_parser(
        &self,
    ) -> impl ChumskyParser<Token, FunctionSignature, Error = Simple<Token>> + '_ {
        self.ident_parser()
            .then(
                self.param_parser()
                    .separated_by(just(Token::Control(Control::Comma)))
                    .allow_trailing()
                    .delimited_by(
                        just(Token::Control(Control::Pipe)),
                        just(Token::Control(Control::PipeArrow)),
                    ),
            )
            .then(self.type_parser())
            .map_with_span(|((name, params), ret_ty), span| FunctionSignature {
                id: self.next_id(),
                span,
                name,
                params,
                ret_ty,
            })
            .boxed()
    }

    fn param_parser(&self) -> impl ChumskyParser<Token, Param, Error = Simple<Token>> + '_ {
        self.ident_parser()
            .then(just(Token::Control(Control::Colon)))
            .then(self.type_parser())
            .map_with_span(|((name, _), ty), span| Param {
                id: self.next_id(),
                span,
                name,
                ty,
            })
            .boxed()
    }

    fn ident_parser(&self) -> impl ChumskyParser<Token, Ident, Error = Simple<Token>> + '_ {
        filter_map(|span, tok| match tok {
            Token::Ident(ident) => Ok(ident),
            _ => Err(Simple::expected_input_found(span, [], Some(tok))),
        })
        .map_with_span(|name, span| Ident {
            id: self.next_id(),
            span,
            name,
        })
        .boxed()
    }

    fn type_parser(&self) -> impl ChumskyParser<Token, Type, Error = Simple<Token>> + '_ {
        recursive(|type_| {
            let unit = just(Token::Control(Control::Unit))
                .ignored()
                .map(|_| TypeKind::Unit)
                .map_with_span(|kind, span| Type {
                    id: self.next_id(),
                    span,
                    kind,
                })
                .boxed();

            let other = self
                .ident_parser()
                .map(|name| match name.name.as_str() {
                    "i32" => TypeKind::Int,
                    "f32" => TypeKind::Float,
                    "bool" => TypeKind::Bool,
                    _ => TypeKind::Ident(name),
                })
                .map_with_span(|kind, span| Type {
                    id: self.next_id(),
                    span,
                    kind,
                })
                .boxed();

            let list = type_
                .delimited_by(
                    just(Token::Control(Control::OpenSquareBracket)),
                    just(Token::Control(Control::CloseSquareBracket)),
                )
                .map_with_span(|inner, span| Type {
                    id: self.next_id(),
                    span,
                    kind: TypeKind::List(Box::new(inner)),
                })
                .boxed();

            choice((unit, list, other)).boxed()
        })
    }

    fn assignment_target_parser(
        &self,
    ) -> impl ChumskyParser<Token, AssignmentTarget, Error = Simple<Token>> + '_ {
        let ident_assignment = self
            .ident_parser()
            .map(AssignmentTargetKind::Var)
            .map_with_span(|kind, span| AssignmentTarget {
                id: self.next_id(),
                span,
                kind,
            })
            .boxed();

        let index = ident_assignment
            .clone()
            .map(Box::new)
            .then(self.expr_parser().map(Box::new).delimited_by(
                just(Token::Control(Control::OpenSquareBracket)),
                just(Token::Control(Control::CloseSquareBracket)),
            ))
            .map(|(target, expr)| AssignmentTargetKind::Index(target, expr))
            .recover_with(nested_delimiters(
                Token::Control(Control::OpenSquareBracket),
                Token::Control(Control::CloseSquareBracket),
                [
                    (
                        Token::Control(Control::OpenParen),
                        Token::Control(Control::CloseParen),
                    ),
                    (
                        Token::Control(Control::OpenCurlyBrace),
                        Token::Control(Control::CloseCurlyBrace),
                    ),
                ],
                |_| AssignmentTargetKind::Error,
            ))
            .map_with_span(|kind, span| AssignmentTarget {
                id: self.next_id(),
                span,
                kind,
            })
            .boxed();

        choice((index, ident_assignment)).boxed()
    }
}
