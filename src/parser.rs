use crate::ast::{
    AssignmentTarget, Ast, BinaryOperator, Expr, Ident, PostfixOperator, Statement, Type,
    UnaryOperator, Value,
};
use crate::lexer::{Control, Keyword, Operator, Token};
use crate::Spanned;
use chumsky::prelude::*;

pub fn parser() -> impl Parser<Token, Ast, Error = Simple<Token>> + Clone {
    statement_parser()
        .repeated()
        .then_ignore(end())
        .recover_with(skip_then_retry_until([Token::Control(Control::Semicolon)]).consume_end())
        .map_with_span(|statements, span| (statements, span))
}

fn statement_parser() -> impl Parser<Token, Spanned<Statement>, Error = Simple<Token>> + Clone {
    recursive(|statement| {
        let block = statement
            .clone()
            .repeated()
            .recover_with(skip_then_retry_until([Token::Control(Control::Semicolon)]).consume_end())
            .delimited_by(
                just(Token::Control(Control::OpenCurlyBrace)),
                just(Token::Control(Control::CloseCurlyBrace)),
            )
            .map_with_span(|stmts, span| (stmts, span))
            .map_with_span(|stmts, span| (Statement::Block(stmts), span))
            .recover_with(nested_delimiters(
                Token::Control(Control::OpenCurlyBrace),
                Token::Control(Control::CloseCurlyBrace),
                [
                    (
                        Token::Control(Control::OpenParen),
                        Token::Control(Control::CloseParen),
                    ),
                    (
                        Token::Control(Control::OpenSquareBracket),
                        Token::Control(Control::CloseSquareBracket),
                    ),
                ],
                |span| (Statement::Error, span),
            ))
            .boxed();

        let ident_assignment = ident_parser()
            .map_with_span(|ident, span| (AssignmentTarget::Var(ident), span))
            .boxed();

        let index = ident_parser()
            .then(expr_parser().delimited_by(
                just(Token::Control(Control::OpenSquareBracket)),
                just(Token::Control(Control::CloseSquareBracket)),
            ))
            .map_with_span(|(expr, index), span| (AssignmentTarget::Index(expr, index), span))
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
                |span| (AssignmentTarget::Error, span),
            ))
            .boxed();

        let assignment_target = choice((index, ident_assignment));

        let var_assign = assignment_target
            .then_ignore(just(Token::Control(Control::Equals)))
            .then(expr_parser())
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map_with_span(|(name, expr), span| (Statement::VarAssign(name, expr), span))
            .boxed();

        let var_declare = just(Token::Keyword(Keyword::Var))
            .ignore_then(ident_parser())
            .then_ignore(just(Token::Control(Control::Colon)))
            .then(type_parser().or_not())
            .then_ignore(just(Token::Control(Control::Equals)))
            .then(expr_parser())
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map_with_span(|((name, type_), expr), span| {
                (Statement::VarDeclare(name, type_, expr), span)
            })
            .boxed();

        let expr = expr_parser()
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map_with_span(|expr, span| (Statement::Expr(expr), span))
            .boxed();

        let function_args = ident_parser()
            .then_ignore(just(Token::Control(Control::Colon)))
            .then(type_parser())
            .separated_by(just(Token::Control(Control::Comma)))
            .allow_trailing()
            .delimited_by(
                just(Token::Control(Control::Pipe)),
                just(Token::Control(Control::PipeArrow)),
            )
            .map_with_span(|args, span| (args, span))
            .recover_with(nested_delimiters(
                Token::Control(Control::Pipe),
                Token::Control(Control::PipeArrow),
                [
                    (
                        Token::Control(Control::OpenParen),
                        Token::Control(Control::CloseParen),
                    ),
                    (
                        Token::Control(Control::OpenSquareBracket),
                        Token::Control(Control::CloseSquareBracket),
                    ),
                    (
                        Token::Control(Control::OpenCurlyBrace),
                        Token::Control(Control::CloseCurlyBrace),
                    ),
                ],
                |span| (vec![], span),
            ))
            .boxed();

        let function = just(Token::Keyword(Keyword::Fn))
            .ignore_then(ident_parser())
            .then(function_args)
            .then(type_parser().or_not().map_with_span(|t, span| (t, span)))
            .then(statement.clone())
            .map_with_span(|(((name, args), returns), body), span| {
                (
                    Statement::Function {
                        name,
                        args,
                        returns: returns.0.unwrap_or((Type::Unit, returns.1)),
                        body: Box::new(body),
                    },
                    span,
                )
            })
            .boxed();

        let return_ = just(Token::Keyword(Keyword::Return))
            .ignore_then(expr_parser().or_not())
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map_with_span(|expr, span| (Statement::Return(expr), span))
            .boxed();

        let break_ = just(Token::Keyword(Keyword::Break))
            .ignored()
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map_with_span(|_, span| (Statement::Break, span))
            .boxed();

        let continue_ = just(Token::Keyword(Keyword::Continue))
            .ignored()
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map_with_span(|_, span| (Statement::Continue, span))
            .boxed();

        let if_ = just(Token::Keyword(Keyword::If))
            .ignore_then(expr_parser().delimited_by(
                just(Token::Control(Control::OpenParen)),
                just(Token::Control(Control::CloseParen)),
            ))
            .then(statement.clone())
            .map(|(if_cond, if_stmt)| (if_cond, Box::new(if_stmt)))
            .boxed();

        let else_ = just(Token::Keyword(Keyword::Else))
            .ignore_then(statement.clone())
            .map(Box::new)
            .boxed();

        let if_else = if_
            .then(else_.or_not())
            .map_with_span(|(if_, else_), span| (Statement::IfElse { if_, else_ }, span))
            .boxed();

        let for_ = just(Token::Keyword(Keyword::For))
            .ignore_then(ident_parser())
            .then_ignore(just(Token::Keyword(Keyword::In)))
            .then(expr_parser())
            .then(block.clone())
            .map_with_span(|((name, expr), block), span| {
                (Statement::For(name, expr, Box::new(block)), span)
            })
            .boxed();

        let while_ = just(Token::Keyword(Keyword::While))
            .ignore_then(expr_parser())
            .then(block.clone())
            .map_with_span(|(expr, block), span| (Statement::While(expr, Box::new(block)), span))
            .boxed();

        choice((
            block,
            if_else,
            var_assign,
            var_declare,
            expr,
            function,
            return_,
            break_,
            continue_,
            for_,
            while_,
        ))
    })
}

fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let val = select! {
            Token::Int(n) => Value::Int(n.parse().unwrap_or(0)),
            Token::Float(n) => Value::Float(n.parse().unwrap_or(0.0)),
            Token::Bool(b) => Value::Bool(b),
        }
        .map_with_span(|val, span| (val, span))
        .map_with_span(|val, span| (Expr::Value(Box::new(val)), span))
        .boxed();

        let var = ident_parser().map_with_span(|name, span| (Expr::Var(name), span));

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
                |span| (Expr::Error, span),
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
            .map_with_span(|args, span| (args, span))
            .map_with_span(|args, span| (Value::List(args), span))
            .map_with_span(|list, span| (Expr::Value(Box::new(list)), span))
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
                |span| (Expr::Error, span),
            ))
            .boxed();

        let atom = choice((val, var, parenthesized_expr, list)).boxed();

        let call_args = expr
            .clone()
            .separated_by(just(Token::Control(Control::Comma)))
            .allow_trailing()
            .delimited_by(
                just(Token::Control(Control::OpenParen)),
                just(Token::Control(Control::CloseParen)),
            )
            .map_with_span(|args, span| (args, span))
            .map_with_span(|args, span| (PostfixOperator::Call(args), span))
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
                |span| (PostfixOperator::Error, span),
            ))
            .boxed();

        let index = expr
            .clone()
            .delimited_by(
                just(Token::Control(Control::OpenSquareBracket)),
                just(Token::Control(Control::CloseSquareBracket)),
            )
            .map_with_span(|index, span| (PostfixOperator::Index(index), span))
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
                |span| (PostfixOperator::Error, span),
            ))
            .boxed();

        let op = choice((call_args, index));

        let postfix = atom
            .then(op.repeated())
            .foldl(|f, args| {
                let span = f.1.start..args.1.end;
                (Expr::Postfix(Box::new(f), Box::new(args)), span)
            })
            .boxed();

        let op = choice((
            just(Token::Operator(Operator::Plus)).to(UnaryOperator::Pos),
            just(Token::Operator(Operator::Minus)).to(UnaryOperator::Neg),
        ))
        .map_with_span(|op, span| (op, span));

        let unary = op
            .repeated()
            .then(postfix)
            .foldr(|op, a| {
                let span = op.1.start..a.1.end;
                (Expr::Unary(op, Box::new(a)), span)
            })
            .boxed();

        let op = choice((
            just(Token::Operator(Operator::Star)).to(BinaryOperator::Mul),
            just(Token::Operator(Operator::Slash)).to(BinaryOperator::Div),
        ))
        .map_with_span(|op, span| (op, span));

        let product = unary
            .clone()
            .then(op.then(unary).repeated())
            .foldl(|a: Spanned<Expr>, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary(Box::new(a), op, Box::new(b)), span)
            })
            .boxed();

        let op = choice((
            just(Token::Operator(Operator::Plus)).to(BinaryOperator::Add),
            just(Token::Operator(Operator::Minus)).to(BinaryOperator::Sub),
        ))
        .map_with_span(|op, span| (op, span));

        let sum = product
            .clone()
            .then(op.then(product).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary(Box::new(a), op, Box::new(b)), span)
            })
            .boxed();

        let op = choice((
            just(Token::Operator(Operator::Lt)).to(BinaryOperator::Lt),
            just(Token::Operator(Operator::Lte)).to(BinaryOperator::Lte),
            just(Token::Operator(Operator::Gt)).to(BinaryOperator::Gt),
            just(Token::Operator(Operator::Gte)).to(BinaryOperator::Gte),
        ))
        .map_with_span(|op, span| (op, span));

        let relational = sum
            .clone()
            .then(op.then(sum).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary(Box::new(a), op, Box::new(b)), span)
            })
            .boxed();

        let op = choice((
            just(Token::Operator(Operator::Eq)).to(BinaryOperator::Eq),
            just(Token::Operator(Operator::Neq)).to(BinaryOperator::Neq),
        ))
        .map_with_span(|op, span| (op, span));

        let equality = relational
            .clone()
            .then(op.then(relational).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary(Box::new(a), op, Box::new(b)), span)
            })
            .boxed();

        let op = choice((just(Token::Operator(Operator::Range)).to(BinaryOperator::Range),))
            .map_with_span(|op, span| (op, span));

        let range = equality
            .clone()
            .then(op.then(equality).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary(Box::new(a), op, Box::new(b)), span)
            })
            .boxed();

        range
    })
}

fn ident_parser() -> impl Parser<Token, Spanned<Ident>, Error = Simple<Token>> + Clone {
    filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident),
        _ => Err(Simple::expected_input_found(span, [], Some(tok))),
    })
    .map_with_span(|name, span| (name, span))
    .boxed()
}

fn type_parser() -> impl Parser<Token, Spanned<Type>, Error = Simple<Token>> + Clone {
    recursive(|type_| {
        let unit = just(Token::Control(Control::Unit))
            .map_with_span(|_, span| (Type::Unit, span))
            .boxed();

        let ident = ident_parser()
            .map_with_span(|name, span| (Type::Ident(name), span))
            .boxed();

        let list = type_
            .delimited_by(
                just(Token::Control(Control::OpenSquareBracket)),
                just(Token::Control(Control::CloseSquareBracket)),
            )
            .map_with_span(|inner, span| (Type::List(Box::new(inner)), span))
            .boxed();

        choice((unit, list, ident)).boxed()
    })
}
