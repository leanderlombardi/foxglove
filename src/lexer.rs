use crate::Spanned;
use chumsky::prelude::*;

pub fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let int = text::int(10)
        .validate(|s: String, span, emit| {
            if s.parse::<i64>().is_err() {
                emit(Simple::custom(span, "integer literal doesnt fit in i64"));
            };

            s
        })
        .map(Token::Int)
        .boxed();

    let float = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)))
        .collect::<String>()
        .validate(|s, span, emit| {
            if s.parse::<f64>().is_err() {
                emit(Simple::custom(span, "float literal doesnt fit in f64"));
            };

            s
        })
        .map(Token::Float)
        .boxed();

    let bool = choice((
        just("true").to(Token::Bool(true)),
        just("false").to(Token::Bool(false)),
    ))
    .boxed();

    let ident = text::ident()
        .map(|ident: String| match ident.as_str() {
            "return" => Token::Keyword(Keyword::Return),
            "var" => Token::Keyword(Keyword::Var),
            "fn" => Token::Keyword(Keyword::Fn),
            "if" => Token::Keyword(Keyword::If),
            "else" => Token::Keyword(Keyword::Else),
            "for" => Token::Keyword(Keyword::For),
            "while" => Token::Keyword(Keyword::While),
            "in" => Token::Keyword(Keyword::In),
            "break" => Token::Keyword(Keyword::Break),
            "continue" => Token::Keyword(Keyword::Continue),
            "loop" => Token::Keyword(Keyword::Loop),
            _ => Token::Ident(ident),
        })
        .boxed();

    let op = choice((
        just("<=").to(Token::Operator(Operator::Lte)),
        just(">=").to(Token::Operator(Operator::Gte)),
        just("==").to(Token::Operator(Operator::Eq)),
        just("!=").to(Token::Operator(Operator::Neq)),
        just("..").to(Token::Operator(Operator::Range)),
        just("+").to(Token::Operator(Operator::Plus)),
        just("-").to(Token::Operator(Operator::Minus)),
        just("*").to(Token::Operator(Operator::Star)),
        just("/").to(Token::Operator(Operator::Slash)),
        just("<").to(Token::Operator(Operator::Lt)),
        just(">").to(Token::Operator(Operator::Gt)),
    ))
    .boxed();

    let ctrl = choice((
        just("|>").to(Token::Control(Control::PipeArrow)),
        just("()").to(Token::Control(Control::Unit)),
        just("(").to(Token::Control(Control::OpenParen)),
        just(")").to(Token::Control(Control::CloseParen)),
        just("{").to(Token::Control(Control::OpenCurlyBrace)),
        just("}").to(Token::Control(Control::CloseCurlyBrace)),
        just("[").to(Token::Control(Control::OpenSquareBracket)),
        just("]").to(Token::Control(Control::CloseSquareBracket)),
        just(",").to(Token::Control(Control::Comma)),
        just("=").to(Token::Control(Control::Equals)),
        just(";").to(Token::Control(Control::Semicolon)),
        just("|").to(Token::Control(Control::Pipe)),
        just(":").to(Token::Control(Control::Colon)),
    ))
    .boxed();

    let token = choice((float, int, bool, op, ctrl, ident)).boxed();

    let comment = just("//").then(take_until(just('\n'))).padded().boxed();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .then_ignore(end())
        .recover_with(skip_then_retry_until([]).consume_end())
        .boxed()
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),
    Int(String),
    Float(String),
    Bool(bool),
    Keyword(Keyword),
    Operator(Operator),
    Control(Control),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::Int(n) => write!(f, "{n}"),
            Token::Float(n) => write!(f, "{n}"),
            Token::Bool(b) => write!(f, "{b}"),
            Token::Keyword(kw) => write!(f, "{kw}"),
            Token::Operator(op) => write!(f, "{op}"),
            Token::Control(ctrl) => write!(f, "{ctrl}"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Keyword {
    Return,
    Var,
    Fn,
    If,
    Else,
    For,
    While,
    In,
    Break,
    Continue,
    Loop,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Return => write!(f, "return"),
            Keyword::Var => write!(f, "var"),
            Keyword::Fn => write!(f, "fn"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::For => write!(f, "for"),
            Keyword::While => write!(f, "while"),
            Keyword::In => write!(f, "in"),
            Keyword::Break => write!(f, "break"),
            Keyword::Continue => write!(f, "continue"),
            Keyword::Loop => write!(f, "loop"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Range,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Neq,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Star => write!(f, "*"),
            Operator::Slash => write!(f, "/"),
            Operator::Range => write!(f, ".."),
            Operator::Lt => write!(f, "<"),
            Operator::Gt => write!(f, ">"),
            Operator::Lte => write!(f, "<="),
            Operator::Gte => write!(f, ">="),
            Operator::Eq => write!(f, "=="),
            Operator::Neq => write!(f, "!="),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Control {
    OpenParen,
    CloseParen,
    OpenCurlyBrace,
    CloseCurlyBrace,
    OpenSquareBracket,
    CloseSquareBracket,
    Comma,
    Equals,
    Semicolon,
    Pipe,
    PipeArrow,
    Colon,
    Unit,
}

impl std::fmt::Display for Control {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Control::OpenParen => write!(f, "("),
            Control::CloseParen => write!(f, ")"),
            Control::OpenCurlyBrace => write!(f, "{{"),
            Control::CloseCurlyBrace => write!(f, "}}"),
            Control::OpenSquareBracket => write!(f, "["),
            Control::CloseSquareBracket => write!(f, "]"),
            Control::Comma => write!(f, ","),
            Control::Equals => write!(f, "="),
            Control::Semicolon => write!(f, ";"),
            Control::Pipe => write!(f, "|"),
            Control::PipeArrow => write!(f, "|>"),
            Control::Colon => write!(f, ":"),
            Control::Unit => write!(f, "()"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::lexer;
    use chumsky::Parser;

    macro_rules! lex_test {
        ($name:ident, $input:expr) => {
            #[test]
            fn $name() {
                let (tokens, lex_errs) = lexer().parse_recovery($input);

                insta::with_settings!({
                    description => $input
                },
                {
                    insta::assert_debug_snapshot!(tokens);
                    insta::assert_debug_snapshot!(lex_errs);
                })
            }
        };
    }

    lex_test!(lex_empty, "");

    lex_test!(lex_int, "123");

    lex_test!(lex_float, "123.456");

    lex_test!(lex_bool, "true false");

    lex_test!(lex_list, "[1, 2, 3]");

    lex_test!(lex_list_with_float, "[1, 2.0, 3]");

    lex_test!(lex_empty_list, "[]");

    lex_test!(lex_empty_block, "{}");

    lex_test!(lex_block, "{ 1; 2; }");

    lex_test!(lex_block_with_float, "{ 1; 2.0; }");

    lex_test!(lex_block_with_list, "{ 1; [2, 3]; }");

    lex_test!(lex_block_with_block, "{ 1; { 2; 3; } }");

    lex_test!(lex_block_with_block_with_list, "{ 1; { 2; [3, 4]; } }");

    lex_test!(lex_empty_function, "fn|| {}");

    lex_test!(lex_function, "fn|a, b| { a + b; }");

    lex_test!(lex_function_with_float, "fn|a, b| { a + 1.0; }");

    lex_test!(lex_function_with_list, "fn|a, b| { a + [1, 2]; }");

    lex_test!(lex_weird, "((((((a + b) * c) / d) - e) + f) * g)");

    lex_test!(lex_operators, "+ - * / .. == != < > <= >=");

    lex_test!(lex_delimiters, "( ) [ ] { } , ;");

    lex_test!(
        lex_keywords,
        "return var fn if else for while in break continue"
    );

    lex_test!(lex_pipe, "||>");

    lex_test!(lex_colon, ":");

    lex_test!(lex_unit, "()");
}
