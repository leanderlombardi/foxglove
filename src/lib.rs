//! # foxglove compiler
//!
//! Compiler for the foxglove programming language.
//!
//! ```fox
//! fn main ||> {
//!    print(34 + 35);
//! }
//! ```

use crate::error::ErrorReport;
use ariadne::{Label, Report, ReportKind, Source};
use chumsky::Parser as _;
use std::path::PathBuf;

mod ast;
mod error;
mod hir;
mod lexer;
mod parser;

/// The main entry point for the compiler.
/// It goes through every stage sequentially and only continues to the
/// next stage if the previous stage returned no errors and a `Some` variant.
/// Otherwise it skips everything and prints the errors and exits.
pub fn run(config: Config) -> Result<(), Box<dyn std::error::Error>> {
    let input = std::fs::read_to_string(config.filename)?;

    let (tokens, lex_errs) = lexer::lexer().parse_recovery(input.as_str());

    if config.debug_tokens {
        dbg!(&tokens);
    }

    let (ast, parse_errs) = if lex_errs.is_empty() {
        if let Some(tokens) = tokens {
            parser::parse(tokens, input.chars().count())
        } else {
            (None, Vec::new())
        }
    } else {
        (None, Vec::new())
    };

    if config.debug_ast {
        dbg!(&ast);
    }

    let (hir, lower_errs) = if parse_errs.is_empty() {
        if let Some(ast) = ast {
            hir::lower(&ast)
        } else {
            (None, Vec::new())
        }
    } else {
        (None, Vec::new())
    };

    if config.debug_hir {
        dbg!(&hir);
    }

    Vec::new()
        .into_iter()
        .chain(
            lex_errs
                .into_iter()
                .map(|e| e.map(|tok| tok.to_string()))
                .map(Into::into),
        )
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map(|tok| tok.to_string()))
                .map(Into::into),
        )
        .chain(lower_errs.into_iter())
        .for_each(|e: ErrorReport| {
            let mut report = Report::build(ReportKind::Error, (), e.offset())
                .with_code(e.code().0.clone())
                .with_message(e.msg());

            report.add_labels(
                e.errors()
                    .iter()
                    .map(|e| Label::new(e.span()).with_message(e.msg())),
            );

            if let Some(note) = e.note() {
                report.set_note(note);
            }

            if let Some(help) = e.help_message() {
                report.set_help(help);
            }

            report.finish().eprint(Source::from(&input)).unwrap();
        });

    Err("compilation failed".into())
}

pub type Span = std::ops::Range<usize>;

/// The configuration for the compiler. It is used to pass
/// information from the command line to the compiler.
/// It contains the filename of the file to compile.
pub struct Config {
    filename: PathBuf,
    debug_tokens: bool,
    debug_ast: bool,
    debug_hir: bool,
}

impl Config {
    /// Creates a new `Config` from a filename.
    pub fn new(filename: PathBuf, debug_tokens: bool, debug_ast: bool, debug_hir: bool) -> Self {
        Self {
            filename,
            debug_tokens,
            debug_ast,
            debug_hir,
        }
    }
}
