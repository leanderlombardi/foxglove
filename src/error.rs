use crate::Span;
use ariadne::{Color, Fmt, Span as _};
use chumsky::error::SimpleReason;
use chumsky::prelude::Simple;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ErrorReport {
    msg: String,
    code: ErrorCode,
    errors: Vec<ActualError>,
    help_message: Option<String>,
    note: Option<String>,
}

impl ErrorReport {
    fn new(
        msg: String,
        code: ErrorCode,
        errors: Vec<ActualError>,
        help_message: Option<String>,
        note: Option<String>,
    ) -> Self {
        Self {
            msg,
            code,
            errors,
            help_message,
            note,
        }
    }

    fn custom(span: Span, msg: &str) -> Self {
        Self::new(
            msg.to_string(),
            ErrorKind::Custom.into(),
            vec![ActualError::new(span, msg.to_string())],
            None,
            None,
        )
    }

    fn unexpected(span: Span, found: Option<&str>, expected: Vec<Option<&str>>) -> Self {
        Self::new(
            format!(
                "{}, expected {}",
                if found.is_some() {
                    "Unexpected token in input"
                } else {
                    "Unexpected end of input"
                },
                if expected.is_empty() {
                    "something else".to_string()
                } else {
                    expected
                        .iter()
                        .map(|expected| match expected {
                            Some(expected) => expected.fg(Color::Yellow).to_string(),
                            None => "end of input".fg(Color::Yellow).to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                }
            ),
            ErrorKind::Unexpected.into(),
            vec![ActualError::new(
                span,
                format!(
                    "Unexpected token `{}`",
                    found.unwrap_or("end of file").fg(Color::Yellow)
                ),
            )],
            None,
            None,
        )
    }

    fn unclosed(span: Span, delimiter: &str, found: Option<&str>) -> Self {
        Self::new(
            format!("Unclosed delimiter `{}`", delimiter.fg(Color::Yellow)),
            ErrorKind::Unclosed.into(),
            vec![
                ActualError::new(
                    span.clone(),
                    format!("Unclosed delimiter `{}`", delimiter.fg(Color::Yellow)),
                ),
                ActualError::new(
                    span,
                    format!(
                        "Must be closed before this `{}`",
                        found.unwrap_or("end of file").fg(Color::Yellow)
                    ),
                ),
            ],
            None,
            None,
        )
    }

    pub fn msg(&self) -> &str {
        &self.msg
    }

    pub fn code(&self) -> &ErrorCode {
        &self.code
    }

    pub fn errors(&self) -> &[ActualError] {
        &self.errors
    }

    pub fn help_message(&self) -> Option<&str> {
        self.help_message.as_deref()
    }

    pub fn note(&self) -> Option<&str> {
        self.note.as_deref()
    }

    pub fn offset(&self) -> usize {
        self.errors
            .iter()
            .map(|e| e.span().start())
            .min()
            .unwrap_or(0)
    }
}

impl From<Simple<String, Span>> for ErrorReport {
    fn from(e: Simple<String, Span>) -> Self {
        match e.reason() {
            SimpleReason::Custom(msg) => Self::custom(e.span(), msg),
            SimpleReason::Unexpected => Self::unexpected(
                e.span(),
                e.found().map(|x| x.as_str()),
                e.expected()
                    .map(|e| e.as_ref().map(|e| e.as_str()))
                    .collect(),
            ),
            SimpleReason::Unclosed { span, delimiter } => {
                Self::unclosed(span.clone(), delimiter, e.found().map(|x| x.as_str()))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ActualError {
    span: Span,
    msg: String,
}

impl ActualError {
    fn new(span: Span, msg: String) -> Self {
        Self { span, msg }
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }

    pub fn msg(&self) -> &str {
        &self.msg
    }
}

enum ErrorKind {
    Custom,
    Unexpected,
    Unclosed,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ErrorCode(pub String);

impl From<ErrorKind> for ErrorCode {
    fn from(kind: ErrorKind) -> Self {
        ErrorCode(format!(
            "E{:04}",
            match kind {
                ErrorKind::Custom => 1,
                ErrorKind::Unexpected => 2,
                ErrorKind::Unclosed => 3,
            }
        ))
    }
}
