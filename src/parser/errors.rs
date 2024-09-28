use super::*;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected end of file")]
pub struct Eof;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected token: {actual}, possibilities: {}", possibilities.into_iter().map(|t| format!("{t}")).collect::<Vec<_>>().join(", "))]
pub struct UnexpectedToken {
    pub actual: Token,
    pub possibilities: Vec<Token>,

    #[label]
    pub span: crate::loc::Loc,

    #[source_code]
    pub code: NamedSource<String>,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected token: {actual}, expected: {token}")]
pub struct ExpectedToken {
    pub token: Token,
    pub actual: Token,

    #[label]
    pub span: crate::loc::Loc,
}

macro_rules! recover {
    ($p:expr, $expr:expr) => {
        match $expr {
            Ok(value) => value,
            Err(err) => match err.downcast_ref::<Eof>() {
                Some(_) => return miette::IntoDiagnostic::into_diagnostic(Err(Eof)),
                None => {
                    $p.report(err);
                    $p.bump();
                    Term::Error
                }
            },
        }
    };
}

pub(crate) use recover;
