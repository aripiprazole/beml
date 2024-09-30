use super::*;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected end of file")]
pub struct Eof;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected token: {actual}, possibilities: {}", possibilities.iter().map(|t| format!("{t}")).collect::<Vec<_>>().join(", "))]
pub struct UnexpectedToken {
    pub actual: Token,
    pub possibilities: Vec<Token>,

    #[label]
    pub span: crate::loc::Loc,

    #[source_code]
    pub code: NamedSource,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected token: {actual}, expected: {token}")]
pub struct ExpectedToken {
    pub token: Token,
    pub actual: Token,

    #[label]
    pub span: crate::loc::Loc,

    #[source_code]
    pub code: NamedSource,
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

macro_rules! expect_or_bail {
    ($p:expr, $token:expr) => {
        if let None = $p.expect($token) {
            $p.bump();
            return Ok(Term::Error);
        }
    };
}

pub(crate) use expect_or_bail;
pub(crate) use recover;
