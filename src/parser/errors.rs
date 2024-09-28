use super::*;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected end of file")]
pub struct Eof;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected token")]
pub struct UnexpectedToken;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected token")]
pub struct ExpectedToken(pub Token);

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
