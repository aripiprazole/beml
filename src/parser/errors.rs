use super::*;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected end of file")]
pub struct Eof;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected token")]
pub struct UnexpectedToken;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected token")]
pub struct ExpectedToken(Token);
