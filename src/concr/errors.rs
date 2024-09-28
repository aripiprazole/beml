// can't find the constructor
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("can't find the constructor")]
pub struct UnresolvedConstructorError;

// can't find the constructor
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("can't find the variable")]
pub struct UnresolvedVariableError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("can't find the type")]
pub struct UnresolvedTypeError;

// can't find the constructor, so if it is a variable pattern, uncapitalize it
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("can't find the constructor")]
pub struct UncapitalizeVariableError {
    pub error: Option<UnresolvedConstructorError>,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("pattern constructor application")]
pub struct PatternConstructorAppError {
    pub error: miette::Report,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("expected a constructor")]
pub struct ExpectedConstructorError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("pattern argument already exists")]
pub struct PatternArgumentAlreadyExistsError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected pattern syntax")]
pub struct UnexpectedPatternSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected case syntax")]
pub struct UnexpectedCaseSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected parameter ascription syntax")]
pub struct UnexpectedParameterAscriptionSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected parameter syntax")]
pub struct UnexpectedParameterSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected term syntax")]
pub struct TermSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("type callee is not a constructor")]
pub struct TypeCalleeIsNotAConstructorError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected type syntax")]
pub struct TypeSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected decl syntax")]
pub struct DeclSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected decl syntax")]
pub struct ConstructorSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected type syntax at term level")]
pub struct TypeSyntaxAtTermLevelError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected term syntax at decl level")]
pub struct TermSyntaxAtDeclLevelError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected symbol error")]
pub enum UnresolvedSymbolError {
    #[error("can't find the symbol")]
    UnresolvedConstructorError(UnresolvedConstructorError),

    #[error("can't find the symbol")]
    UnresolvedVariableError(UnresolvedVariableError),
}
