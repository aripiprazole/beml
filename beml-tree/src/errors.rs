use std::fmt::Display;

use crate::loc::{Loc, Source};

/// Error that can occur during the compilation process. It composes all errors of that
/// compilation process.
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error("{compiler_pass}: compiler error")]
pub struct StepFailedError {
    pub compiler_pass: CompilerPass,

    #[related]
    pub errors: Vec<miette::Report>,
}

/// Error that can occur during the compilation process. It composes all errors of that
/// compilation process.
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("{source}")]
pub struct LoweringError<E: miette::Diagnostic + Display + std::error::Error + Send + Sync + 'static> {
    #[label("here")]
    pub loc: Loc,

    #[source_code]
    pub source_code: Source,

    #[source]
    pub source: E,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompilerPass {
    Parsing,
    Lowering,
    TypeChecking,
    BorrowChecking,
    Linearization,
    Desugaring,
    Reduction,
    Normalization,
    CodeGeneration,
}

/// Errors related to the abstract syntax tree.
pub mod abstr {
    use crate::{abstr::Reference, hir::Type};

    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unresolved variable: {name}")]
    pub struct UnresolvedVariableError {
        pub name: String,
    }

    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("type has wrong arity: expected {arity} with {type_repr:?}")]
    pub struct IncorrectTypeArityError {
        pub arity: usize,
        pub type_repr: Type,
    }

    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("type don't have argument: {type_repr:?}")]
    pub struct TypeDontHaveArgumentError {
        pub type_repr: Type,
    }

    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("incompatible pattern type")]
    pub struct IncompatiblePatternTypeError;

    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unresolved constructor: {name}")]
    pub struct UnresolvedConstructorError {
        pub name: String,
    }

    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("application pattern in constructor")]
    pub struct ApplicationPatternInConstructorError;

    #[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
    #[error("unification error")]
    pub enum UnificationError {
        #[error("incompatible types: {0:?} and {1:?}")]
        IncompatibleTypes(Type, Type),

        #[error("incompatible constructors: {} and {}", .0.name.text, .1.name.text)]
        IncompatibleConstructors(Reference, Reference),

        #[error("occurs check between {name} and {type_repr:?}")]
        OccursCheck { name: String, type_repr: Type },
    }
}

/// Errors related to the concrete syntax tree.
pub mod concr {
    // can't find the constructor
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("can't find the constructor: {name}")]
    pub struct UnresolvedConstructorError {
        pub name: String,
    }

    // can't find the constructor
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("can't find the variable: {name}")]
    pub struct UnresolvedVariableError {
        pub name: String,
    }

    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("can't find the type: {name}")]
    pub struct UnresolvedTypeError {
        pub name: String,
    }

    // can't find the constructor, so if it is a variable pattern, uncapitalize it
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("capitalized variables should be constructors and not variables: {name}")]
    pub struct UncapitalizeVariableError {
        pub name: String,

        #[related]
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
    pub enum UnresolvedSymbolError {
        #[error("{0}")]
        UnresolvedConstructorError(UnresolvedConstructorError),

        #[error("{0}")]
        UnresolvedVariableError(UnresolvedVariableError),
    }
}

/// Errors related to the parser.
pub mod parser {
    use super::*;

    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected end of file")]
    pub struct Eof;

    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected token: `{actual}`, possibilities: {}", possibilities.iter().map(|t| format!("`{t}`")).collect::<Vec<_>>().join(", "))]
    pub struct UnexpectedToken {
        pub actual: String,
        pub possibilities: Vec<String>,

        #[label]
        pub span: crate::loc::Loc,

        #[source_code]
        pub source_code: Source,
    }

    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected token: {actual}, expected: {token}")]
    pub struct ExpectedToken {
        pub token: String,
        pub actual: String,

        #[label]
        pub span: crate::loc::Loc,

        #[source_code]
        pub source_code: Source,
    }
}

impl std::fmt::Display for CompilerPass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerPass::Parsing => write!(f, "parsing"),
            CompilerPass::Lowering => write!(f, "lowering"),
            CompilerPass::TypeChecking => write!(f, "type checking"),
            CompilerPass::Desugaring => write!(f, "desugaring"),
            CompilerPass::BorrowChecking => write!(f, "borrow checking"),
            CompilerPass::Linearization => write!(f, "linearization"),
            CompilerPass::Reduction => write!(f, "reduction"),
            CompilerPass::Normalization => write!(f, "normalization"),
            CompilerPass::CodeGeneration => write!(f, "code generation"),
        }
    }
}
