use std::fmt::Display;

use crate::loc::{Loc, Source};

/// Error that can occur during the compilation process. It composes all errors of that
/// compilation process.
///
/// This error is used to report errors that occur during the compilation process, it serves
/// as a wrapper around many errors that can occur in a phase.
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error("{compiler_pass}: compiler error")]
#[diagnostic(code(beml::compiler::step), url(docsrs))]
pub struct StepFailedError {
    pub compiler_pass: CompilerPass,

    #[related]
    pub errors: Vec<miette::Report>,
}

/// Error that can occur during the compilation process. It composes all errors of that
/// compilation process.
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("{source}")]
#[diagnostic(code(beml::compiler::lowering), url(docsrs))]
pub struct LoweringError<E: miette::Diagnostic + Display + std::error::Error + Send + Sync + 'static> {
    #[label("here")]
    pub loc: Loc,

    #[source_code]
    pub source_code: Source,

    #[source]
    #[diagnostic_source]
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

    /// The type checker can not find the specified variable, maybe check the context,
    /// or the spelling of the variable.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unresolved variable: {name}")]
    #[diagnostic(code(beml::typing::unresolved_variable), url(docsrs))]
    pub struct UnresolvedVariableError {
        /// The name of the variable that was not found.
        pub name: String,
    }

    /// The type was expected to have a certain number of arguments, but it has a different
    /// number of arguments.
    ///
    /// This error is reported when the type checker encounters a type that has a different
    /// number of arguments than the expected number.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("type has wrong arity: expected {arity} with {type_repr:?}")]
    #[diagnostic(code(beml::typing::incorrect_type_arity), url(docsrs))]
    pub struct IncorrectTypeArityError {
        /// The expected number of arguments.
        pub arity: usize,

        /// The type that has the wrong arity.
        pub type_repr: Type,
    }

    /// The type checker can not find the specified type, maybe check the context,
    /// or the spelling of the type.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("type don't have argument: {type_repr:?}")]
    #[diagnostic(code(beml::typing::incorrect_type_arity), url(docsrs))]
    pub struct TypeDontHaveArgumentError {
        /// The type that doesn't have an argument.
        pub type_repr: Type,
    }

    /// The type checker can not find the specified constructor, maybe check the context,
    /// or the spelling of the constructor.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("incompatible pattern type")]
    #[diagnostic(code(beml::typing::incompatible_pattern_type), url(docsrs))]
    pub struct IncompatiblePatternTypeError;

    /// The type checker can not find the specified constructor, maybe check the context,
    /// or the spelling of the constructor.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unresolved constructor: {name}")]
    #[diagnostic(code(beml::typing::unresolved_constructor), url(docsrs))]
    pub struct UnresolvedConstructorError {
        /// The name of the constructor that was not found.
        pub name: String,
    }

    /// Application pattern in a constructor is not allowed. This error is reported when
    /// the type checker encounters a pattern that is not a constructor.
    ///
    /// ```beml
    /// type bool = True | False
    ///
    /// match true {
    ///   true x -> x
    /// }
    /// ```
    ///
    /// In this example, the pattern `true x` is not a constructor, so this is an error.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("application pattern in constructor")]
    #[diagnostic(code(beml::typing::application_pattern_in_constructor), url(docsrs))]
    pub struct ApplicationPatternInConstructorError;

    /// The type checker can not find the specified constructor, maybe check the context,
    /// or the spelling of the constructor.
    #[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
    #[error("unification error")]
    pub enum UnificationError {
        /// Incompatible types error.
        ///
        /// ```
        /// val x : bool
        ///
        /// val map : ('a -> 'b) -> 'a list -> 'b list
        /// let _ = map 10 30
        /// ```
        ///
        /// In this example, the type checker checks that the value `x` is of type `bool`, but
        /// the value `10` is of type `'a -> 'b` and `30` is not of type `'a list`. This is an
        /// error.
        #[error("incompatible types: {0:?} and {1:?}")]
        #[diagnostic(code(beml::unfying::incompatible_types), url(docsrs))]
        IncompatibleTypes(Type, Type),

        /// Incompatible constructors error.
        ///
        /// ```
        /// val x : bool
        /// let x = 10
        /// ```
        ///
        /// In this example, the type checker checks that the value `x` is of type `bool`, but
        /// the value `x` is of type `int`. This is an error.
        #[error("incompatible constructors: {} and {}", .0.name.text, .1.name.text)]
        #[diagnostic(code(beml::unfying::incompatible_constructors), url(docsrs))]
        IncompatibleConstructors(Reference, Reference),

        /// Occurs check error. A type 'a can't occur in a type 'a list, for example.
        #[error("occurs check between {name} and {type_repr:?}")]
        #[diagnostic(code(beml::unfying::occurs_check), url(docsrs))]
        OccursCheck { name: String, type_repr: Type },
    }
}

/// Errors related to the concrete syntax tree.
pub mod concr {
    /// The lowering can not find the specified constructor, maybe check the context,
    /// or the spelling of the constructor.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("can't find the constructor: {name}")]
    #[diagnostic(code(beml::lowering::unresolved_constructor), url(docsrs))]
    pub struct UnresolvedConstructorError {
        /// The name of the constructor that was not found.
        pub name: String,
    }

    /// The lowering can not find the specified variable, maybe check the context,
    /// or the spelling of the variable.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("can't find the variable: {name}")]
    #[diagnostic(code(beml::lowering::unresolved_variable), url(docsrs))]
    pub struct UnresolvedVariableError {
        /// The name of the variable that was not found.
        pub name: String,
    }

    /// The lowering can not find the specified type, maybe check the context,
    /// or the spelling of the type.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("can't find the type: {name}")]
    #[diagnostic(code(beml::lowering::unresolved_type), url(docsrs))]
    pub struct UnresolvedTypeError {
        /// The name of the type that was not found.
        pub name: String,
    }

    /// Uncapitalize variable, capitalized variables should be constructors and not variables.
    ///
    /// ```beml
    /// let Variable = 10
    /// ```
    ///
    /// In this example, the variable `Variable` is capitalized, so it should be a constructor,
    /// not a variable. This is an error.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("capitalized variables should be constructors and not variables: {name}")]
    #[diagnostic(
        code(beml::lowering::uncapitalize_variable),
        help("capitalized variables should be constructors and not variables"),
        url(docsrs)
    )]
    pub struct UncapitalizeVariableError {
        /// The name of the variable that was not capitalized.
        pub name: String,

        /// The error that occurred when the variable was not found.
        #[related]
        pub error: Option<UnresolvedConstructorError>,
    }

    /// Pattern constructor application error. This error is reported when the lowering
    /// encounters a pattern that is not a constructor.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("pattern constructor application")]
    #[diagnostic(code(beml::lowering::pattern_constructor_app), url(docsrs))]
    pub struct PatternConstructorAppError {
        pub error: miette::Report,
    }

    /// Expected a constructor error. This error is reported when the lowering
    /// encounters a pattern that is not a constructor.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("expected a constructor")]
    #[diagnostic(code(beml::lowering::expected_constructor), url(docsrs))]
    pub struct ExpectedConstructorError;

    /// Pattern argument already exists error, remove the duplicated argument.
    ///
    /// ```beml
    /// let f x x = x
    /// ```
    ///
    /// In this example, the pattern `x x` is duplicated, so this is an error.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("pattern argument already exists")]
    #[diagnostic(code(beml::lowering::pattern_argument_already_exists), url(docsrs))]
    pub struct PatternArgumentAlreadyExistsError;

    /// Unexpected pattern syntax error, there's something wrong with the pattern syntax,
    /// or there's a term, type or declaration syntax that is not allowed in a pattern.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected pattern syntax")]
    #[diagnostic(code(beml::lowering::unexpected_pattern_syntax), url(docsrs))]
    pub struct UnexpectedPatternSyntaxError;

    /// Unexpected case syntax error, there's something wrong with the case syntax,
    /// or there's a term, type or declaration syntax that is not allowed in a case.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected case syntax")]
    #[diagnostic(code(beml::lowering::unexpected_case_syntax), url(docsrs))]
    pub struct UnexpectedCaseSyntaxError;

    /// Unexpected parameter ascription syntax error, there's something wrong with the
    /// parameter ascription syntax, or there's a term, type or declaration syntax that is not
    /// allowed in a parameter ascription.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected parameter ascription syntax")]
    #[diagnostic(code(beml::lowering::unexpected_parameter_ascription_syntax), url(docsrs))]
    pub struct UnexpectedParameterAscriptionSyntaxError;

    /// Unexpected parameter syntax error, there's something wrong with the parameter syntax,
    /// or there's a term, type or declaration syntax that is not allowed in a parameter.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected parameter syntax")]
    #[diagnostic(code(beml::lowering::unexpected_parameter_syntax), url(docsrs))]
    pub struct UnexpectedParameterSyntaxError;

    /// Unexpected term syntax error, there's something wrong with the term syntax,
    /// or there's a pattern, case, type or declaration syntax that is not allowed in a term.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected term syntax")]
    #[diagnostic(code(beml::lowering::unexpected_term_syntax), url(docsrs))]
    pub struct TermSyntaxError;

    /// Type callee is not a constructor, the callee in type application should be a constructor,
    /// not a type.
    ///
    /// ```beml
    /// val x : 'a 'b
    /// ```
    ///
    /// In this example, the callee in type application is `'b`, which is not a constructor,
    /// so this is an error.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("type callee is not a constructor")]
    #[diagnostic(code(beml::lowering::type_callee_is_not_a_constructor), url(docsrs))]
    pub struct TypeCalleeIsNotAConstructorError;

    /// Unexpected type syntax error, there's something wrong with the type syntax,
    /// or there's a term, case, parameter or declaration syntax that is not allowed in a type.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected type syntax")]
    #[diagnostic(
        code(beml::lowering::unexpected_type_syntax),
        help("the language doesn't support dependent types"),
        url(docsrs)
    )]
    pub struct TypeSyntaxError;

    /// Unexpected decl syntax error, there's something wrong with the decl syntax,
    /// or there's a term, case, parameter or type syntax that is not allowed in a declaration.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected decl syntax")]
    #[diagnostic(code(beml::lowering::unexpected_decl_syntax), url(docsrs))]
    pub struct DeclSyntaxError;

    /// Unexpected constructor syntax error, there's something wrong with the constructor syntax,
    /// or there's a term, case, parameter or type syntax that is not allowed in a constructor.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected decl syntax")]
    pub struct ConstructorSyntaxError;

    /// Unexpected type syntax error, there's something wrong with the type syntax,
    /// or there's a term, case, parameter or declaration syntax that is not allowed in a type.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected type syntax at term level")]
    #[diagnostic(
        code(beml::lowering::unexpected_type_syntax),
        help("the language doesn't support dependent types"),
        url(docsrs)
    )]
    pub struct TypeSyntaxAtTermLevelError;

    /// Unexpected term syntax error, there's something wrong with the term syntax,
    /// or there's a pattern, case, type or declaration syntax that is not allowed in a term.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected term syntax at decl level")]
    #[diagnostic(code(beml::lowering::term_syntax_at_decl_level), url(docsrs))]
    pub struct TermSyntaxAtDeclLevelError;

    /// Unresolved symbol error, the symbol was not found. It can be a constructor, variable,
    /// type or a pattern.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    pub enum UnresolvedSymbolError {
        #[error("{0}")]
        #[diagnostic(transparent)]
        UnresolvedConstructorError(#[from] UnresolvedConstructorError),

        #[error("{0}")]
        #[diagnostic(transparent)]
        UnresolvedVariableError(#[from] UnresolvedVariableError),
    }
}

/// Errors related to the parser.
pub mod parser {
    use super::*;

    /// Unexpected end of file error, the parser reached the end of the file without
    /// finding what it was looking for.
    ///
    /// This error is reported when the parser encounters the end of the file without
    /// finding what it was looking for.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected end of file")]
    #[diagnostic(code(beml::parsing::eof), url(docsrs))]
    pub struct EofError;

    /// Unexpected token error, the parser encountered an unexpected token.
    ///
    /// This error is reported when the parser encounters an unexpected token.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected token: `{actual}`, possibilities: {}", possibilities.iter().map(|t| format!("`{t}`")).collect::<Vec<_>>().join(", "))]
    #[diagnostic(code(beml::parsing::unexpected_token), url(docsrs))]
    pub struct UnexpectedTokenError {
        pub actual: String,
        pub possibilities: Vec<String>,

        /// The location of the token.
        #[label]
        pub span: crate::loc::Loc,

        /// The source code that the parser is parsing.
        #[source_code]
        pub source_code: Source,
    }

    /// Expected token error, the parser encountered an unexpected token.
    ///
    /// This error is reported when the parser encounters an unexpected token.
    #[derive(Debug, thiserror::Error, miette::Diagnostic)]
    #[error("unexpected token: {actual}, expected: {token}")]
    #[diagnostic(code(beml::parsing::expected_token), url(docsrs))]
    pub struct ExpectedTokenError {
        pub token: String,
        pub actual: String,

        /// The location of the token.
        #[label]
        pub span: crate::loc::Loc,

        /// The source code that the parser is parsing.
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
