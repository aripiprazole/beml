/// Error that can occur during the compilation process. It composes all errors of that
/// compilation process.
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error("{compiler_pass}: compiler error")]
pub struct StepFailedError {
    pub compiler_pass: CompilerPass,

    #[related]
    pub errors: Vec<miette::Report>,
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
