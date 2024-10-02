use crate::hir::Type;

use super::Reference;

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
