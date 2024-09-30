use miette::{SourceOffset, SourceSpan};

/// Location in the source code. It is a range in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum Loc {
    /// Synthesized location.
    #[default]
    Nowhere,

    /// Actual location in the source code.
    Loc {
        startpos: usize,
        endpos: usize,
        path: std::path::PathBuf,
    },
}

/// Transformation into miette's [SourceSpan].
impl From<Loc> for SourceSpan {
    fn from(value: Loc) -> Self {
        match value {
            Loc::Nowhere => Self::new(SourceOffset::from(0), 0.into()),
            Loc::Loc { startpos, endpos, .. } => {
                let length = endpos - startpos;
                Self::new(SourceOffset::from(startpos), length.into())
            }
        }
    }
}

/// Identifier in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub text: String,
    pub loc: Loc,
}

/// String data in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Text {
    pub value: String,
    pub loc: Loc,
}

impl Identifier {
    pub fn new(text: &str, loc: Loc) -> Self {
        Self { text: text.into(), loc }
    }
}

impl From<&str> for Identifier {
    fn from(text: &str) -> Self {
        Self::new(text, Loc::default())
    }
}
