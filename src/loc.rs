use miette::{SourceOffset, SourceSpan};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum Loc {
    #[default]
    Nowhere,
    Loc {
        startpos: usize,
        endpos: usize,
        path: std::path::PathBuf,
    },
}

impl From<Loc> for SourceSpan {
    fn from(value: Loc) -> Self {
        match value {
            Loc::Nowhere => Self::new(SourceOffset::from(0), 0.into()),
            Loc::Loc { startpos, endpos, .. } => {
                let length = endpos - 1 - startpos;
                Self::new(SourceOffset::from(startpos), length.into())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub text: String,
    pub loc: Loc,
}

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
