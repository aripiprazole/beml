use std::{fmt::Debug, hash::Hash, path::PathBuf, sync::Arc};

use miette::{IntoDiagnostic, NamedSource, SourceCode, SourceOffset, SourceSpan};

/// Location in the source code. It is a range in the source code.
#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub enum Loc {
    /// Synthesized location.
    #[default]
    Nowhere,

    /// Actual location in the source code.
    Loc {
        startpos: usize,
        endpos: usize,
        path: Source,
    },
}

impl Debug for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Loc::Nowhere => write!(f, "nowhere"),
            Loc::Loc { startpos, endpos, path } => write!(f, "{}:{startpos}-{endpos}", path.source.name()),
        }
    }
}

/// Source code location.
#[derive(Debug, Clone)]
pub struct Source {
    source: NamedSource<Arc<String>>,
}

impl Source {
    pub fn text(&self) -> &str {
        self.source.inner()
    }

    pub fn get_text(&self) -> String {
        self.source.inner().to_string()
    }
}

impl Hash for Source {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.source.name().hash(state);
    }
}

impl Eq for Source {}

impl PartialEq for Source {
    fn eq(&self, other: &Self) -> bool {
        self.source.name() == other.source.name()
    }
}

impl From<&str> for Source {
    fn from(text: &str) -> Self {
        Self {
            source: NamedSource::new("synthetic source", Arc::new(text.to_string())).with_language("beml"),
        }
    }
}

impl From<String> for Source {
    fn from(text: String) -> Self {
        Self {
            source: NamedSource::new("synthetic source", Arc::new(text.clone())).with_language("beml"),
        }
    }
}

impl TryFrom<PathBuf> for Source {
    type Error = miette::Report;

    fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
        let text = std::fs::read_to_string(&value).into_diagnostic()?;
        let source = NamedSource::new(value.to_str().unwrap_or(""), Arc::new(text.clone())).with_language("beml");
        Ok(Self { source })
    }
}

impl SourceCode for Source {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        self.source.read_span(span, context_lines_before, context_lines_after)
    }
}

/// Transformation into miette's [SourceSpan].
impl From<Loc> for SourceSpan {
    fn from(value: Loc) -> Self {
        match value {
            Loc::Nowhere => Self::new(SourceOffset::from(0), 0),
            Loc::Loc { startpos, endpos, .. } => {
                let length = endpos - startpos;
                Self::new(SourceOffset::from(startpos), length)
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
