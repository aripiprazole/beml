#![feature(box_patterns)]
#![feature(new_range_api)]

pub mod loc {
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
                Loc::Nowhere => Self::new(SourceOffset::from(0), 0),
                Loc::Loc { startpos, endpos, .. } => {
                    let length = endpos - startpos;
                    Self::new(SourceOffset::from(startpos), length)
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
}

pub mod abs;
pub mod aux;
pub mod concrete;
pub mod lexer;
pub mod parser;
