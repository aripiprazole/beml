use std::{cell::Cell, path::PathBuf};

use crate::{
    concr::{File, Term},
    errors::{CompilerPass, StepFailedError},
    lexer::Token,
    loc::Loc,
};

pub mod errors;
pub mod grammar;

use errors::*;
use logos::Logos;
use miette::{IntoDiagnostic, NamedSource};

/// Transforms the tokens into a concrete syntax tree.
pub struct Parser<'a> {
    file: PathBuf,
    lexer: logos::Lexer<'a, crate::lexer::Token>,
    curr: Option<Token>,
    terms: Vec<Term>,
    text: String,
    errors: Vec<miette::Report>,
    #[cfg(debug_assertions)]
    gas: Cell<usize>,
}

/// Create a new parser.
pub fn parse_file(file: PathBuf) -> miette::Result<File> {
    let text = std::fs::read_to_string(&file).into_diagnostic()?;
    Parser::parse(file, text.as_str())
}

impl<'a> Parser<'a> {
    /// Parse a file with a text.
    pub fn parse(file: PathBuf, text: &str) -> miette::Result<File> {
        let mut p = Parser {
            file,
            lexer: Token::lexer(text),
            curr: None,
            errors: vec![],
            text: text.into(),
            terms: vec![],
            gas: Cell::new(0),
        };

        // eat the first and bump into current token
        p.bump();
        while !p.eof() {
            let decl = recover!(&mut p, grammar::decl(&mut p));
            p.terms.push(decl);
        }

        if p.errors.is_empty() {
            Ok(File {
                terms: p.terms,
                path: p.file,
                shebang: None,
                text: text.into(),
            })
        } else {
            Err(StepFailedError {
                compiler_pass: CompilerPass::Parsing,
                errors: p.errors,
            })?
        }
    }

    pub fn report(&mut self, error: miette::Report) {
        let source = NamedSource::new(self.file.to_str().unwrap_or(""), self.text.clone());
        self.errors.push(error.with_source_code(source));
    }

    pub fn expect(&mut self, token: Token) -> Option<(String, crate::loc::Loc)> {
        let tok = { self.eat(token).map(|(text, loc)| (text.to_string(), loc)) };
        match tok {
            Ok(value) => Some(value),
            Err(err) => {
                self.report(err);
                None
            }
        }
    }

    pub fn at_any(&self, tokens: &[Token]) -> bool {
        match self.curr {
            Some(curr) => tokens.contains(&curr),
            None => false,
        }
    }

    pub fn eat(&mut self, token: Token) -> miette::Result<(&str, crate::loc::Loc)> {
        let range = self.lexer.span();
        let text = self.lexer.slice();
        let code = NamedSource::new(self.file.to_str().unwrap_or(""), self.text.clone());
        let span = Loc::Loc {
            startpos: range.start,
            endpos: range.end,
            path: self.file.clone(),
        };
        if Some(token) == self.curr {
            self.bump();
            Ok((text, span))
        } else {
            Err(ExpectedToken {
                token,
                span,
                actual: self.curr.unwrap_or(Token::Skip),
                code,
            })?
        }
    }

    pub fn unexpected_token<T>(&self, possibilities: &[Token]) -> miette::Result<T> {
        let range = self.lexer.span();
        let code = NamedSource::new(self.file.to_str().unwrap_or(""), self.text.clone());
        let span = Loc::Loc {
            startpos: range.start,
            endpos: range.end,
            path: self.file.clone(),
        };
        Err(UnexpectedToken {
            actual: self.curr.unwrap_or(Token::Skip),
            possibilities: possibilities.to_vec(),
            span,
            code,
        })?
    }

    pub fn check(&mut self, token: Token) -> bool {
        Some(token) == self.curr
    }

    pub fn eof(&mut self) -> bool {
        self.curr.is_none()
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> miette::Result<(Token, &str, crate::loc::Loc)> {
        self.bump();
        let Some(token) = self.curr else {
            return Err(Eof)?;
        };
        let text = self.lexer.slice();
        Ok((token, text, self.loc()))
    }

    pub fn text(&self) -> &str {
        self.lexer.slice()
    }

    pub fn loc(&self) -> Loc {
        let range = self.lexer.span();
        Loc::Loc {
            startpos: range.start,
            endpos: range.end,
            path: self.file.clone(),
        }
    }

    pub fn bump(&mut self) {
        #[cfg(debug_assertions)]
        if self.gas.get() == 20000 {
            panic!("gas exhausted")
        }

        match self.lexer.next() {
            Some(Ok(value)) => {
                self.curr = Some(value);
            }
            Some(Err(_)) | None => {
                self.curr = None;
            }
        }
    }
}

macro_rules! close {
    ($p:expr, $expr:expr) => {{
        let m = $p.lexer.span();
        let value = match $expr {
            Ok(value) => value,
            Err(err) => return Err(err),
        };
        if let SrcPos(_, _) = value {
            return Ok(value);
        }
        let e = $p.lexer.span();
        let span = Loc::Loc {
            startpos: m.start,
            endpos: e.end,
            path: $p.file.clone(),
        };
        Ok(SrcPos(value.into(), span))
    }};
}

pub(crate) use close;

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_val() {
        crate::aux::golden_test! {
            expected:
r#"Ok(
    File {
        path: "",
        shebang: None,
        terms: [
            SrcPos(
                ValDecl(
                    ValDecl {
                        name: Identifier {
                            text: "println",
                            loc: Loc {
                                startpos: 21,
                                endpos: 28,
                                path: "",
                            },
                        },
                        type_repr: SrcPos(
                            BinOp(
                                SrcPos(
                                    Var(
                                        Identifier {
                                            text: "string",
                                            loc: Loc {
                                                startpos: 31,
                                                endpos: 37,
                                                path: "",
                                            },
                                        },
                                    ),
                                    Loc {
                                        startpos: 31,
                                        endpos: 40,
                                        path: "",
                                    },
                                ),
                                Arrow,
                                SrcPos(
                                    Var(
                                        Identifier {
                                            text: "unit",
                                            loc: Loc {
                                                startpos: 41,
                                                endpos: 45,
                                                path: "",
                                            },
                                        },
                                    ),
                                    Loc {
                                        startpos: 41,
                                        endpos: 58,
                                        path: "",
                                    },
                                ),
                            ),
                            Loc {
                                startpos: 31,
                                endpos: 58,
                                path: "",
                            },
                        ),
                    },
                ),
                Loc {
                    startpos: 17,
                    endpos: 58,
                    path: "",
                },
            ),
        ],
    },
)"#,
            input: Parser::parse(PathBuf::new(), r#"
                val println : string -> unit
            "#)
        };
    }

    #[test]
    fn test_type() {
        crate::aux::golden_test! {
            expected:
r#"Ok(
    File {
        path: "",
        shebang: None,
        terms: [
            SrcPos(
                TypeDecl(
                    TypeDecl {
                        name: Identifier {
                            text: "list",
                            loc: Loc {
                                startpos: 25,
                                endpos: 29,
                                path: "",
                            },
                        },
                        variable: SrcPos(
                            Meta(
                                Identifier {
                                    text: "a",
                                    loc: Loc {
                                        startpos: 22,
                                        endpos: 24,
                                        path: "",
                                    },
                                },
                            ),
                            Loc {
                                startpos: 22,
                                endpos: 29,
                                path: "",
                            },
                        ),
                        cases: [
                            Constructor(
                                Constructor {
                                    name: Identifier {
                                        text: "Nil",
                                        loc: Loc {
                                            startpos: 50,
                                            endpos: 53,
                                            path: "",
                                        },
                                    },
                                    type_repr: None,
                                },
                            ),
                            Constructor(
                                Constructor {
                                    name: Identifier {
                                        text: "Cons",
                                        loc: Loc {
                                            startpos: 72,
                                            endpos: 76,
                                            path: "",
                                        },
                                    },
                                    type_repr: Some(
                                        SrcPos(
                                            BinOp(
                                                SrcPos(
                                                    Meta(
                                                        Identifier {
                                                            text: "a",
                                                            loc: Loc {
                                                                startpos: 80,
                                                                endpos: 82,
                                                                path: "",
                                                            },
                                                        },
                                                    ),
                                                    Loc {
                                                        startpos: 80,
                                                        endpos: 84,
                                                        path: "",
                                                    },
                                                ),
                                                Star,
                                                SrcPos(
                                                    Parens(
                                                        SrcPos(
                                                            App(
                                                                SrcPos(
                                                                    Meta(
                                                                        Identifier {
                                                                            text: "a",
                                                                            loc: Loc {
                                                                                startpos: 86,
                                                                                endpos: 88,
                                                                                path: "",
                                                                            },
                                                                        },
                                                                    ),
                                                                    Loc {
                                                                        startpos: 86,
                                                                        endpos: 93,
                                                                        path: "",
                                                                    },
                                                                ),
                                                                SrcPos(
                                                                    Var(
                                                                        Identifier {
                                                                            text: "list",
                                                                            loc: Loc {
                                                                                startpos: 89,
                                                                                endpos: 93,
                                                                                path: "",
                                                                            },
                                                                        },
                                                                    ),
                                                                    Loc {
                                                                        startpos: 89,
                                                                        endpos: 94,
                                                                        path: "",
                                                                    },
                                                                ),
                                                            ),
                                                            Loc {
                                                                startpos: 86,
                                                                endpos: 94,
                                                                path: "",
                                                            },
                                                        ),
                                                    ),
                                                    Loc {
                                                        startpos: 85,
                                                        endpos: 107,
                                                        path: "",
                                                    },
                                                ),
                                            ),
                                            Loc {
                                                startpos: 80,
                                                endpos: 107,
                                                path: "",
                                            },
                                        ),
                                    ),
                                },
                            ),
                        ],
                    },
                ),
                Loc {
                    startpos: 17,
                    endpos: 107,
                    path: "",
                },
            ),
        ],
    },
)"#,
            input: Parser::parse(PathBuf::new(), r#"
                type 'a list =
                | Nil
                | Cons of 'a * ('a list)
            "#)
        };
    }

    #[test]
    fn test_let() {
        crate::aux::golden_test! {
            expected:
r#"Ok(
    File {
        path: "",
        shebang: None,
        terms: [
            SrcPos(
                LetDecl(
                    LetDecl {
                        pattern: SrcPos(
                            Var(
                                Identifier {
                                    text: "f",
                                    loc: Loc {
                                        startpos: 21,
                                        endpos: 22,
                                        path: "",
                                    },
                                },
                            ),
                            Loc {
                                startpos: 21,
                                endpos: 24,
                                path: "",
                            },
                        ),
                        parameters: [
                            SrcPos(
                                Var(
                                    Identifier {
                                        text: "x",
                                        loc: Loc {
                                            startpos: 23,
                                            endpos: 24,
                                            path: "",
                                        },
                                    },
                                ),
                                Loc {
                                    startpos: 23,
                                    endpos: 26,
                                    path: "",
                                },
                            ),
                        ],
                        body: SrcPos(
                            Var(
                                Identifier {
                                    text: "x",
                                    loc: Loc {
                                        startpos: 27,
                                        endpos: 28,
                                        path: "",
                                    },
                                },
                            ),
                            Loc {
                                startpos: 27,
                                endpos: 41,
                                path: "",
                            },
                        ),
                    },
                ),
                Loc {
                    startpos: 17,
                    endpos: 41,
                    path: "",
                },
            ),
        ],
    },
)"#,
            input: Parser::parse(PathBuf::new(), r#"
                let f x = x
            "#)
        };
    }
}
