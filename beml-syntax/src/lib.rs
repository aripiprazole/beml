#![feature(box_patterns)]

use std::cell::Cell;

use beml_tree::{
    concr::{File, Term},
    errors::{parser::*, CompilerPass, StepFailedError},
    loc::{Loc, Source},
};

use crate::lexer::Token;

use logos::Logos;

pub mod grammar;
pub mod lexer;

/// Transforms the tokens into a concrete syntax tree.
pub struct Parser<'a> {
    // File and text
    data: Source,
    lexer: logos::Lexer<'a, crate::lexer::Token>,

    // Parsing state
    curr: Option<Token>,
    terms: Vec<Term>,
    lastpos: usize,

    // Error reporting and debugging
    errors: Vec<miette::Report>,
    #[cfg(debug_assertions)]
    gas: Cell<usize>,
}

/// Parse a file with a text.
pub fn parse_file(source: Source) -> miette::Result<File> {
    let text = source.text.clone();
    let mut p = Parser {
        lexer: Token::lexer(&text),
        data: source,
        curr: None,
        errors: vec![],
        terms: vec![],
        lastpos: 0,
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
            shebang: None,
            source: p.data,
        })
    } else {
        Err(StepFailedError {
            compiler_pass: CompilerPass::Parsing,
            errors: p.errors,
        })?
    }
}

macro_rules! recover {
    ($p:expr, $expr:expr) => {
        match $expr {
            Ok(value) => value,
            Err(err) => match err.downcast_ref::<EofError>() {
                Some(_) => return miette::IntoDiagnostic::into_diagnostic(Err(EofError)),
                None => {
                    $p.report(err);
                    $p.bump();
                    Term::Error
                }
            },
        }
    };
}

macro_rules! expect_or_bail {
    ($p:expr, $token:expr) => {
        if let None = $p.expect($token) {
            $p.bump();
            return Ok(Term::Error);
        }
    };
}

impl<'a> Parser<'a> {
    pub fn report(&mut self, error: miette::Report) {
        self.errors.push(error.with_source_code(self.data.clone()));
    }

    pub fn expect(&mut self, token: Token) -> Option<(String, beml_tree::loc::Loc)> {
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

    pub fn eat(&mut self, token: Token) -> miette::Result<(&str, beml_tree::loc::Loc)> {
        let range = self.lexer.span();
        let text = self.lexer.slice();
        let span = Loc::Loc {
            startpos: range.start,
            endpos: range.end,
            path: self.data.clone(),
        };
        if Some(token) == self.curr {
            self.bump();
            Ok((text, span))
        } else {
            Err(ExpectedTokenError {
                token: token.to_string(),
                span,
                actual: self.curr.unwrap_or(Token::Skip).to_string(),
                source_code: self.data.clone(),
            })?
        }
    }

    pub fn unexpected_token<T>(&self, possibilities: &[Token]) -> miette::Result<T> {
        let range = self.lexer.span();
        let span = Loc::Loc {
            startpos: range.start,
            endpos: range.end,
            path: self.data.clone(),
        };
        Err(UnexpectedTokenError {
            actual: self.curr.unwrap_or(Token::Skip).to_string(),
            possibilities: possibilities.iter().map(|t| t.to_string()).collect(),
            span,
            source_code: self.data.clone(),
        })?
    }

    pub fn check(&mut self, token: Token) -> bool {
        Some(token) == self.curr
    }

    pub fn eof(&mut self) -> bool {
        self.curr.is_none()
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> miette::Result<(Token, &str, beml_tree::loc::Loc)> {
        self.bump();
        let Some(token) = self.curr else {
            return Err(EofError)?;
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
            path: self.data.clone(),
        }
    }

    pub fn bump(&mut self) {
        #[cfg(debug_assertions)]
        if self.gas.get() == 20000 {
            panic!("gas exhausted")
        }

        self.lastpos = self.lexer.span().end;

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
        let span = Loc::Loc {
            startpos: m.start,
            endpos: $p.lastpos,
            path: $p.data.clone(),
        };
        Ok(SrcPos(value.into(), span))
    }};
}

pub(crate) use close;
pub(crate) use expect_or_bail;
pub(crate) use recover;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_val() {
        beml_tree::golden_test! {
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
            input: parse_file(Source::from(r#"
                            val println : string -> unit
                        "#))
        };
    }

    #[test]
    fn test_type() {
        beml_tree::golden_test! {
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
            input: parse_file(Source::from(r#"
                            type 'a list =
                            | Nil
                            | Cons of 'a * ('a list)
                        "#))
        };
    }

    #[test]
    fn test_let() {
        beml_tree::golden_test! {
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
            input: parse_file(Source::from(r#"
                            let f x = x
                        "#))
        };
    }
}
