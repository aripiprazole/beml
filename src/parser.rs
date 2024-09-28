use std::path::PathBuf;

use crate::{concrete::{File, Term},
            lexer::Token,
            loc::Loc};

pub mod errors;
pub mod grammar;

use errors::*;
use logos::Logos;

pub struct Parser<'a> {
    file: PathBuf,
    lexer: logos::Lexer<'a, crate::lexer::Token>,
    curr: Option<Token>,
    terms: Vec<Term>,
    errors: Vec<miette::Report>,
}

impl<'a> Parser<'a> {
    pub fn parse(file: PathBuf, text: &str) -> miette::Result<File> {
        let mut p = Parser { file,
                             lexer: Token::lexer(text),
                             curr: None,
                             errors: vec![],
                             terms: vec![] };

        p.bump();
        while !p.eof()? {
            let decl = grammar::decl(&mut p)?;
            p.terms.push(decl);
        }

        Ok(File { terms: p.terms,
                  path: p.file,
                  shebang: None })
    }

    pub fn report(&mut self, error: miette::Report) {
        self.errors.push(error);
    }

    pub fn expect(&mut self, token: Token) -> miette::Result<(&str, crate::loc::Loc)> {
        if Some(token) == self.curr {
            let text = self.lexer.slice();
            let range = self.lexer.span();
            self.bump();
            Ok((text,
                Loc::Loc { startpos: range.start,
                           endpos: range.end,
                           path: self.file.clone() }))
        } else {
            Err(ExpectedToken(token))?
        }
    }

    pub fn peek(&self) -> (&str, crate::loc::Loc) {
        let text = self.lexer.slice();
        let range = self.lexer.span();
        (text,
         Loc::Loc { startpos: range.start,
                    endpos: range.end,
                    path: self.file.clone() })
    }

    pub fn at_any(&self, tokens: &[Token]) -> bool {
        match self.curr {
            Some(curr) => tokens.contains(&curr),
            None => false,
        }
    }

    pub fn eat(&mut self, token: Token) -> miette::Result<()> {
        if Some(token) == self.curr {
            self.bump();
            Ok(())
        } else {
            Err(UnexpectedToken)?
        }
    }

    pub fn check(&mut self, token: Token) -> miette::Result<bool> {
        Ok(Some(token) == self.curr)
    }

    pub fn eof(&mut self) -> miette::Result<bool> {
        Ok(self.curr.is_none())
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> miette::Result<(Token, &str, crate::loc::Loc)> {
        self.bump();
        let Some(token) = self.curr else {
            return Err(Eof)?;
        };
        let text = self.lexer.slice();
        let range = self.lexer.span();
        Ok((token,
            text,
            Loc::Loc { startpos: range.start,
                       endpos: range.end,
                       path: self.file.clone() }))
    }

    pub fn bump(&mut self) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_file() {
        crate::aux::golden_test! {
            expected: r#""#,
            input: Parser::parse(PathBuf::new(), r#"
                let f x = x
            "#)
        };
    }

    #[test]
    fn test_type() {
        crate::aux::golden_test! {
            expected: r#""#,
            input: Parser::parse(PathBuf::new(), r#"
                type 'a list =
                | Nil
                | Cons of 'a * ('a list)
            "#)
        };
    }

    #[test]
    fn test_val() {
        crate::aux::golden_test! {
            expected: r#""#,
            input: Parser::parse(PathBuf::new(), r#"
                val println : string -> unit
            "#)
        };
    }
}
