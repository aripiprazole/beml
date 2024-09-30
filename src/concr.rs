use super::*;

use crate::{
    errors::{CompilerPass, JoinErrors},
    loc::Identifier,
};

use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    path::PathBuf,
    rc::Rc,
};

use abstr::{Declaration, Definition};
use miette::IntoDiagnostic;

pub use Term::*;

mod errors;
mod lowering;

#[derive(Debug)]
pub struct TypeDecl {
    pub name: Identifier,
    pub variable: Box<Term>,
    pub cases: Vec<Term>,
}

#[derive(Debug)]
pub struct LetDecl {
    pub pattern: Box<Term>,
    pub parameters: Vec<Term>,
    pub body: Box<Term>,
}

#[derive(Debug)]
pub struct ValDecl {
    pub name: Identifier,
    pub type_repr: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Colon,       // :
    Comma,       // ,
    Star,        // * : pair type
    Arrow,       // ->
    DoubleArrow, // =>
    Bar,         // |
    UserDefined(Identifier),
}

#[derive(Debug)]
pub struct Function {
    pub cases: Vec<Term>,
}

#[derive(Debug)]
pub struct Match {
    pub scrutinee: Box<Term>,
    pub cases: Vec<Term>,
}

#[derive(Debug)]
pub struct Let {
    pub pattern: Box<Term>,
    pub parameters: Vec<Term>,
    pub body: Box<Term>,
    pub next: Box<Term>,
}

#[derive(Debug)]
pub struct If {
    pub condition: Box<Term>,
    pub then: Box<Term>,
    pub otherwise: Box<Term>,
}

#[derive(Debug)]
pub struct Constructor {
    pub name: Identifier,
    pub type_repr: Option<Box<Term>>,
}

#[derive(Debug)]
pub enum Term {
    SrcPos(Box<Term>, crate::loc::Loc),
    Parens(Box<Term>),
    Brackets(Box<Term>),
    Braces(Box<Term>),
    TypeDecl(TypeDecl),
    LetDecl(LetDecl),
    ValDecl(ValDecl),
    Constructor(Constructor),
    BinOp(Box<Term>, BinOp, Box<Term>),
    Fun(Vec<Identifier>, Box<Term>),
    App(Box<Term>, Box<Term>),
    Var(Identifier),
    Meta(Identifier),
    Function(Function),
    Match(Match),
    If(If),
    Let(Let),
    Int(i64),
    Text(crate::loc::Text),
    Error,
}

#[derive(Debug)]
pub struct File {
    pub path: PathBuf,
    pub shebang: Option<String>,
    pub terms: Vec<Term>,
    pub text: String,
}

pub fn lower_file(file: File) -> miette::Result<abstr::File> {
    let mut ctx = lowering::LoweringCtx::new(file.path.clone(), file.text);
    let mut declarations = HashMap::new();
    let terms = file
        .terms
        .into_iter()
        .map(|decl| ctx.do_declaration_lowering(decl))
        .collect::<miette::Result<Vec<_>>>()?;
    for lowering::decl::Defer(f) in terms {
        let mut local = ctx.clone();
        let decl = f(&mut local)?;

        declarations.insert(decl.name().name.clone(), decl);
    }

    if ctx.errors.borrow().is_empty() {
        Ok(abstr::File {
            path: file.path,
            shebang: file.shebang,
            declarations,
        })
    } else {
        Err(JoinErrors {
            compiler_pass: CompilerPass::Lowering,
            errors: unsafe { std::mem::transmute_copy(&ctx.errors.borrow()) },
        })?
    }
}

#[cfg(test)]
mod tests {
    use parser::Parser;

    use super::*;

    #[test]
    fn test_f_x() {
        assert!(lower_file(Parser::parse(PathBuf::new(), r#"let f x = x"#).unwrap(),).is_ok());
    }

    #[test]
    fn test_type() {
        assert!(
            lower_file(Parser::parse(PathBuf::new(), r#"type 'a list = | Nil | Cons of 'a * ('a list)"#).unwrap())
                .is_ok()
        );
    }
}
