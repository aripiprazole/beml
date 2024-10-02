//! This module contains the lowering rules for the concrete syntax tree.

use super::*;

use crate::loc::Identifier;

use loc::Source;

pub use Term::*;

#[derive(Debug)]
pub struct TypeDecl {
    pub name: Identifier,
    pub variable: Option<Box<Term>>,
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
    Parens(Option<Box<Term>>),
    Brackets(Option<Box<Term>>),
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
    pub shebang: Option<String>,
    pub terms: Vec<Term>,
    pub source: Source,
}
