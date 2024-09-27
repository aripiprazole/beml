use std::{cell::RefCell, collections::HashMap, fmt::Debug, hash::Hash, path::PathBuf, rc::Rc};

pub use Body::*;
pub use Decl::*;
pub use Pattern::*;
pub use Term::*;

use crate::loc::Text;

pub mod pprint;

pub trait Declaration {
    fn name(&self) -> Rc<Definition>;
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub name: crate::loc::Identifier,
    pub references: Rc<RefCell<Vec<Reference>>>,
    pub loc: crate::loc::Loc,
}

impl PartialEq for Definition {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Definition {}

impl Hash for Definition {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Reference {
    pub name: crate::loc::Identifier,
    pub definition: Rc<Definition>,
    pub loc: crate::loc::Loc,
}

impl Debug for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.name)
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    SrcPos(Box<Type>, crate::loc::Loc),
    Int,                       // int
    Pair(Vec<Type>),           // 'a * 'b
    VPair(Vec<Type>),          // ('a, 'b)
    Fun(Box<Type>, Box<Type>), // 'a -> 'b
    App(Reference, Box<Type>), // 'a list
    Local(Box<Type>),          // 'a local
    Meta(Rc<Definition>),      // 'a
    Constructor(Reference),    //  C
    Hole,
}

#[derive(Debug, Clone)]
pub struct Constructor {
    pub name: Rc<Definition>,
    pub parameters: Type,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: Rc<Definition>,
    pub variables: Vec<Rc<Definition>>,
    pub cases: Vec<Constructor>,
    pub loc: crate::loc::Loc,
}

impl Declaration for TypeDecl {
    fn name(&self) -> Rc<Definition> {
        self.name.clone()
    }
}

#[derive(Debug, Clone)]
pub enum Body {
    External(Text),
    Value(Term),
    No,
}

#[derive(Debug, Clone)]
pub struct LetDecl {
    pub name: Rc<Definition>,
    pub type_repr: Type,
    pub body: Body,
    pub loc: crate::loc::Loc,
}

impl Declaration for LetDecl {
    fn name(&self) -> Rc<Definition> {
        self.name.clone()
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    TypeDecl(TypeDecl),
    LetDecl(LetDecl),
}

impl Declaration for Decl {
    fn name(&self) -> Rc<Definition> {
        match self {
            Decl::TypeDecl(decl) => decl.name(),
            Decl::LetDecl(decl) => decl.name(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Case {
    pub pattern: Pattern,
    pub body: Term,
}

#[derive(Debug, Clone)]
pub enum Term {
    SrcPos(Box<Term>, crate::loc::Loc),
    List(Vec<Term>),
    Pair(Vec<Term>),
    Fun(Rc<Definition>, Box<Term>),
    Match(Box<Term>, Vec<Case>),
    Ascription(Box<Term>, Type),
    App(Box<Term>, Box<Term>),
    Var(Reference),
    Int(i64),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(Rc<Definition>, Box<Term>, Box<Term>),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    PatternSrcPos(Box<Pattern>, crate::loc::Loc),
    Constructor(Reference, Option<Box<Pattern>>),
    Elements(Vec<Pattern>),
    Variable(Rc<Definition>),
}

#[derive(Debug, Clone)]
pub struct File {
    pub path: PathBuf,
    pub shebang: Option<String>,
    pub declarations: HashMap<Rc<Definition>, Decl>,
}

impl Definition {
    pub fn use_reference(self: Rc<Self>) -> Reference {
        let reference = Reference { name: self.name.clone(),
                                    loc: self.loc.clone(),
                                    definition: self.clone() };
        self.references.borrow_mut().push(reference.clone());
        reference
    }
}
