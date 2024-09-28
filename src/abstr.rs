use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    path::PathBuf,
    rc::Rc,
    sync::{Arc, RwLock},
};

pub use Body::*;
pub use Decl::*;
pub use Pattern::*;
pub use Term::*;

use crate::loc::Identifier;

pub mod pprint;
pub mod typing;

pub trait Declaration {
    fn name(&self) -> Arc<Definition>;
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub name: crate::loc::Identifier,
    pub references: Arc<RwLock<Vec<Reference>>>,
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

impl Definition {
    pub fn new(name: &str) -> Arc<Self> {
        Arc::new(Self {
            name: crate::loc::Identifier::from(name),
            references: Default::default(),
            loc: crate::loc::Loc::default(),
        })
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Reference {
    pub name: crate::loc::Identifier,
    pub definition: Arc<Definition>,
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
    Pair(Vec<Type>),              // 'a * 'b
    Tuple(Vec<Type>),             // ('a, 'b)
    Fun(Box<Type>, Box<Type>),    // 'a -> 'b
    App(Reference, Box<Type>),    // 'a list | ('a, 'b) hashmap
    Local(Box<Type>),             // 'a local - linear types
    Meta(crate::loc::Identifier), // 'a | _
    Constructor(Reference),       //  C
    Hole,
}

#[derive(Debug, Clone)]
pub struct Constructor {
    pub name: Arc<Definition>,
    pub type_repr: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: Arc<Definition>,
    pub variables: Vec<crate::loc::Identifier>,
    pub cases: Vec<Constructor>,
    pub loc: crate::loc::Loc,
}

impl Declaration for TypeDecl {
    fn name(&self) -> Arc<Definition> {
        self.name.clone()
    }
}

#[derive(Debug, Clone)]
pub enum Body {
    External(crate::loc::Text),
    Value(Term),
    No,
}

#[derive(Debug, Clone)]
pub struct LetDecl {
    pub name: Arc<Definition>,
    pub type_repr: Type,
    pub body: Body,
    pub loc: crate::loc::Loc,
}

impl Declaration for LetDecl {
    fn name(&self) -> Arc<Definition> {
        self.name.clone()
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    TypeDecl(TypeDecl),
    LetDecl(LetDecl),
}

impl Declaration for Decl {
    fn name(&self) -> Arc<Definition> {
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
    Fun(Arc<Definition>, Box<Term>),
    Match(Box<Term>, Vec<Case>),
    Ascription(Box<Term>, Type),
    App(Box<Term>, Box<Term>),
    Var(Reference),
    Int(i64),
    Text(crate::loc::Text),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(Arc<Definition>, Box<Term>, Box<Term>),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    PatternSrcPos(Box<Pattern>, crate::loc::Loc),
    Constructor(Reference, Option<Box<Pattern>>),
    Elements(Vec<Pattern>),
    Variable(Arc<Definition>),
}

#[derive(Debug, Clone)]
pub struct File {
    pub path: PathBuf,
    pub shebang: Option<String>,
    pub declarations: HashMap<Identifier, Decl>,
}

impl Definition {
    pub fn use_reference(self: Arc<Self>) -> Reference {
        let reference = Reference {
            name: self.name.clone(),
            loc: self.loc.clone(),
            definition: self.clone(),
        };
        self.references.write().unwrap().push(reference.clone());
        reference
    }
}
