use std::{
    fmt::Debug,
    hash::Hash,
    sync::{Arc, RwLock},
};

use fxhash::FxBuildHasher;
pub use Body::*;
pub use Decl::*;
pub use Pattern::*;
pub use Term::*;

use crate::loc::{Identifier, Source};

pub mod pprint;

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
pub enum TypeRepr {
    SrcPos(Box<TypeRepr>, crate::loc::Loc),
    Pair(Vec<TypeRepr>),               // 'a * 'b
    Tuple(Vec<TypeRepr>),              // ('a, 'b)
    Fun(Box<TypeRepr>, Box<TypeRepr>), // 'a -> 'b
    App(Reference, Box<TypeRepr>),     // 'a list | ('a, 'b) hashmap
    Local(Box<TypeRepr>),              // 'a local - linear types
    Meta(crate::loc::Identifier),      // 'a | _
    Constructor(Reference),            //  C
    Hole,
}

impl TypeRepr {
    /// Computes the free type variables of a type.
    pub fn ftv(&self) -> im_rc::HashSet<String, FxBuildHasher> {
        fn go(vars: &mut im_rc::HashSet<String, FxBuildHasher>, value: &TypeRepr) {
            use TypeRepr::*;
            match value {
                SrcPos(box term, _) => go(vars, term),
                Pair(elements) => elements.iter().for_each(|element| go(vars, element)),
                Tuple(elements) => elements.iter().for_each(|element| go(vars, element)),
                Fun(box domain, box codomain) => {
                    go(vars, domain);
                    go(vars, codomain);
                }
                App(_, box argument) => go(vars, argument),
                Local(box local) => go(vars, local),
                Meta(id) => {
                    vars.insert(id.text.clone());
                }
                Constructor(_) | Hole => {}
            }
        }

        let mut vars = im_rc::HashSet::default();
        go(&mut vars, self);
        vars
    }
}

#[derive(Debug, Clone)]
pub struct Constructor {
    pub def: Arc<Definition>,
    pub type_repr: Option<TypeRepr>,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub def: Arc<Definition>,
    pub variables: Vec<crate::loc::Identifier>,
    pub cases: Vec<Constructor>,
    pub loc: crate::loc::Loc,
}

impl Declaration for TypeDecl {
    fn name(&self) -> Arc<Definition> {
        self.def.clone()
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
    pub def: Arc<Definition>,
    pub type_repr: TypeRepr,
    pub body: Body,
    pub loc: crate::loc::Loc,
}

impl Declaration for LetDecl {
    fn name(&self) -> Arc<Definition> {
        self.def.clone()
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
    Ascription(Box<Term>, TypeRepr),
    App(Box<Term>, Box<Term>),
    Var(Reference),
    Int(i64),
    Text(crate::loc::Text),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(Arc<Definition>, Box<Term>, Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    PatternSrcPos(Box<Pattern>, crate::loc::Loc),
    Constructor(Reference, Option<Box<Pattern>>),
    Elements(Vec<Pattern>),
    Variable(Arc<Definition>),
}

impl Pattern {
    pub fn unwrap(self) -> Pattern {
        match self {
            Pattern::PatternSrcPos(box pattern, _) => pattern.unwrap(),
            pattern => pattern,
        }
    }
}

#[derive(Debug, Clone)]
pub struct File {
    pub shebang: Option<String>,
    pub declarations: im_rc::HashMap<Identifier, Decl, FxBuildHasher>,
    pub source: Source,
}

pub trait HasLocation {
    /// Returns the location of the current context. So we can use it
    /// to get the location of the current term and represents the
    /// references to definitions.
    fn src_pos(&self) -> crate::loc::Loc;
}

/// Returns nowhere whenever we request the location of the context.
pub struct HasNowhere;

impl HasLocation for HasNowhere {
    fn src_pos(&self) -> crate::loc::Loc {
        crate::loc::Loc::Nowhere
    }
}

impl Definition {
    pub fn use_at<T: HasLocation>(self: Arc<Self>, ctx: &T) -> Reference {
        let reference = Reference {
            name: self.name.clone(),
            loc: ctx.src_pos(),
            definition: self.clone(),
        };
        self.references.write().unwrap().push(reference.clone());
        reference
    }
}
