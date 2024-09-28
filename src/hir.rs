use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::abstr::{Definition, Reference};

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unification error")]
pub enum UnificationError {
    #[error("incompatible types")]
    IncompatibleTypes(Type, Type),
    #[error("occurs check")]
    OccursCheck,
}

#[derive(Debug, Clone)]
pub struct Term {
    pub value: TermKind,
    pub type_repr: Type,
}

#[derive(Debug, Clone)]
pub enum TermKind {
    List(Vec<Term>),
    Pair(Vec<Term>),
    Fun(Rc<Definition>, Box<Term>),
    Match(Box<Term>, CaseTree),
    Ascription(Box<Term>, Poly),
    App(Box<Term>, Box<Term>),
    Var(Reference),
    Int(i64),
    Text(crate::loc::Text),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(Rc<Definition>, Box<Term>, Box<Term>),
}

#[derive(Debug, Clone)]
pub struct Condition;

#[derive(Debug, Clone)]
pub struct Occurence;

#[derive(Debug, Clone)]
pub enum CaseTree {
    Failure,
    Leaf(usize),
    Branch(Occurence, Vec<(Condition, CaseTree)>, Box<CaseTree>),
}

#[derive(Debug, Clone)]
pub enum Type {
    Pair(Vec<Type>),           // 'a * 'b
    Tuple(Vec<Type>),          // ('a, 'b)
    Fun(Box<Type>, Box<Type>), // 'a -> 'b
    App(Reference, Box<Type>), // 'a list | ('a, 'b) hashmap
    Local(Box<Type>),          // 'a local - linear types
    Constructor(Reference),    //  C
    Meta(usize),
    Hole(Rc<RefCell<Option<Type>>>),
}

impl Type {
    pub fn unify(&self, rhs: Type) -> Result<Type, UnificationError> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct Poly {
    pub args: usize,
    pub mono: Type,
}

impl Poly {
    pub fn new(value: Type) -> Poly {
        Poly { args: 0, mono: value }
    }

    pub fn instantiate(&self) -> Type {
        fn go(holes: &HashMap<usize, Type>, tt: Type) -> Type {
            match tt {
                Type::Pair(elements) => Type::Pair(elements.into_iter().map(|element| go(holes, element)).collect()),
                Type::Tuple(elements) => Type::Tuple(elements.into_iter().map(|element| go(holes, element)).collect()),
                Type::Fun(box domain, box codomain) => Type::Fun(go(holes, domain).into(), go(holes, codomain).into()),
                Type::App(n, box argument) => Type::App(n, go(holes, argument).into()),
                Type::Local(box local) => Type::Local(go(holes, local).into()),
                Type::Constructor(c) => Type::Constructor(c),
                Type::Meta(idx) => holes.get(&idx).cloned().expect("can't index holes"),
                Type::Hole(h) => Type::Hole(h),
            }
        }
        let mut holes = HashMap::new();
        for idx in 0..self.args {
            holes.insert(idx, Type::Hole(Rc::default()));
        }
        go(&holes, self.mono.clone())
    }

    pub fn parse(abstr: crate::abstr::Type) -> Poly {
        fn go(vars: &mut HashMap<String, usize>, tt: crate::abstr::Type) -> Type {
            use crate::abstr::Type::*;
            match tt {
                SrcPos(box term, _) => go(vars, term),
                Pair(elements) => Type::Pair(elements.into_iter().map(|element| go(vars, element)).collect()),
                Tuple(elements) => Type::Tuple(elements.into_iter().map(|element| go(vars, element)).collect()),
                Fun(box domain, box codomain) => Type::Fun(go(vars, domain).into(), go(vars, codomain).into()),
                App(name, box argument) => Type::App(name, go(vars, argument).into()),
                Local(box local) => Type::Local(go(vars, local).into()),
                Meta(id) => {
                    let idx = vars.len();
                    let value = vars.entry(id.text).or_insert(idx);
                    Type::Meta(*value)
                }
                Constructor(constructor) => Type::Constructor(constructor),
                Hole => Type::Hole(Rc::default()),
            }
        }

        let mut vars = HashMap::new();
        let mono = go(&mut vars, abstr);
        Poly { args: vars.len(), mono }
    }
}
