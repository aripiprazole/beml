use std::{
    collections::HashMap,
    hash::Hash,
    path::PathBuf,
    sync::{Arc, RwLock},
};

use crate::{
    abstr::{typing::TypeEnv, Definition, Reference},
    loc::Loc,
};

/// A term is a node in the HIR. It has a [TermKind] and a type. It holds
/// the location and type information of the term.
#[derive(Debug, Clone)]
pub struct Term {
    pub value: TermKind,
    pub src_pos: Loc,
    pub type_repr: Type,
}

/// Means that something can be typed.
pub trait Typeable {
    fn type_of(&self) -> Type;
}

impl Typeable for Type {
    fn type_of(&self) -> Type {
        self.clone()
    }
}

impl Typeable for Term {
    fn type_of(&self) -> Type {
        self.type_repr.clone()
    }
}

/// Kind of a term. It is used to represent the different kinds of terms
/// in the HIR.
#[derive(Debug, Clone)]
pub enum TermKind {
    List(Vec<Term>),                            // [1, 2, 3, 4, 5...]
    Pair(Vec<Term>),                            // (1, 2, 3, 4, 5...)
    Fun(Arc<Definition>, Box<Term>),            // fun x -> x + 1
    Match(CaseTree),                            // match x with | 1 -> true | 2 -> false
    Ascription(Box<Term>, Scheme),              // x : int
    App(Box<Term>, Box<Term>),                  // f x
    Var(Reference),                             // x
    Int(i64),                                   // 42
    Text(crate::loc::Text),                     // "Hello, world!"
    If(Box<Term>, Box<Term>, Box<Term>),        // if x then y else z
    Let(Arc<Definition>, Box<Term>, Box<Term>), // let x = 42 in x + 1
}

/// An occurrence is where a case tree branches. It can be a [Term], a [Variable],
/// a [Reference], or an [Index].
#[derive(Debug, Clone)]
pub enum Occurrence {
    Term(Box<Term>),           // <term>
    Variable(Arc<Definition>), // x
    Index(usize),              // .0 | .1 | ...
    Tuple(usize, usize),       // 0.0 | 0.1 | ...
}

#[derive(Debug, Clone)]
pub enum Condition {
    /// Is instance of a constructor, and it's occurences
    Constructor(Reference, Option<usize>),

    /// Is a tuple, and it's occurences
    Tuple(usize),
}

/// A case tree is a tree of cases, where each case is a [Condition] and a [CaseTree].
///
/// Examples:
///
/// ```ocaml
/// match x with
/// | Cons (x, xs) => true
/// | Nil => false
/// ```
///
/// Will translate into the pseudo-code:
///
/// ```rust,ignore
/// CaseTree::Branch {
///    occurence: Reference(x),
///    cases: vec![
///        (Condition::Constructor(Reference(Cons), Some(0)), CaseTree::Branch {
///            occurrence: Occurrence::Tuple(0, 1),
///            cases: vec![],
///            default: Some(CaseTree::Branch {
///                occurrence: Occurrence::Tuple(0, 1),
///                cases: vec![],
///                default: Some(CaseTree::Leaf(true)),
///            })
///        }),
///        (Condition::Constructor(Reference(Nil)), CaseTree::Leaf(false)),
///    ],
///    default: None,
/// }
/// ```
#[derive(Debug, Clone)]
pub enum CaseTree {
    /// Resiliene case, used so that we can handle the case where we don't
    /// have a match expression.
    Failure,

    /// Leaf with a, it's the leaf of the case tree.
    Leaf(Box<Term>),

    /// Branch with occurence, cases and default case.
    Branch {
        occurence: Occurrence,
        cases: Vec<(Condition, CaseTree)>,
        default: Option<Box<CaseTree>>,
    },
}

/// Monomorphic type. It is used to represent the type of a term in the HIR.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Any,
    Pair(Vec<Type>),           // 'a * 'b
    Tuple(Vec<Type>),          // ('a, 'b)
    Fun(Box<Type>, Box<Type>), // 'a -> 'b
    App(Reference, Box<Type>), // 'a list | ('a, 'b) hashmap
    Local(Box<Type>),          // 'a local - linear types
    Constructor(Reference),    //  C
    Meta(usize),

    /// Miette use Send + Sync, so we need to use Arc, to be thread safe
    /// and able to send it between threads.
    Hole(Variable),
}

pub fn fun_type(domain: &Type, codomain: &Type) -> Type {
    Type::Fun(domain.clone().into(), codomain.clone().into())
}

pub fn app_type(env: &TypeEnv, name: Reference, argument: Type) -> Type {
    let _ = env; // TODO: check arity
    Type::App(name, argument.into())
}

/// Type scheme. It's the polymorphic type of a term in the HIR.
#[derive(Debug, Clone)]
pub struct Scheme {
    pub args: usize,
    pub mono: Type,
}

#[derive(Debug, Clone)]
pub struct Constructor {
    pub type_repr: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct AlgebraicDataType {
    pub definition: Arc<Definition>,
    pub arity: usize,
    pub constructors: im_rc::HashMap<String, Constructor>,
}

#[derive(Debug, Clone)]
pub struct Value {
    pub scheme: Scheme,
    pub value: Term,
}

/// A file is a collection of definitions and algebraic data types.
#[derive(Debug, Clone)]
pub struct File {
    pub path: PathBuf,
    pub algebraic_data_types: im::HashMap<String, AlgebraicDataType>,
    pub definitions: im::HashMap<String, Term>,
}

/// A variable is a mutable reference to a type. It is used to represent
/// a variable in the HIR.
#[derive(Debug, Clone, Default)]
pub struct Variable(usize, Arc<RwLock<Option<Type>>>);

impl Variable {
    pub fn new(idx: usize) -> Self {
        Variable(idx, Arc::new(RwLock::new(None)))
    }

    pub fn value(&self) -> Option<Type> {
        self.1.read().unwrap().clone()
    }

    pub fn update(&self, value: Type) {
        *self.1.write().unwrap() = Some(value);
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for Variable {}

impl Hash for Variable {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("incompatible pattern type")]
pub struct IncompatiblePatternTypeError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unresolved constructor")]
pub struct UnresolvedConstructorError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("application pattern in constructor")]
pub struct ApplicationPatternInConstructorError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unification error")]
pub enum UnificationError {
    #[error("incompatible types")]
    IncompatibleTypes(Type, Type),

    #[error("incompatible constructors")]
    IncompatibleConstructors(Reference, Reference),

    #[error("occurs check")]
    OccursCheck,
}

impl Scheme {
    pub fn new(value: Type) -> Scheme {
        Scheme { args: 0, mono: value }
    }

    pub fn instantiate(&self) -> Type {
        fn go(holes: &HashMap<usize, Type>, tt: Type) -> Type {
            match tt {
                Type::Any => Type::Any,
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
            holes.insert(idx, Type::Hole(Variable::default()));
        }
        go(&holes, self.mono.clone())
    }
}

impl Type {}

impl Type {
    pub fn new(abstr: crate::abstr::Type, env: &TypeEnv) -> Self {
        fn go(vars: &mut HashMap<String, Type>, env: &TypeEnv, value: crate::abstr::Type) -> Type {
            use crate::abstr::Type::*;
            match value {
                SrcPos(box term, _) => go(vars, env, term),
                Pair(elements) => Type::Pair(elements.into_iter().map(|element| go(vars, env, element)).collect()),
                Tuple(elements) => Type::Tuple(elements.into_iter().map(|element| go(vars, env, element)).collect()),
                Fun(box domain, box codomain) => {
                    Type::Fun(go(vars, env, domain).into(), go(vars, env, codomain).into())
                }
                App(name, box argument) => Type::App(name, go(vars, env, argument).into()),
                Local(box local) => Type::Local(go(vars, env, local).into()),
                Meta(id) => vars.entry(id.text).or_insert_with(|| env.fresh_type_variable()).clone(),
                Constructor(constructor) => Type::Constructor(constructor),
                Hole => env.fresh_type_variable(),
            }
        }

        go(&mut HashMap::new(), env, abstr)
    }

    #[allow(clippy::mutable_key_type)]
    pub fn generalize(self) -> Scheme {
        fn go(vars: &mut HashMap<Type, usize>, value: Type) -> Type {
            use Type::*;

            match value {
                Type::Any => Type::Any,
                Pair(elements) => Type::Pair(elements.into_iter().map(|element| go(vars, element)).collect()),
                Tuple(elements) => Type::Tuple(elements.into_iter().map(|element| go(vars, element)).collect()),
                Fun(box domain, box codomain) => Type::Fun(go(vars, domain).into(), go(vars, codomain).into()),
                App(name, box argument) => Type::App(name, go(vars, argument).into()),
                Local(box local) => Type::Local(go(vars, local).into()),
                Hole(Variable(idx, _)) => Type::Meta(*vars.entry(value).or_insert_with(|| idx)),
                Constructor(constructor) => Type::Constructor(constructor),
                Meta(m) => Type::Meta(m),
            }
        }

        let mut vars = HashMap::new();
        let mono = go(&mut vars, self);
        Scheme { args: vars.len(), mono }
    }

    pub fn unify(self, rhs: Type) -> Result<(), UnificationError> {
        use Type::*;
        use UnificationError::*;

        match (self, rhs) {
            (Any, _) | (_, Any) => Ok(()),
            (Local(box lvar), Local(box rvar)) => lvar.unify(rvar),
            (Constructor(lconstructor), Constructor(rconstructor)) if lconstructor == rconstructor => Ok(()),
            (App(ln, box largument), App(rn, box rargument)) if ln == rn => largument.unify(rargument),
            (App(ln, _), App(rn, _)) => Err(IncompatibleConstructors(ln, rn)),
            (Fun(box ldom, box lcod), Fun(box rdom, box rcod)) => {
                ldom.unify(rdom)?;
                lcod.unify(rcod)
            }
            (Pair(lelements), Pair(relements)) => {
                for (lelement, relement) in lelements.into_iter().zip(relements.into_iter()) {
                    lelement.unify(relement)?;
                }
                Ok(())
            }
            (Tuple(lelements), Tuple(relements)) => {
                for (lelement, relement) in lelements.into_iter().zip(relements.into_iter()) {
                    lelement.unify(relement)?;
                }
                Ok(())
            }
            (Hole(h), value) | (value, Hole(h)) => match h.value() {
                Some(contents) => contents.unify(value),
                None => {
                    h.update(value);
                    Ok(())
                }
            },
            (Constructor(lconstructor), Constructor(rconstructor)) => {
                Err(IncompatibleConstructors(lconstructor, rconstructor))
            }
            (lhs, rhs) => Err(IncompatibleTypes(lhs, rhs)),
        }
    }
}
