use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    path::PathBuf,
    rc::Rc,
    sync::{Arc, RwLock},
};

use fxhash::FxBuildHasher;

use crate::{
    abstr::{errors::UnificationError, typing::TypeEnv, Definition, Reference},
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
    Error,
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
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Any,
    Pair(Vec<Type>),           // 'a * 'b
    Tuple(Vec<Type>),          // ('a, 'b)
    Fun(Box<Type>, Box<Type>), // 'a -> 'b
    App(Reference, Box<Type>), // 'a list | ('a, 'b) hashmap
    Local(Box<Type>),          // 'a local - linear types
    Constructor(Reference),    //  C
    Rigid(String),

    /// Miette use Send + Sync, so we need to use Arc, to be thread safe
    /// and able to send it between threads.
    Flexible(Variable),
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
    pub args: Vec<String>,
    pub mono: Type,
}

#[derive(Debug, Clone)]
pub struct Constructor {
    pub type_repr: Option<Type>,
    pub scheme: Scheme,
}

#[derive(Debug, Clone)]
pub struct AlgebraicDataType {
    pub definition: Arc<Definition>,
    pub scheme: Scheme,
    pub arity: usize,
    pub constructors: RefCell<im_rc::HashMap<String, Constructor, FxBuildHasher>>,
}

impl AlgebraicDataType {
    pub fn get(&self, name: &str) -> Option<Constructor> {
        self.constructors.borrow().get(name).cloned()
    }
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
    pub algebraic_data_types: im_rc::HashMap<String, Rc<AlgebraicDataType>, FxBuildHasher>,
    pub definitions: im::HashMap<String, Term, FxBuildHasher>,
}

/// A variable is a mutable reference to a type. It is used to represent
/// a variable in the HIR.
#[derive(Debug, Clone)]
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

impl Scheme {
    pub fn new(value: Type) -> Scheme {
        Scheme {
            args: vec![],
            mono: value,
        }
    }

    pub fn instantiate(&self, env: &TypeEnv) -> Type {
        fn go(holes: &HashMap<String, Type, FxBuildHasher>, tt: Type) -> Type {
            match tt {
                Type::Any => Type::Any,
                Type::Pair(elements) => Type::Pair(elements.into_iter().map(|element| go(holes, element)).collect()),
                Type::Tuple(elements) => Type::Tuple(elements.into_iter().map(|element| go(holes, element)).collect()),
                Type::Fun(box domain, box codomain) => Type::Fun(go(holes, domain).into(), go(holes, codomain).into()),
                Type::App(n, box argument) => Type::App(n, go(holes, argument).into()),
                Type::Local(box local) => Type::Local(go(holes, local).into()),
                Type::Constructor(c) => Type::Constructor(c),
                Type::Rigid(idx) => holes
                    .get(&idx)
                    .cloned()
                    .unwrap_or_else(|| panic!("can't index hole {idx}")),
                Type::Flexible(h) => Type::Flexible(h),
            }
        }
        let mut holes = HashMap::default();
        for idx in self.args.iter() {
            holes.insert(idx.to_string(), env.fresh_type_variable());
        }
        go(&holes, self.mono.clone())
    }

    pub fn apply(&self, pat: Type, env: &TypeEnv) -> Type {
        match self.instantiate(env) {
            Type::App(reference, box argument) => {
                env.unify_catch(&pat, &argument);
                Type::App(reference, pat.into())
            }
            t => t,
        }
    }
}

impl Type {
    pub fn force(self) -> Type {
        match self {
            Type::Flexible(h) => h.value().unwrap_or(Type::Any),
            other => other,
        }
    }

    pub fn new_with_args(vars: &HashMap<String, Type>, env: &TypeEnv, abstr: crate::abstr::Type) -> Self {
        use crate::abstr::Type::*;
        match abstr {
            SrcPos(box term, _) => Self::new_with_args(vars, env, term),
            Pair(elements) => Type::Pair(
                elements
                    .into_iter()
                    .map(|element| Self::new_with_args(vars, env, element))
                    .collect(),
            ),
            Tuple(elements) => Type::Tuple(
                elements
                    .into_iter()
                    .map(|element| Self::new_with_args(vars, env, element))
                    .collect(),
            ),
            Fun(box domain, box codomain) => Type::Fun(
                Self::new_with_args(vars, env, domain).into(),
                Self::new_with_args(vars, env, codomain).into(),
            ),
            App(name, box argument) => Type::App(name, Self::new_with_args(vars, env, argument).into()),
            Local(box local) => Type::Local(Self::new_with_args(vars, env, local).into()),
            Meta(id) => vars
                .get(&id.text)
                .unwrap_or_else(|| panic!("can't index hole {}", id.text))
                .clone(),
            Constructor(constructor) => Type::Constructor(constructor),
            Hole => env.fresh_type_variable(),
        }
    }

    /// Creates a new type from an abstract syntax tree.
    pub fn new(abstr: crate::abstr::Type, env: &TypeEnv) -> Self {
        let vars = abstr
            .ftv()
            .into_iter()
            .map(|id| (id, env.fresh_type_variable()))
            .collect();

        Self::new_with_args(&vars, env, abstr)
    }

    pub fn generalize(self) -> Scheme {
        fn go(vars: &mut Vec<String>, value: Type) -> Type {
            use Type::*;

            match value {
                Any => Any,
                Pair(elements) => Pair(elements.into_iter().map(|element| go(vars, element)).collect()),
                Tuple(elements) => Tuple(elements.into_iter().map(|element| go(vars, element)).collect()),
                Fun(box domain, box codomain) => Fun(go(vars, domain).into(), go(vars, codomain).into()),
                App(name, box argument) => App(name, go(vars, argument).into()),
                Local(box local) => Local(go(vars, local).into()),
                Flexible(h @ Variable(idx, _)) => match h.value() {
                    Some(value) => value,
                    None => {
                        let var = letters().nth(idx).unwrap().to_string();
                        vars.push(var.clone());
                        Rigid(var)
                    }
                },
                Constructor(constructor) => Constructor(constructor),
                Rigid(m) => Rigid(m),
            }
        }

        let mut args = Vec::new();
        Scheme {
            mono: go(&mut args, self),
            args,
        }
    }

    pub fn unify(self, rhs: Type) -> Result<(), UnificationError> {
        use Type::*;
        use UnificationError::*;

        match (self, rhs) {
            (Any, _) | (_, Any) => Ok(()),
            (Local(box l_var), Local(box r_var)) => l_var.unify(r_var),
            (Constructor(l_con), Constructor(r_con)) if l_con.definition == r_con.definition => Ok(()),
            (App(l_name, box l_arg), App(r_name, box r_arg)) if l_name.definition == r_name.definition => {
                l_arg.unify(r_arg)
            }
            (Fun(box l_dom, box l_cod), Fun(box r_dom, box r_cod)) => {
                l_dom.unify(r_dom)?;
                l_cod.unify(r_cod)
            }
            (Pair(l_elements), Pair(r_elements)) | (Tuple(l_elements), Tuple(r_elements)) => {
                for (lhs, rhs) in l_elements.into_iter().zip(r_elements.into_iter()) {
                    lhs.unify(rhs)?;
                }
                Ok(())
            }
            (Flexible(h), value) | (value, Flexible(h)) => match h.value() {
                Some(contents) => contents.unify(value),
                None => {
                    value.pre_check(&h).unwrap();
                    h.update(value);
                    Ok(())
                }
            },
            (App(l_name, _), App(r_name, _)) => Err(IncompatibleConstructors(l_name, r_name)),
            (Constructor(l_con), Constructor(r_con)) => Err(IncompatibleConstructors(l_con, r_con)),
            (lhs, rhs) => Err(IncompatibleTypes(lhs, rhs)),
        }
    }

    fn pre_check(&self, hole: &Variable) -> Result<(), UnificationError> {
        use Type::*;
        use UnificationError::*;

        match self {
            Flexible(h) if h.0 == hole.0 => Err(OccursCheck {
                name: letters().nth(h.0).unwrap().to_string(),
                type_repr: self.clone(),
            }),
            Pair(elements) | Tuple(elements) => {
                for element in elements {
                    element.pre_check(hole)?;
                }
                Ok(())
            }
            Fun(box domain, box codomain) => {
                domain.pre_check(hole)?;
                codomain.pre_check(hole)
            }
            App(_, box argument) => argument.pre_check(hole),
            Local(box local) => local.pre_check(hole),
            _ => Ok(()),
        }
    }
}

struct Join<'a, 'b, T: Debug>(&'a Vec<T>, &'b str);

impl<'a, 'b, T: Debug> Debug for Join<'a, 'b, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, element) in self.0.iter().enumerate() {
            if idx > 0 {
                write!(f, "{}", self.1)?;
            }
            Debug::fmt(element, f)?;
        }
        Ok(())
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => write!(f, "*"),
            Type::Pair(elements) => write!(f, "{:?}", Join(elements, " * ")),
            Type::Tuple(elements) => write!(f, "({:?})", Join(elements, ", ")),
            Type::Fun(box domain, box codomain) => write!(f, "({:?} -> {:?})", domain, codomain),
            Type::App(reference, box argument) => write!(f, "{:?} {}", argument, reference.name.text),
            Type::Local(box local) => write!(f, "{:?}", local),
            Type::Constructor(constructor) => write!(f, "{}", constructor.name.text),
            Type::Rigid(idx) => write!(f, "?{idx}"),
            Type::Flexible(h) => match h.value() {
                Some(v) => write!(f, "{:?}", v),
                None => write!(f, "'{}", letters().nth(h.0).unwrap()),
            },
        }
    }
}

fn letters() -> impl Iterator<Item = String> {
    fn combinations(n: usize, alphabet: &[char]) -> Vec<String> {
        if n == 0 {
            return vec![String::new()];
        }

        let mut result = vec![];
        let smaller_combinations = combinations(n - 1, alphabet);

        for comb in smaller_combinations {
            for &letter in alphabet {
                let mut new_comb = comb.clone();
                new_comb.push(letter);
                result.push(new_comb);
            }
        }

        result
    }

    let alphabet: Vec<char> = ('a'..='z').collect();

    (1..).flat_map(move |length| combinations(length, &alphabet).into_iter())
}
