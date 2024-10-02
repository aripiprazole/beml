use std::cell::Cell;

use fxhash::FxBuildHasher;
use miette::NamedSource;

use crate::{
    errors::LoweringError,
    hir::{self, Scheme, Typeable},
    loc::Loc,
};

use super::*;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unresolved variable: {name}")]
pub struct UnresolvedVariableError {
    pub name: String,
}

#[derive(Clone)]
pub struct TypeEnv {
    pub(crate) file: PathBuf,
    pub(crate) text: String,
    pub(crate) src_pos: Loc,
    pub(crate) types: im_rc::HashMap<String, Rc<hir::AlgebraicDataType>, FxBuildHasher>,
    pub(crate) constructors_to_types: im_rc::HashMap<String, Rc<hir::AlgebraicDataType>, FxBuildHasher>,
    pub(crate) assumptions: im_rc::HashMap<String, hir::Scheme, FxBuildHasher>,
    pub(crate) errors: Rc<RefCell<Vec<miette::Report>>>,
    pub(crate) counter: Rc<Cell<usize>>,
}

/// Infers the type of a term. It returns a [crate::hir::Term] with the
/// new type and location information.
pub fn infer(env: &TypeEnv, term: Term) -> hir::Term {
    match term {
        SrcPos(box term, src_pos) => infer(&TypeEnv { src_pos, ..env.clone() }, term),

        // Γ ⊢ (x : a)
        // --------------
        // [x] : a list
        List(elements) => {
            let h = env.fresh_type_variable();
            let elements = elements
                .into_iter()
                .map(|element| {
                    let element = infer(env, element);
                    env.unify_catch(&h, &element);
                    element
                })
                .collect::<Vec<_>>();

            hir::Term {
                src_pos: env.src_pos.clone(),
                value: hir::TermKind::List(elements),
                type_repr: hir::app_type(env, env.get_type("list"), h),
            }
        }
        Pair(elements) => {
            let elements = elements.into_iter().map(|term| infer(env, term)).collect::<Vec<_>>();

            hir::Term {
                src_pos: env.src_pos.clone(),
                value: hir::TermKind::Pair(elements.clone()),
                type_repr: hir::Type::Pair(elements.into_iter().map(|t| t.type_repr).collect()),
            }
        }
        Fun(parameter, box body) => {
            let h = env.fresh_type_variable();
            let fun_env = env.extend(parameter.name.text.clone(), Scheme::new(h.clone()));
            let body = infer(&fun_env, body);
            let type_repr = hir::Type::Fun(h.into(), body.type_repr.clone().into());

            hir::Term {
                type_repr,
                src_pos: env.src_pos.clone(),
                value: hir::TermKind::Fun(parameter, body.into()),
            }
        }
        Match(box scrutinee, cases) => {
            let scrutinee = infer(env, scrutinee);
            let h = env.fresh_type_variable();
            let cases = cases
                .into_iter()
                .map(|Case { pattern, body }| {
                    let mut ctx = HashMap::default();
                    let mut local_env = env.clone();
                    pat::check_pat(&mut local_env, &mut ctx, pattern.clone(), scrutinee.type_repr.clone());
                    let body = infer(&local_env, body);
                    env.unify_catch(&h, &body);
                    pat::Case { pattern, body }
                })
                .collect::<Vec<_>>();

            hir::Term {
                type_repr: h,
                src_pos: env.src_pos.clone(),
                value: hir::TermKind::Match(pat::specialize(scrutinee, cases)),
            }
        }
        Ascription(box term, type_repr) => check(env, term, hir::Type::new(type_repr, env)),
        App(box callee, box argument) => {
            let h = env.fresh_type_variable();
            let callee = infer(env, callee);
            let argument = infer(env, argument);
            env.unify_catch(&hir::fun_type(&argument.type_repr, &h), &callee);

            hir::Term {
                type_repr: h,
                src_pos: env.src_pos.clone(),
                value: hir::TermKind::App(callee.into(), argument.into()),
            }
        }
        Var(var) => match env.assumptions.get(&var.name.text) {
            Some(value) => hir::Term {
                type_repr: value.instantiate(env),
                src_pos: env.src_pos.clone(),
                value: hir::TermKind::Var(var),
            },
            None => {
                env.report(UnresolvedVariableError { name: var.name.text });

                hir::Term {
                    type_repr: hir::Type::Any,
                    src_pos: env.src_pos.clone(),
                    value: hir::TermKind::Error,
                }
            }
        },
        Int(i) => hir::Term {
            type_repr: hir::Type::Constructor(env.get_type("int")),
            src_pos: env.src_pos.clone(),
            value: hir::TermKind::Int(i),
        },
        Text(text) => hir::Term {
            type_repr: hir::Type::Constructor(env.get_type("string")),
            src_pos: env.src_pos.clone(),
            value: hir::TermKind::Text(text),
        },
        If(box condition, box then, box otherwise) => {
            let condition = check(env, condition, hir::Type::Constructor(env.get_type("bool")));
            let then = infer(env, then);
            let otherwise = infer(env, otherwise);
            env.unify_catch(&then, &otherwise.type_repr);

            hir::Term {
                src_pos: env.src_pos.clone(),
                type_repr: then.type_repr.clone(),
                value: hir::TermKind::If(condition.into(), then.into(), otherwise.into()),
            }
        }
        Let(def, box value, box next) => {
            let trial = hir::Scheme::new(env.fresh_type_variable());
            let value = infer(&env.extend(def.name.text.clone(), trial), value);
            let type_repr = value.type_repr.clone().generalize();
            let next = infer(&env.extend(def.name.text.clone(), type_repr), next);

            hir::Term {
                src_pos: env.src_pos.clone(),
                type_repr: next.type_repr.clone(),
                value: hir::TermKind::Let(def, value.into(), next.into()),
            }
        }
    }
}

/// Checks the type of a term against an expected type. It returns a [crate::hir::Term]
/// with the new type and location information.
///
/// It also checks that the type is compatible with the expected type, and reports errors
/// if it is not.
pub fn check(env: &TypeEnv, term: Term, expected: hir::Type) -> hir::Term {
    match (term, expected.force()) {
        (Term::SrcPos(box term, src_pos), expected) => check(&TypeEnv { src_pos, ..env.clone() }, term, expected),
        (Term::Fun(parameter, box body), hir::Type::Fun(box domain, box codomain)) => {
            let env = env.extend(parameter.name.text.clone(), hir::Scheme::new(domain.clone()));
            let body = check(&env, body, codomain.clone());

            hir::Term {
                src_pos: env.src_pos.clone(),
                type_repr: hir::Type::Fun(domain.into(), codomain.into()),
                value: hir::TermKind::Fun(parameter, body.into()),
            }
        }
        (term, expected) => {
            let term = infer(env, term);
            env.unify_catch(&expected, &term);
            term
        }
    }
}

/// Declaration inference, used for statement checking and declaration checking
/// in let expressions.
pub(crate) mod decl {
    use super::*;

    #[allow(clippy::type_complexity)]
    pub struct Defer(pub Box<dyn FnOnce(&mut TypeEnv) -> Option<hir::Term>>);

    pub fn infer_decl(env: &mut TypeEnv, decl: Decl) -> Defer {
        match decl {
            Decl::TypeDecl(decl) => {
                let mut vars = HashMap::default();
                let variables = decl
                    .variables
                    .clone()
                    .into_iter()
                    .map(|variable| {
                        vars.entry(variable.text.clone())
                            .or_insert_with(|| env.fresh_type_variable())
                            .clone()
                    })
                    .collect::<Vec<_>>();
                let scheme = match variables.as_slice() {
                    [] => hir::Type::Constructor(decl.def.clone().use_at(env)),
                    [variable] => hir::Type::App(decl.def.clone().use_at(env), variable.clone().into()),
                    _ => hir::Type::App(decl.def.clone().use_at(env), hir::Type::Tuple(variables).into()),
                };
                let adt = Rc::new(hir::AlgebraicDataType {
                    definition: decl.def.clone(),
                    arity: decl.variables.len(),
                    scheme: scheme.clone().generalize(),
                    constructors: RefCell::new(Default::default()),
                });

                env.src_pos = decl.loc.clone();
                env.types.insert(decl.def.name.text.clone(), adt.clone());

                Defer(Box::new(move |env| {
                    for Constructor { name: def, type_repr } in decl.cases {
                        let constructor_name = def.name.text.clone();
                        let mut constructors = adt.constructors.borrow_mut();
                        env.constructors_to_types.insert(constructor_name.clone(), adt.clone());
                        if let Some(type_repr) = type_repr {
                            let type_repr = hir::Type::new_with_args(&vars, env, type_repr);
                            let constructor_type = hir::fun_type(&type_repr, &scheme).generalize();
                            env.assumptions.insert(def.name.text.clone(), constructor_type.clone());
                            constructors.insert(constructor_name, hir::Constructor {
                                type_repr: Some(type_repr),
                                scheme: constructor_type,
                            });
                        } else {
                            env.assumptions.insert(constructor_name, scheme.clone().generalize());
                            constructors.insert(def.name.text.clone(), hir::Constructor {
                                type_repr: None,
                                scheme: scheme.clone().generalize(),
                            });
                        }
                    }

                    None
                }))
            }
            Decl::LetDecl(decl) => {
                env.src_pos = decl.loc.clone();
                let tt = hir::Type::new(decl.type_repr, env);
                let Body::Value(term) = decl.body else {
                    let scheme = tt.generalize();
                    env.assumptions.insert(decl.def.name.text.clone(), scheme);
                    return Defer(Box::new(|_| None));
                };

                let h = env.fresh_type_variable();
                env.unify_catch(&h, &tt);
                env.assumptions
                    .insert(decl.def.name.text.clone(), Scheme::new(h.clone()));

                Defer(Box::new(move |env| {
                    let value = infer(env, term);
                    let type_repr = value.type_repr.clone().generalize();
                    env.assumptions.insert(decl.def.name.text.clone(), type_repr);
                    Some(value)
                }))
            }
        }
    }
}

/// Pattern inference, used for coverage checking and pattern checking
/// in match expressions.
pub(crate) mod pat {
    use crate::abstr::errors::{IncompatiblePatternTypeError, UnresolvedConstructorError};

    use super::*;

    pub type Pats<'a> = &'a mut HashMap<String, hir::Type, FxBuildHasher>;

    pub struct Case {
        pub pattern: Pattern,
        pub body: hir::Term,
    }

    /// Compiles a pattern into a [hir::CaseTree].
    ///
    /// ```ocaml
    /// match x with
    /// | Cons (Cons (x, xs), xs) => true
    /// | Cons (Nil, xs) => false
    /// | Nil => false
    /// ```
    ///
    /// Will translate into the pseudo-code:
    ///
    /// ```rust,ignore
    /// CaseTree::Branch {
    ///    occurence: Reference(x),
    ///    cases: vec![
    ///        (Condition::Constructor(Reference(Cons), vec![Occurence::Index(0), Occurence::Variable(xs)]), CaseTree::Branch {
    ///            occurence: Occurence::Index(0),
    ///            cases: vec![
    ///                (Condition::Constructor(Reference(Cons), vec![Occurence::Variable(x), Occurence::Variable(xs)]), CaseTree::Leaf(true)),
    ///                (Condition::Constructor(Reference(Nil)), CaseTree::Leaf(false)),
    ///            ],
    ///            default: None,
    ///        }),
    ///        (Condition::Constructor(Reference(Nil)), CaseTree::Leaf(false)),
    ///    ],
    ///    default: None,
    /// }
    /// ```
    ///
    /// And this more complex example:
    ///
    /// ```ocaml
    /// match x with
    /// | Cons (Cons (x, xs), (Cons x', xs')) => true
    /// | Cons (Nil, xs) => false
    /// | Nil => false
    /// ```
    ///
    /// Will translate into the pseudo-code:
    ///
    /// ```rust,ignore
    /// CaseTree::Branch {
    ///    occurence: Reference(x),
    ///    cases: vec![
    ///        (Condition::Constructor(Reference(Cons), vec![Occurence::Index(0), Occurence::Index(1)]), CaseTree::Branch {
    ///            occurence: Occurence::Index(0),
    ///            cases: vec![
    ///                (Condition::Constructor(Reference(Cons), vec![Occurence::Variable(x), Occurence::Variable(xs)]), CaseTree::Branch {
    ///                    occurence: Occurence::Index(1),
    ///                    cases: vec![
    ///                        (Condition::Constructor(Reference(Cons), vec![Occurence::Variable(x'), Occurence::Variable(xs')]), CaseTree::Leaf(true)),
    ///                        (Condition::Constructor(Reference(Nil)), CaseTree::Leaf(false)),
    ///                    ],
    ///                    default: None,
    ///                }),
    ///                (Condition::Constructor(Reference(Nil)), CaseTree::Leaf(false)),
    ///            ],
    ///            default: None,
    ///        }),
    ///        (Condition::Constructor(Reference(Nil)), CaseTree::Leaf(false)),
    ///    ],
    ///    default: None,
    /// }
    /// ```
    pub fn specialize(scrutinee: hir::Term, cases: Vec<Case>) -> hir::CaseTree {
        type Specialization = (Option<hir::Condition>, hir::CaseTree);

        fn into_case_tree(idx: usize, (condition, case_tree): Specialization) -> hir::CaseTree {
            match condition {
                Some(condition) => hir::CaseTree::Branch {
                    occurence: hir::Occurrence::Index(idx),
                    cases: vec![(condition, case_tree)],
                    default: None,
                },
                None => hir::CaseTree::Branch {
                    occurence: hir::Occurrence::Index(idx),
                    cases: vec![],
                    default: Some(case_tree.into()),
                },
            }
        }

        fn go(loc: Loc, idx: usize, pattern: Pattern, body: hir::CaseTree) -> Specialization {
            match pattern {
                PatternSrcPos(box pattern, src_pos) => go(src_pos, idx, pattern, body),
                Variable(var) => (None, hir::CaseTree::Branch {
                    occurence: hir::Occurrence::Variable(var),
                    cases: vec![],
                    default: Some(Box::new(body)),
                }),
                Constructor(constructor, None) => (Some(hir::Condition::Constructor(constructor, None)), body),
                Constructor(constructor, Some(box pattern)) => (
                    Some(hir::Condition::Constructor(constructor, Some(idx + 1))),
                    into_case_tree(idx + 1, go(loc, idx + 1, pattern, body)),
                ),
                Elements(elements) => {
                    let mut target = body;
                    for (sub_index, pattern) in elements.iter().enumerate() {
                        let (condition, case_tree) = go(loc.clone(), idx + sub_index, pattern.clone(), target.clone());
                        target = match condition {
                            Some(condition) => hir::CaseTree::Branch {
                                occurence: hir::Occurrence::Tuple(idx, sub_index),
                                cases: vec![(condition, case_tree)],
                                default: None,
                            },
                            None => hir::CaseTree::Branch {
                                occurence: hir::Occurrence::Tuple(idx, sub_index),
                                cases: vec![],
                                default: Some(case_tree.into()),
                            },
                        };
                    }

                    (Some(hir::Condition::Tuple(elements.len())), target)
                }
            }
        }

        let cases = cases
            .into_iter()
            .enumerate()
            .map(|(idx, Case { pattern, body })| go(Loc::Nowhere, idx, pattern, hir::CaseTree::Leaf(body.into())))
            .collect::<Vec<_>>();

        let mut default = None;
        let mut relevant_cases = vec![];
        for (condition, case_tree) in cases {
            match condition {
                Some(condition) => relevant_cases.push((condition, case_tree)),
                None => {
                    default = Some(Box::new(case_tree));
                    break; // TODO: report irrelevance
                }
            }
        }

        hir::CaseTree::Branch {
            occurence: hir::Occurrence::Term(Box::new(scrutinee)),
            cases: relevant_cases,
            default,
        }
    }

    /// Infers the type of a pattern. It returns a [hir::Type] with the new type.
    pub fn infer_pat(env: &mut TypeEnv, ctx: Pats, pat: Pattern) -> hir::Type {
        match pat {
            PatternSrcPos(box pat, src_pos) => {
                env.src_pos = src_pos;
                infer_pat(env, ctx, pat)
            }
            Variable(var) => {
                let tt = ctx
                    .entry(var.name.text.clone())
                    .or_insert_with(|| env.fresh_type_variable())
                    .clone();
                env.assumptions.insert(var.name.text.clone(), Scheme::new(tt.clone()));
                tt
            }
            Constructor(ref constructor, Some(box pat)) => {
                let Some(adt) = env.constructors_to_types.get(&constructor.name.text).cloned() else {
                    env.report(IncompatiblePatternTypeError);
                    return hir::Type::Any;
                };
                let Some(hir::Constructor { scheme, .. }) = adt.get(&constructor.name.text) else {
                    env.report(UnresolvedConstructorError {
                        name: constructor.name.text.clone(),
                    });
                    return hir::Type::Any;
                };

                let scheme_type = adt.scheme.instantiate(env);
                let fun_type = hir::fun_type(&infer_pat(env, ctx, pat), &scheme_type);
                env.unify_catch(&fun_type, &scheme.instantiate(env));
                scheme_type
            }
            Constructor(ref constructor, None) => {
                let Some(adt) = env.constructors_to_types.get(&constructor.name.text).cloned() else {
                    env.report(IncompatiblePatternTypeError);
                    return hir::Type::Any;
                };
                let Some(hir::Constructor { scheme, .. }) = adt.get(&constructor.name.text) else {
                    env.report(UnresolvedConstructorError {
                        name: constructor.name.text.clone(),
                    });
                    return hir::Type::Any;
                };
                let tt = scheme.instantiate(env);
                env.unify_catch(&tt, &adt.scheme.instantiate(env));
                tt
            }
            Elements(elements) => hir::Type::Pair(
                elements
                    .into_iter()
                    .map(|element| infer_pat(env, ctx, element))
                    .collect(),
            ),
        }
    }

    /// Checks pattern against expected type. It returns the expected type.
    pub fn check_pat(env: &mut TypeEnv, ctx: Pats, pat: Pattern, expected: hir::Type) -> hir::Type {
        match (pat, expected) {
            (PatternSrcPos(box pat, src_pos), expected) => {
                env.src_pos = src_pos;
                check_pat(env, ctx, pat, expected)
            }
            (Elements(elements), hir::Type::Pair(element_types)) => {
                let elements = elements
                    .into_iter()
                    .zip(element_types)
                    .map(|(element, element_type)| check_pat(env, ctx, element, element_type))
                    .collect::<Vec<_>>();

                hir::Type::Pair(elements)
            }
            (Variable(var), expected) => {
                env.assumptions
                    .insert(var.name.text.clone(), Scheme::new(expected.clone()));
                expected
            }
            (term, expected) => {
                let inferred = infer_pat(env, ctx, term);
                env.unify_catch(&expected, &inferred);
                expected
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_specialize() {
            let env = TypeEnv::new(Default::default(), Default::default());
            let cons = Definition::new("Cons");
            let nil = Definition::new("Nil");

            let scrutinee = infer(&env, Term::List(vec![]));
            dbg!(specialize(scrutinee, vec![
                Case {
                    pattern: Constructor(
                        cons.clone().use_at(&HasNowhere),
                        Some(Box::new(Elements(vec![
                            Constructor(
                                cons.clone().use_at(&HasNowhere),
                                Some(Box::new(Elements(vec![
                                    Variable(Definition::new("x")),
                                    Variable(Definition::new("xs")),
                                ])))
                            ),
                            Constructor(
                                cons.clone().use_at(&HasNowhere),
                                Some(Box::new(Elements(vec![
                                    Variable(Definition::new("x'")),
                                    Variable(Definition::new("xs'")),
                                ])))
                            )
                        ])))
                    ),
                    body: infer(&env, Term::Int(42))
                },
                Case {
                    pattern: Constructor(nil.use_at(&HasNowhere), None),
                    body: infer(&env, Term::Int(42))
                }
            ]));
        }
    }
}

macro_rules! intrinsic_algebraic_data_type {
    ($types:expr, $name:ident) => {{
        let env = $types;
        let definition = Definition::new(stringify!($name));
        let adt = Rc::new(hir::AlgebraicDataType {
            definition: definition.clone(),
            arity: 0,
            scheme: hir::Scheme::new(hir::Type::Constructor(definition.clone().use_at(env))),
            constructors: Default::default(),
        });
        env.types.insert(definition.name.text.clone(), adt.clone());
    }};
}

impl TypeEnv {
    pub fn new(file: PathBuf, text: String) -> Self {
        let mut types = Self {
            file,
            text,
            src_pos: Loc::Nowhere,
            types: im_rc::HashMap::default(),
            constructors_to_types: Default::default(),

            assumptions: Default::default(),
            errors: Rc::new(RefCell::new(vec![])),
            counter: Rc::new(Cell::new(0)),
        };

        intrinsic_algebraic_data_type!(&mut types, int);
        intrinsic_algebraic_data_type!(&mut types, bool);
        intrinsic_algebraic_data_type!(&mut types, string);

        types
    }

    pub fn fresh_type_variable(&self) -> hir::Type {
        let idx = self.counter.get();
        self.counter.set(idx + 1);
        hir::Type::Flexible(hir::Variable::new(self.counter.get()))
    }

    pub fn get_type(&self, name: &str) -> Reference {
        self.types
            .get(name)
            .unwrap_or_else(|| panic!("intrinsic type {name} not found"))
            .clone()
            .definition
            .clone()
            .use_at(self)
    }

    pub fn extend(&self, name: String, poly: hir::Scheme) -> Self {
        let mut new_type_env = self.clone();
        new_type_env.assumptions.insert(name, poly);
        new_type_env
    }

    pub fn report_direct_error(&self, error: miette::Report) {
        self.errors.borrow_mut().push(error);
    }

    pub fn report(&self, error: impl miette::Diagnostic + Send + Sync + 'static) {
        self.report_direct_error(self.wrap_error::<(), _>(error).unwrap_err());
    }

    pub fn unify_catch<A: Typeable, B: Typeable>(&self, lhs: &A, rhs: &B) {
        let lhs = lhs.type_of();
        let rhs = rhs.type_of();
        tracing::trace!("unify_catch: {:?}, {:?}", lhs, rhs);
        if let Err(err) = lhs.unify(rhs) {
            self.report(err);
        }
    }

    /// Wraps the error with the source code and location
    pub fn wrap_error<A, T>(&self, source: T) -> miette::Result<A>
    where
        T: miette::Diagnostic + std::error::Error + Send + Sync + 'static, {
        Err(LoweringError {
            loc: self.src_pos.clone(),
            source_code: NamedSource::new(self.file.to_str().unwrap(), self.text.clone()),
            source,
        })?
    }
}

impl HasLocation for TypeEnv {
    fn src_pos(&self) -> crate::loc::Loc {
        self.src_pos.clone()
    }
}
