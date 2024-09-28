use std::cell::Cell;

use miette::IntoDiagnostic;

use crate::{
    hir::{self, Poly},
    loc::Loc,
};

use super::*;

#[derive(Debug, Clone)]
struct Constructor {
    name: Arc<Definition>,
    type_repr: Option<Type>,
}

#[derive(Clone)]
pub struct TypeEnv {
    src_pos: Loc,
    types: im_rc::HashMap<String, Arc<Definition>>,
    constructors: im_rc::HashMap<String, Constructor>,
    assumptions: im_rc::HashMap<String, hir::Poly>,
    errors: Rc<RefCell<Vec<miette::Report>>>,
    counter: Rc<Cell<usize>>,
}

pub fn infer(env: &TypeEnv, term: Term) -> hir::Term {
    match term {
        SrcPos(box term, src_pos) => infer(&TypeEnv { src_pos, ..env.clone() }, term),

        // Γ ⊢ (x : a)
        // --------------
        // [x] : a list
        List(elements) => {
            let h = env.fresh_type_variable();
            let mut new_elements = vec![];
            for element in elements {
                let element = infer(env, element);
                env.unify_catch(h.clone(), element.type_repr.clone());
                new_elements.push(element);
            }
            hir::Term {
                src_pos: env.src_pos.clone(),
                type_repr: hir::Type::App(env.get_type("list"), h.into()),
                value: hir::TermKind::List(new_elements),
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
            let fun_env = env.extend(parameter.name.text.clone(), Poly::new(h));
            let body = infer(&fun_env, body);

            hir::Term {
                type_repr: body.type_repr.clone(),
                src_pos: env.src_pos.clone(),
                value: hir::TermKind::Fun(parameter, body.into()),
            }
        }
        Match(_, _) => todo!(),
        Ascription(box term, type_repr) => check(env, term, hir::Type::from(type_repr)),
        App(box callee, box argument) => {
            let h = env.fresh_type_variable();
            let callee = infer(env, callee);
            let argument = infer(env, argument);
            env.unify_catch(
                hir::Type::Fun(argument.type_repr.clone().into(), h.clone().into()),
                callee.type_repr.clone(),
            );

            hir::Term {
                type_repr: h,
                src_pos: env.src_pos.clone(),
                value: hir::TermKind::App(callee.into(), argument.into()),
            }
        }
        Var(var) => match env.assumptions.get(&var.name.text) {
            Some(value) => hir::Term {
                type_repr: value.instantiate(),
                src_pos: env.src_pos.clone(),
                value: hir::TermKind::Var(var),
            },
            None => panic!("couldn't find term {}", var.name.text),
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
            env.unify_catch(then.type_repr.clone(), otherwise.type_repr.clone());

            hir::Term {
                src_pos: env.src_pos.clone(),
                type_repr: then.type_repr.clone(),
                value: hir::TermKind::If(condition.into(), then.into(), otherwise.into()),
            }
        }
        Let(def, box value, box next) => {
            let value = infer(env, value);
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

pub fn check(env: &TypeEnv, term: Term, expected: hir::Type) -> hir::Term {
    match (term, expected) {
        (Term::SrcPos(box term, src_pos), expected) => check(&TypeEnv { src_pos, ..env.clone() }, term, expected),
        (Term::Fun(parameter, box body), hir::Type::Fun(box domain, box codomain)) => {
            let env = env.extend(parameter.name.text.clone(), hir::Poly::new(domain.clone()));
            let body = check(&env, body, codomain.clone());

            hir::Term {
                src_pos: env.src_pos.clone(),
                type_repr: hir::Type::Fun(domain.into(), codomain.into()),
                value: hir::TermKind::Fun(parameter, body.into()),
            }
        }
        (term, expected) => {
            let term = infer(env, term);
            env.unify_catch(expected, term.type_repr.clone());
            term
        }
    }
}

impl TypeEnv {
    pub fn fresh_type_variable(&self) -> hir::Type {
        hir::Type::Hole(hir::Variable::default())
    }

    pub fn get_type(&self, name: &str) -> Reference {
        self.types.get(name).unwrap().clone().use_reference()
    }

    pub fn extend(&self, name: String, poly: hir::Poly) -> Self {
        let mut new_type_env = self.clone();
        new_type_env.assumptions.insert(name, poly);
        new_type_env
    }

    pub fn report(&self, error: miette::Report) {
        self.errors.borrow_mut().push(error);
    }

    pub fn unify_catch(&self, lhs: hir::Type, rhs: hir::Type) {
        if let Err(err) = lhs.unify(rhs).into_diagnostic() {
            self.report(err);
        }
    }
}
