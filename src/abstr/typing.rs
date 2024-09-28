use std::cell::Cell;

use crate::{
    hir::{self, Poly},
    loc::Loc,
};

use super::*;

#[derive(Clone)]
pub struct TypeEnv {
    src_pos: Loc,
    types: HashMap<String, Rc<Definition>>,
    constructors: HashMap<String, Rc<Definition>>,
    assumptions: HashMap<String, hir::Poly>,
    errors: Rc<RefCell<Vec<miette::Report>>>,
    counter: Rc<Cell<usize>>,
}

impl TypeEnv {
    pub fn fresh_type_variable(&self) -> hir::Type {
        todo!()
    }

    pub fn get_type(&self, name: &str) -> Reference {
        self.types.get(name).unwrap().clone().use_reference()
    }

    pub fn extend(&self, name: String, poly: hir::Poly) -> Self {
        let mut new_type_env = self.clone();
        new_type_env.assumptions.insert(name, poly);
        new_type_env
    }

    pub fn unify_catch(&self, lhs: hir::Type, rhs: hir::Type) {
        todo!()
    }
}

pub fn infer(env: &TypeEnv, term: Term) -> hir::Term {
    match term {
        SrcPos(box term, src_pos) => infer(&TypeEnv { src_pos, ..env.clone() }, term),
        List(elements) => {
            let h = env.fresh_type_variable();
            let mut new_elements = vec![];
            for element in elements {
                let element = infer(env, element);
                env.unify_catch(h.clone(), element.type_repr.clone());
                new_elements.push(element);
            }
            hir::Term {
                type_repr: hir::Type::App(env.get_type("list"), h.into()),
                value: hir::TermKind::List(new_elements),
            }
        }
        Pair(elements) => {
            let elements = elements.into_iter().map(|term| infer(env, term)).collect::<Vec<_>>();

            hir::Term {
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
                value: hir::TermKind::Fun(parameter, body.into()),
            }
        }
        Match(_, _) => todo!(),
        Ascription(box term, type_repr) => check(env, term, hir::Poly::parse(type_repr)),
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
                value: hir::TermKind::App(callee.into(), argument.into()),
            }
        }
        Var(var) => match env.assumptions.get(&var.name.text) {
            Some(value) => hir::Term {
                type_repr: value.instantiate(),
                value: hir::TermKind::Var(var),
            },
            None => panic!("couldn't find term {}", var.name.text),
        },
        Int(i) => hir::Term {
            type_repr: hir::Type::Constructor(env.get_type("int")),
            value: hir::TermKind::Int(i),
        },
        Text(text) => hir::Term {
            type_repr: hir::Type::Constructor(env.get_type("string")),
            value: hir::TermKind::Text(text),
        },
        If(_, _, _) => todo!(),
        Let(_, _, _) => todo!(),
    }
}

#[rustfmt::skip]
pub fn check(env: &TypeEnv, term: Term, expected: Poly) -> hir::Term {
    match (term, expected) {
        (Term::Fun(_, _), Poly { mono: hir::Type::Fun(_, _), .. }) => {
            todo!()
        }
        _ => {
            todo!()
        }
    }
}
