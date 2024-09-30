use crate::{
    abstr,
    concr::{errors::*, lowering::*, *},
};

pub fn lower_term(mut ctx: LoweringCtx, term: Term) -> miette::Result<abstr::Term> {
    ctx.burn();

    match term {
        SrcPos(box term, loc) => {
            ctx.src_pos = loc.clone();
            Ok(abstr::SrcPos(lower_term(ctx, term)?.into(), loc))
        }
        Int(value) => Ok(abstr::Int(value)),                               // 10
        Var(name) => Ok(abstr::Var(ctx.use_reference(ctx.lookup(name)?))), // x

        // fun x y z -> body
        Fun(parameters, box body) => {
            let mut ctx = ctx.clone();
            Ok(parameters
                .into_iter()
                .rev()
                .map(|parameter| ctx.new_variable(parameter))
                .collect::<Vec<_>>()
                .into_iter()
                .fold(lower_term(ctx, body)?, |acc, parameter| abstr::Fun(parameter, acc.into())))
        }

        // function
        // | Nil => 0
        // | Cons x xs => 1 + call xs
        Function(Function { cases }) => {
            let scrutinee = ctx.new_fresh_variable();
            let var = abstr::Var(ctx.use_reference(scrutinee.clone()));
            let cases = pat::lower_cases(ctx, cases);
            Ok(abstr::Fun(scrutinee, abstr::Match(var.into(), cases).into()))
        }

        // match list with
        // | Nil => 0
        // | Cons x xs => 1
        Match(Match { box scrutinee, cases }) => {
            let scrutinee = lower_term(ctx.clone(), scrutinee)?;
            let cases = pat::lower_cases(ctx, cases);
            Ok(abstr::Match(scrutinee.into(), cases))
        }

        // 0 : int
        BinOp(box lhs, BinOp::Colon, box rhs) => {
            let lhs = lower_term(ctx.clone(), lhs)?;
            let rhs = lower_type(ctx, rhs)?;
            Ok(abstr::Ascription(lhs.into(), rhs))
        }

        // 0 + 10
        BinOp(box lhs, BinOp::UserDefined(name), box rhs) => {
            let name = ctx.lookup(name)?;
            let name = ctx.use_reference(name);
            let lhs = lower_term(ctx.clone(), lhs)?;
            let rhs = lower_term(ctx, rhs)?;
            Ok(abstr::App(abstr::App(abstr::Var(name).into(), lhs.into()).into(), rhs.into()))
        }

        // let x y = 1 in x 10
        Let(Let {
            box pattern,
            parameters,
            box body,
            box next,
        }) => {
            let pattern = pat::lower_pattern(&mut ctx, pattern)?;
            let mut ctx = ctx.clone();

            let mut fun_type = abstr::Type::Hole;
            let mut patterns = vec![];
            for parameter in parameters.into_iter().rev() {
                let (pattern, type_repr) = lower_parameter(&mut ctx, parameter)?;
                fun_type = abstr::Type::Fun(type_repr.into(), fun_type.into());
                patterns.push(pattern);
            }
            let mut body = lower_term(ctx.clone(), body)?;
            let next = lower_term(ctx.clone(), next)?;

            for pattern in patterns.into_iter() {
                let name = ctx.new_fresh_variable();
                let default_case = abstr::Case { pattern, body };
                let new_body = abstr::Match(abstr::Var(ctx.use_reference(name.clone())).into(), vec![default_case]);
                body = abstr::Fun(name, new_body.into());
            }

            Ok(abstr::Match(abstr::Ascription(body.into(), fun_type).into(), vec![
                abstr::Case { pattern, body: next },
            ]))
        }

        // f x
        App(box callee, box argument) => {
            let callee = lower_term(ctx.clone(), callee)?;
            let argument = lower_term(ctx, argument)?;
            Ok(abstr::App(callee.into(), argument.into()))
        }

        // [x, y, z]
        Brackets(box elements) => {
            let elements = ctx
                .sep_by(BinOp::Comma, elements)?
                .into_iter()
                .filter_map(|term| ctx.or_none(lower_term(ctx.clone(), term)))
                .collect::<Vec<_>>();
            Ok(abstr::List(elements))
        }

        // (x) or (x, y, z, ..)
        Parens(box value) => {
            if let BinOp(_, BinOp::Comma, _) = value {
                let elements = ctx
                    .sep_by(BinOp::Comma, value)?
                    .into_iter()
                    .filter_map(|term| ctx.or_none(lower_term(ctx.clone(), term)))
                    .collect::<Vec<_>>();
                Ok(abstr::Pair(elements))
            } else {
                lower_term(ctx, value) // (x)
            }
        }

        Text(crate::loc::Text { value, loc }) => Ok(abstr::Text(crate::loc::Text { value, loc })),

        // if x then y else z
        If(If {
            box condition,
            box then,
            box otherwise,
        }) => {
            let condition = lower_term(ctx.clone(), condition)?;
            let then = lower_term(ctx.clone(), then)?;
            let otherwise = lower_term(ctx, otherwise)?;
            Ok(abstr::If(condition.into(), then.into(), otherwise.into()))
        }

        _ => match lower_type(ctx.clone(), term) {
            Ok(_) => ctx.wrap_error(TermSyntaxError),
            Err(_) => ctx.wrap_error(TypeSyntaxAtTermLevelError),
        },
    }
}

pub fn lower_type(mut ctx: LoweringCtx, term: Term) -> miette::Result<abstr::Type> {
    match term {
        SrcPos(box term, loc) => {
            ctx.src_pos = loc.clone();
            Ok(abstr::Type::SrcPos(lower_type(ctx, term)?.into(), loc))
        }
        Meta(name) => Ok(abstr::Type::Meta(name)),
        App(box argument, box callee) => {
            let argument = lower_type(ctx.clone(), argument)?;
            let callee = match lower_type(ctx.clone(), callee)? {
                abstr::Type::SrcPos(box abstr::Type::Constructor(callee), _) | abstr::Type::Constructor(callee) => {
                    callee
                }
                _ => ctx.wrap_error(TypeCalleeIsNotAConstructorError)?,
            };
            if callee.name.text == "local" {
                return Ok(abstr::Type::Local(argument.into()));
            }
            Ok(abstr::Type::App(callee, argument.into()))
        }
        Parens(box type_repr) => {
            if let BinOp(_, BinOp::Comma, _) = type_repr {
                let type_repr = ctx
                    .sep_by(BinOp::Comma, type_repr)?
                    .into_iter()
                    .filter_map(|term| ctx.or_none(lower_type(ctx.clone(), term)))
                    .collect::<Vec<_>>();
                Ok(abstr::Type::Tuple(type_repr))
            } else {
                lower_type(ctx, type_repr) // (x)
            }
        }
        BinOp(_, BinOp::Star, _) => {
            let elements = ctx
                .sep_by(BinOp::Star, term)?
                .into_iter()
                .filter_map(|term| ctx.or_none(lower_type(ctx.clone(), term)))
                .collect::<Vec<_>>();
            Ok(abstr::Type::Pair(elements))
        }
        BinOp(_, BinOp::Arrow, _) => Ok(ctx
            .sep_by(BinOp::Arrow, term)?
            .into_iter()
            .map(|term| match ctx.or_none(lower_type(ctx.clone(), term)) {
                Some(value) => value,
                None => abstr::Type::Hole,
            })
            .reduce(|acc, value| abstr::Type::Fun(value.into(), acc.into()))
            .expect("this should never fail because we have at least two elements.")),
        Var(name) => ctx
            .lookup_type(name)
            .map(|definition| abstr::Type::Constructor(ctx.use_reference(definition)))
            .into_diagnostic(),
        _ => ctx.wrap_error(TypeSyntaxError),
    }
}

pub fn lower_parameter(ctx: &mut LoweringCtx, parameter: Term) -> miette::Result<(abstr::Pattern, abstr::Type)> {
    ctx.burn();

    match parameter {
        SrcPos(box term, loc) => {
            ctx.src_pos = loc;
            lower_parameter(ctx, term)
        }
        Parens(box BinOp(box Var(name), BinOp::Colon, box type_repr)) => {
            let name = ctx.new_variable(name);
            let type_repr = lower_type(ctx.clone(), type_repr)?;
            Ok((abstr::Variable(name), type_repr))
        }
        Parens(_) => ctx.wrap_error(UnexpectedParameterAscriptionSyntaxError),
        Var(name) => Ok((abstr::Variable(ctx.new_variable(name)), abstr::Type::Hole)),
        _ => ctx.wrap_error(UnexpectedParameterSyntaxError),
    }
}

/// Pattern lowering uses mutable context to include the variables in the context.
pub mod pat {
    use super::*;

    pub fn lower_pattern(ctx: &mut LoweringCtx, case: Term) -> miette::Result<abstr::Pattern> {
        match case {
            SrcPos(box term, loc) => {
                ctx.src_pos = loc.clone();
                Ok(abstr::PatternSrcPos(lower_pattern(ctx, term)?.into(), loc))
            }
            Var(name) if name.text.chars().next().unwrap().is_uppercase() => {
                Ok(match ctx.lookup_constructor(name.clone()) {
                    Ok(def) => abstr::Constructor(ctx.use_reference(def), None),
                    Err(error) => {
                        let definition = ctx.new_variable(name.clone());
                        ctx.report_error(UncapitalizeVariableError {
                            name: name.text,
                            error: Some(error),
                        });
                        abstr::Variable(definition)
                    }
                })
            }
            Var(name) => Ok(abstr::Variable(ctx.new_variable(name))),
            App(box term, box arg) => {
                let term = lower_pattern(ctx, term)
                    .map_err(|error| PatternConstructorAppError { error })
                    .into_diagnostic()?;
                let (name, parameters) = match term {
                    abstr::PatternSrcPos(box abstr::Constructor(name, parameters), _)
                    | abstr::Constructor(name, parameters) => (name, parameters),
                    _ => return ctx.wrap_error(ExpectedConstructorError),
                };

                if parameters.is_some() {
                    ctx.report_error(PatternArgumentAlreadyExistsError);
                }

                let parameters = lower_pattern(ctx, arg)?;

                Ok(abstr::Constructor(name, Some(parameters.into())))
            }
            Parens(box varargs) => Ok(abstr::Elements(
                ctx.sep_by(BinOp::Comma, varargs)?
                    .into_iter()
                    .map(|term| lower_pattern(ctx, term))
                    .collect::<miette::Result<Vec<_>>>()?,
            )),
            _ => {
                ctx.report_error(UnexpectedPatternSyntaxError);
                let definition = ctx.new_variable(Identifier::new("_", ctx.src_pos.clone()));
                Ok(abstr::Variable(definition))
            }
        }
    }

    pub fn lower_case(mut ctx: LoweringCtx, case: Term) -> miette::Result<abstr::Case> {
        ctx.burn();

        match case {
            SrcPos(box term, loc) => {
                ctx.src_pos = loc;
                lower_case(ctx, term)
            }
            BinOp(box pattern, BinOp::DoubleArrow, box body) => {
                let pattern = lower_pattern(&mut ctx, pattern)?;
                let body = lower_term(ctx, body)?;
                Ok(abstr::Case { pattern, body })
            }
            _ => ctx.wrap_error(UnexpectedCaseSyntaxError),
        }
    }

    pub fn lower_cases(ctx: LoweringCtx, cases: Vec<Term>) -> Vec<abstr::Case> {
        cases
            .into_iter()
            .filter_map(|case| ctx.or_none(lower_case(ctx.clone(), case)))
            .collect()
    }
}

/// Lowering declarations uses mutable context to include the variables in the context.
pub mod decl {
    use super::*;

    #[allow(clippy::type_complexity)]
    pub struct Defer(pub Box<dyn FnOnce(&mut LoweringCtx) -> miette::Result<abstr::Decl>>);

    pub fn lower_decl(ctx: &mut LoweringCtx, term: Term) -> miette::Result<Defer> {
        Ok(match term {
            SrcPos(box term, loc) => {
                ctx.src_pos = loc;
                return lower_decl(ctx, term);
            }
            TypeDecl(TypeDecl {
                name,
                box variable,
                cases,
            }) => {
                let src_pos = ctx.src_pos.clone();
                let name = ctx.new_type(name);
                let variables = lower_type_parameter(ctx, variable);
                let cases = lower_constructors(ctx, cases);

                Defer(Box::new(|_| {
                    Ok(abstr::Decl::TypeDecl(abstr::TypeDecl {
                        name,
                        variables,
                        cases,
                        loc: src_pos,
                    }))
                }))
            }
            LetDecl(LetDecl {
                box pattern,
                box body,
                parameters,
            }) => {
                let src_pos = ctx.src_pos.clone();
                let pattern = pat::lower_pattern(ctx, pattern)?;

                Defer(Box::new(|ctx| {
                    let mut fun_type = abstr::Type::Hole;
                    let mut patterns = vec![];

                    for parameter in parameters.into_iter().rev() {
                        let (pattern, type_repr) = lower_parameter(ctx, parameter)?;
                        fun_type = abstr::Type::Fun(type_repr.into(), fun_type.into());
                        patterns.push(pattern);
                    }

                    let mut body = lower_term(ctx.clone(), body)?;

                    for pattern in patterns.into_iter() {
                        let name = ctx.new_fresh_variable();
                        let default_case = abstr::Case { pattern, body };
                        let new_body =
                            abstr::Match(abstr::Var(ctx.use_reference(name.clone())).into(), vec![default_case]);
                        body = abstr::Fun(name, new_body.into());
                    }

                    if let abstr::Variable(variable) = pattern {
                        Ok(abstr::Decl::LetDecl(abstr::LetDecl {
                            def: variable,
                            type_repr: fun_type,
                            body: abstr::Value(body),
                            loc: src_pos,
                        }))
                    } else {
                        let new_var = ctx.new_fresh_variable();
                        Ok(abstr::Decl::LetDecl(abstr::LetDecl {
                            def: ctx.new_fresh_variable(),
                            type_repr: fun_type,
                            body: abstr::Value(abstr::Match(body.into(), vec![abstr::Case {
                                pattern,
                                body: abstr::Var(ctx.use_reference(new_var)),
                            }])),
                            loc: src_pos,
                        }))
                    }
                }))
            }
            ValDecl(ValDecl { name, box type_repr }) => {
                let src_pos = ctx.src_pos.clone();
                let name = ctx.new_variable(name);
                Defer(Box::new(|ctx| {
                    let type_repr = lower_type(ctx.clone(), type_repr)?;
                    Ok(abstr::Decl::LetDecl(abstr::LetDecl {
                        def: name,
                        type_repr,
                        body: abstr::No,
                        loc: src_pos,
                    }))
                }))
            }
            _ => return ctx.wrap_error(DeclSyntaxError),
        })
    }

    pub fn lower_type_parameter(ctx: &mut LoweringCtx, term: Term) -> Vec<crate::loc::Identifier> {
        let mut variables = vec![];
        match lower_type(ctx.clone(), term) {
            Ok(abstr::Type::SrcPos(box abstr::Type::Tuple(elements), _) | abstr::Type::Tuple(elements)) => {
                for element in elements {
                    let abstr::Type::Meta(variable) = element else {
                        ctx.report_error(TypeSyntaxError);
                        continue;
                    };
                    variables.push(variable);
                }
            }
            Ok(abstr::Type::SrcPos(box abstr::Type::Meta(variable), _) | abstr::Type::Meta(variable)) => {
                variables.push(variable);
            }
            Ok(_) => ctx.report_error(TypeSyntaxError),
            Err(err) => ctx.report_direct_error(err),
        }
        variables
    }

    pub fn lower_constructors(ctx: &mut LoweringCtx, constructors: Vec<Term>) -> Vec<abstr::Constructor> {
        constructors
            .into_iter()
            .filter_map(|term| {
                let constructor = lower_constructor(ctx, term);
                ctx.or_none(constructor)
            })
            .collect()
    }

    pub fn lower_constructor(ctx: &mut LoweringCtx, term: Term) -> miette::Result<abstr::Constructor> {
        match term {
            SrcPos(box term, loc) => {
                ctx.src_pos = loc;
                lower_constructor(ctx, term)
            }
            Constructor(Constructor {
                name,
                type_repr: Some(box type_repr),
            }) => {
                let name = ctx.new_constructor(name);
                let type_repr = lower_type(ctx.clone(), type_repr)?;
                Ok(abstr::Constructor {
                    name,
                    type_repr: Some(type_repr),
                })
            }
            Constructor(Constructor { name, type_repr: None }) => {
                let name = ctx.new_constructor(name);
                Ok(abstr::Constructor { name, type_repr: None })
            }
            _ => ctx.wrap_error(ConstructorSyntaxError),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_f_x_in_x() {
        let ctx = LoweringCtx::new("".into(), "".into());
        crate::aux::golden_test! {
            expected:
r#"Ok(
    Match(
        Ascription(
            Fun(
                Definition {
                    name: Identifier {
                        text: "_1",
                        loc: Nowhere,
                    },
                    references: RefCell {
                        value: [
                            Identifier { text: "_1", loc: Nowhere },
                        ],
                    },
                    loc: Nowhere,
                },
                Match(
                    Var(
                        Identifier { text: "_1", loc: Nowhere },
                    ),
                    [
                        Case {
                            pattern: Variable(
                                Definition {
                                    name: Identifier {
                                        text: "x",
                                        loc: Nowhere,
                                    },
                                    references: RefCell {
                                        value: [
                                            Identifier { text: "x", loc: Nowhere },
                                        ],
                                    },
                                    loc: Nowhere,
                                },
                            ),
                            body: Var(
                                Identifier { text: "x", loc: Nowhere },
                            ),
                        },
                    ],
                ),
            ),
            Fun(
                Hole,
                Hole,
            ),
        ),
        [
            Case {
                pattern: Variable(
                    Definition {
                        name: Identifier {
                            text: "f",
                            loc: Nowhere,
                        },
                        references: RefCell {
                            value: [
                                Identifier { text: "f", loc: Nowhere },
                            ],
                        },
                        loc: Nowhere,
                    },
                ),
                body: Var(
                    Identifier { text: "f", loc: Nowhere },
                ),
            },
        ],
    ),
)"#,
            input: lower_term(ctx, Let(Let { pattern: Term::Var("f".into()).into(),
                                             parameters: vec![Term::Var("x".into())],
                                             body: Term::Var("x".into()).into(),
                                             next: Term::Var("f".into()).into() }))
        }
    }
}
