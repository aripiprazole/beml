use miette::IntoDiagnostic;

use super::*;

pub fn lower_term(mut ctx: LoweringCtx, term: Term) -> miette::Result<abstr::Term> {
    ctx.burn();

    match term {
        SrcPos(box term, loc) => {
            ctx.src_pos = loc.clone();
            Ok(abstr::SrcPos(lower_term(ctx, term)?.into(), loc))
        }
        Int(value) => Ok(abstr::Int(value)),                         // 10
        Var(name) => Ok(abstr::Var(ctx.lookup(name)?.use_at(&ctx))), // x

        // fun x y z -> body
        Fun(parameters, box body) => {
            let mut ctx = ctx.clone();
            let parameters = parameters
                .into_iter()
                .rev()
                .map(|parameter| ctx.new_variable(parameter))
                .collect::<Vec<_>>();
            Ok(parameters
                .into_iter()
                .fold(lower_term(ctx, body)?, |acc, parameter| abstr::Fun(parameter, acc.into())))
        }

        // function
        // | Nil => 0
        // | Cons x xs => 1 + call xs
        Function(Function { cases }) => {
            let scrutinee = ctx.new_fresh_variable();
            let var = abstr::Var(scrutinee.clone().use_at(&ctx));
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
            let name = ctx.lookup(name)?.use_at(&ctx);
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
            let pattern = pat::lower_pat(&mut ctx, pattern)?;
            let mut ctx = ctx.clone();

            let mut fun_type = abstr::TypeRepr::Hole;
            let mut patterns = vec![];
            for parameter in parameters.into_iter().rev() {
                let (pattern, type_repr) = lower_parameter(&mut ctx, parameter)?;
                fun_type = abstr::TypeRepr::Fun(type_repr.into(), fun_type.into());
                patterns.push(pattern);
            }
            let mut body = lower_term(ctx.clone(), body)?;
            let next = lower_term(ctx.clone(), next)?;

            for pattern in patterns.into_iter() {
                let name = ctx.new_fresh_variable();
                let default_case = abstr::Case { pattern, body };
                let new_body = abstr::Match(abstr::Var(name.clone().use_at(&ctx)).into(), vec![default_case]);
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
        Brackets(Some(box elements)) => {
            let elements = ctx
                .sep_by(BinOp::Comma, elements)?
                .into_iter()
                .filter_map(|term| ctx.or_none(lower_term(ctx.clone(), term)))
                .collect::<Vec<_>>();
            Ok(abstr::List(elements))
        }
        Brackets(None) => Ok(abstr::List(vec![])), // []

        // (x, y, z, ..)
        Parens(Some(value @ box BinOp(_, BinOp::Comma, _) | value @ box SrcPos(box BinOp(_, BinOp::Comma, _), _))) => {
            let elements = ctx
                .sep_by(BinOp::Comma, *value)?
                .into_iter()
                .filter_map(|term| ctx.or_none(lower_term(ctx.clone(), term)))
                .collect::<Vec<_>>();
            Ok(abstr::Pair(elements))
        }
        Parens(None) => Ok(abstr::Pair(vec![])),           // () or unit
        Parens(Some(box value)) => lower_term(ctx, value), // (x)

        Text(beml_tree::loc::Text { value, loc }) => Ok(abstr::Text(beml_tree::loc::Text { value, loc })),

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

pub fn lower_type(mut ctx: LoweringCtx, term: Term) -> miette::Result<abstr::TypeRepr> {
    match term {
        SrcPos(box term, loc) => {
            ctx.src_pos = loc.clone();
            Ok(abstr::TypeRepr::SrcPos(lower_type(ctx, term)?.into(), loc))
        }
        Meta(name) => Ok(abstr::TypeRepr::Meta(name)),
        App(box argument, box callee) => {
            let argument = lower_type(ctx.clone(), argument)?;
            let callee = match lower_type(ctx.clone(), callee)? {
                abstr::TypeRepr::SrcPos(box abstr::TypeRepr::Constructor(callee), _)
                | abstr::TypeRepr::Constructor(callee) => callee,
                _ => ctx.wrap_error(TypeCalleeIsNotAConstructorError)?,
            };
            if callee.name.text == "local" {
                return Ok(abstr::TypeRepr::Local(argument.into()));
            }
            Ok(abstr::TypeRepr::App(callee, argument.into()))
        }
        Parens(Some(box type_repr)) => {
            if let BinOp(_, BinOp::Comma, _) = type_repr {
                let type_repr = ctx
                    .sep_by(BinOp::Comma, type_repr)?
                    .into_iter()
                    .filter_map(|term| ctx.or_none(lower_type(ctx.clone(), term)))
                    .collect::<Vec<_>>();
                Ok(abstr::TypeRepr::Tuple(type_repr))
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
            Ok(abstr::TypeRepr::Pair(elements))
        }
        BinOp(_, BinOp::Arrow, _) => Ok(ctx
            .sep_by(BinOp::Arrow, term)?
            .into_iter()
            .map(|term| match ctx.or_none(lower_type(ctx.clone(), term)) {
                Some(value) => value,
                None => abstr::TypeRepr::Hole,
            })
            .reduce(|acc, value| abstr::TypeRepr::Fun(value.into(), acc.into()))
            .expect("this should never fail because we have at least two elements.")),
        Var(name) => ctx
            .lookup_type(name)
            .map(|definition| abstr::TypeRepr::Constructor(definition.use_at(&ctx)))
            .into_diagnostic(),
        _ => ctx.wrap_error(TypeSyntaxError),
    }
}

pub fn lower_parameter(ctx: &mut LoweringCtx, parameter: Term) -> miette::Result<(abstr::Pattern, abstr::TypeRepr)> {
    ctx.burn();

    match parameter {
        SrcPos(box term, loc) => {
            ctx.src_pos = loc;
            lower_parameter(ctx, term)
        }
        Parens(Some(box BinOp(box Var(name), BinOp::Colon, box type_repr))) => {
            let name = ctx.new_variable(name);
            let type_repr = lower_type(ctx.clone(), type_repr)?;
            Ok((abstr::Variable(name), type_repr))
        }
        Parens(_) => ctx.wrap_error(UnexpectedParameterAscriptionSyntaxError),
        Var(name) => Ok((abstr::Variable(ctx.new_variable(name)), abstr::TypeRepr::Hole)),
        _ => ctx.wrap_error(UnexpectedParameterSyntaxError),
    }
}

/// Pattern lowering uses mutable context to include the variables in the context.
pub mod pat {
    use miette::IntoDiagnostic;

    use super::*;

    pub fn lower_pat(ctx: &mut LoweringCtx, case: Term) -> miette::Result<abstr::Pattern> {
        match case {
            SrcPos(box term, loc) => {
                ctx.src_pos = loc.clone();
                Ok(abstr::PatternSrcPos(lower_pat(ctx, term)?.into(), loc))
            }
            Var(name) if name.text.chars().next().unwrap().is_uppercase() => {
                Ok(match ctx.lookup_constructor(name.clone()) {
                    Ok(def) => abstr::Constructor(def.use_at(ctx), None),
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
                let term = lower_pat(ctx, term)
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

                let parameters = lower_pat(ctx, arg)?;

                Ok(abstr::Constructor(name, Some(parameters.into())))
            }
            Parens(Some(box varargs)) => Ok(abstr::Elements(
                ctx.sep_by(BinOp::Comma, varargs)?
                    .into_iter()
                    .map(|term| lower_pat(ctx, term))
                    .collect::<miette::Result<Vec<_>>>()?,
            )),
            Parens(None) => Ok(abstr::Elements(vec![])),
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
                let pattern = lower_pat(&mut ctx, pattern)?;
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
    use abstr::HasNowhere;

    use super::*;

    #[allow(clippy::type_complexity)]
    pub struct Defer(pub Box<dyn FnOnce(&mut LoweringCtx) -> miette::Result<abstr::Decl>>);

    fn desestruct(pattern: abstr::Pattern, scrutinee: abstr::Term) -> im_rc::HashMap<String, abstr::Term> {
        fn ftv(pattern: &abstr::Pattern) -> im_rc::HashSet<Arc<abstr::Definition>> {
            match pattern {
                abstr::PatternSrcPos(box pattern, _) => ftv(pattern),
                abstr::Variable(var) => {
                    let mut vars = im_rc::HashSet::new();
                    vars.insert(var.clone());
                    vars
                }
                abstr::Constructor(_, parameters) => match parameters {
                    Some(parameters) => ftv(parameters),
                    None => im_rc::HashSet::new(),
                },
                abstr::Elements(elements) => elements.iter().flat_map(ftv).collect(),
            }
        }

        let mut bindings = im_rc::HashMap::new();
        for definition in ftv(&pattern) {
            bindings.insert(
                definition.name.text.clone(),
                abstr::Match(scrutinee.clone().into(), vec![abstr::Case {
                    pattern: pattern.clone(),
                    body: abstr::Var(definition.use_at(&HasNowhere)),
                }]),
            );
        }
        bindings
    }

    pub fn lower_decl(ctx: &mut LoweringCtx, term: Term) -> miette::Result<Defer> {
        Ok(match term {
            SrcPos(box term, loc) => {
                ctx.src_pos = loc;
                return lower_decl(ctx, term);
            }
            TypeDecl(TypeDecl { name, variable, cases }) => {
                let src_pos = ctx.src_pos.clone();
                let name = ctx.new_type(name);
                let variables = match variable {
                    Some(box variable) => lower_type_parameter(ctx, variable),
                    None => vec![],
                };
                let cases = lower_constructors(ctx, cases);

                Defer(Box::new(|_| {
                    Ok(abstr::Decl::TypeDecl(abstr::TypeDecl {
                        def: name,
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
                let pattern = pat::lower_pat(ctx, pattern)?;

                Defer(Box::new(|ctx| {
                    let mut fun_type = abstr::TypeRepr::Hole;
                    let mut patterns = vec![];

                    for parameter in parameters.into_iter().rev() {
                        let (pattern, type_repr) = lower_parameter(ctx, parameter)?;
                        fun_type = abstr::TypeRepr::Fun(type_repr.into(), fun_type.into());
                        patterns.push(pattern);
                    }

                    let mut body = lower_term(ctx.clone(), body)?;

                    for pattern in patterns.into_iter() {
                        let name = ctx.new_fresh_variable();
                        let default_case = abstr::Case { pattern, body };
                        let new_body = abstr::Match(abstr::Var(name.clone().use_at(ctx)).into(), vec![default_case]);
                        body = abstr::Fun(name, new_body.into());
                    }

                    match pattern {
                        abstr::Variable(variable) | abstr::PatternSrcPos(box abstr::Variable(variable), _) => {
                            Ok(abstr::Decl::LetDecl(abstr::LetDecl {
                                def: variable,
                                type_repr: fun_type,
                                body: abstr::Value(body),
                                loc: src_pos,
                            }))
                        }
                        _ => {
                            let new_var = ctx.new_fresh_variable();
                            for (name, case) in desestruct(pattern.clone(), abstr::Var(new_var.clone().use_at(ctx))) {
                                ctx.lets.insert(name, case);
                            }

                            Ok(abstr::Decl::LetDecl(abstr::LetDecl {
                                def: new_var.clone(),
                                type_repr: fun_type,
                                body: abstr::Value(abstr::Match(body.into(), vec![abstr::Case {
                                    pattern,
                                    body: abstr::Var(new_var.use_at(ctx)),
                                }])),
                                loc: src_pos,
                            }))
                        }
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

    pub fn lower_type_parameter(ctx: &mut LoweringCtx, term: Term) -> Vec<beml_tree::loc::Identifier> {
        let mut variables = vec![];
        match lower_type(ctx.clone(), term) {
            Ok(abstr::TypeRepr::SrcPos(box abstr::TypeRepr::Tuple(elements), _) | abstr::TypeRepr::Tuple(elements)) => {
                for element in elements {
                    let abstr::TypeRepr::Meta(variable) = element else {
                        ctx.report_error(TypeSyntaxError);
                        continue;
                    };
                    variables.push(variable);
                }
            }
            Ok(abstr::TypeRepr::SrcPos(box abstr::TypeRepr::Meta(variable), _) | abstr::TypeRepr::Meta(variable)) => {
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
                    def: name,
                    type_repr: Some(type_repr),
                })
            }
            Constructor(Constructor { name, type_repr: None }) => {
                let name = ctx.new_constructor(name);
                Ok(abstr::Constructor {
                    def: name,
                    type_repr: None,
                })
            }
            _ => ctx.wrap_error(ConstructorSyntaxError),
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;

    #[test]
    fn test_let_f_x_in_x() {
        let ctx = LoweringCtx::new(Source::from(""));
        let actual = lower_term(
            ctx,
            Let(Let {
                pattern: Term::Var("f".into()).into(),
                parameters: vec![Term::Var("x".into())],
                body: Term::Var("x".into()).into(),
                next: Term::Var("f".into()).into(),
            }),
        );
        let expected = expect![[]];
        expected.assert_debug_eq(&actual);
    }
}
