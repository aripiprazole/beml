use super::*;

impl LoweringCtx {
    pub fn do_lowering(mut self, term: Term) -> miette::Result<abs::Term> {
        self.burn();

        match term {
            SrcPos(box term, loc) => {
                self.src_pos = loc.clone();
                Ok(abs::SrcPos(self.do_lowering(term)?.into(), loc))
            }
            Int(value) => Ok(abs::Int(value)),                             // 10
            Var(name) => Ok(abs::Var(self.lookup(name)?.use_reference())), // x

            // function
            // | Nil => 0
            // | Cons x xs => 1 + call xs
            Function(Function { cases }) => {
                let scrutinee = self.new_fresh_variable();
                let cases = self.parse_cases(cases);
                let var = abs::Var(scrutinee.clone().use_reference());
                Ok(abs::Fun(scrutinee, abs::Match(var.into(), cases).into()))
            }

            // match list with
            // | Nil => 0
            // | Cons x xs => 1
            Match(Match { box scrutinee, cases }) => {
                let scrutinee = self.clone().do_lowering(scrutinee)?;
                let cases = self.parse_cases(cases);
                Ok(abs::Match(scrutinee.into(), cases))
            }

            // 0 : int
            BinOp(box lhs, BinOp::Colon, box rhs) => {
                let lhs = self.clone().do_lowering(lhs)?;
                let rhs = self.parse_type(rhs)?;
                Ok(abs::Ascription(lhs.into(), rhs))
            }

            // 0 + 10
            BinOp(box lhs, BinOp::UserDefined(name), box rhs) => {
                let name = self.lookup(name)?;
                let lhs = self.clone().do_lowering(lhs)?;
                let rhs = self.do_lowering(rhs)?;
                Ok(abs::App(abs::App(abs::Var(name.use_reference()).into(), lhs.into()).into(), rhs.into()))
            }

            // let x y = 1 in x 10
            Let(Let { box pattern,
                      parameters,
                      box body,
                      box next, }) => {
                let pattern = self.parse_pattern(pattern)?;
                let mut ctx = self.clone();

                let mut fun_type = abs::Type::Hole;
                let mut patterns = vec![];
                for parameter in parameters.into_iter().rev() {
                    let (pattern, type_repr) = ctx.parse_parameter(parameter)?;
                    fun_type = abs::Type::Fun(type_repr.into(), fun_type.into());
                    patterns.push(pattern);
                }
                let mut body = ctx.clone().do_lowering(body)?;
                let next = self.do_lowering(next)?;

                for pattern in patterns.into_iter() {
                    let name = ctx.new_fresh_variable();
                    let default_case = abs::Case { pattern, body };
                    let new_body = abs::Match(abs::Var(name.clone().use_reference()).into(), vec![default_case]);
                    body = abs::Fun(name, new_body.into());
                }

                Ok(abs::Match(abs::Ascription(body.into(), fun_type).into(),
                              vec![abs::Case { pattern, body: next }]))
            }

            // f x
            App(box callee, box argument) => {
                let callee = self.clone().do_lowering(callee)?;
                let argument = self.clone().do_lowering(argument)?;
                Ok(abs::App(callee.into(), argument.into()))
            }

            // [x, y, z]
            Brackets(box elements) => {
                let elements = self.sep_by(BinOp::Comma, elements)?
                                   .into_iter()
                                   .filter_map(|term| self.or_none(self.clone().do_lowering(term)))
                                   .collect::<Vec<_>>();
                Ok(abs::List(elements))
            }

            // (x) or (x, y, z, ..)
            Parens(box value) => {
                if let BinOp(_, BinOp::Comma, _) = value {
                    let elements = self.sep_by(BinOp::Comma, value)?
                                       .into_iter()
                                       .filter_map(|term| self.or_none(self.clone().do_lowering(term)))
                                       .collect::<Vec<_>>();
                    Ok(abs::Pair(elements))
                } else {
                    self.do_lowering(value) // (x)
                }
            }

            Text(Text { value, loc }) => Ok(abs::Text(Text { value, loc })),

            // if x then y else z
            If(If { box condition,
                    box then,
                    box otherwise, }) => {
                let condition = self.clone().do_lowering(condition)?;
                let then = self.clone().do_lowering(then)?;
                let otherwise = self.do_lowering(otherwise)?;
                Ok(abs::If(condition.into(), then.into(), otherwise.into()))
            }

            _ => match self.parse_type(term) {
                Ok(_) => Err(TermSyntaxError).into_diagnostic()?,
                Err(_) => Err(TypeSyntaxAtTermLevelError).into_diagnostic(),
            },
        }
    }

    pub fn parse_type(mut self, term: Term) -> miette::Result<abs::Type> {
        match term {
            SrcPos(box term, loc) => {
                self.src_pos = loc.clone();
                Ok(abs::Type::SrcPos(self.parse_type(term)?.into(), loc))
            }
            Meta(name) => Ok(abs::Type::Meta(self.new_variable(name))),
            App(box argument, box callee) => {
                let argument = self.clone().parse_type(argument)?;
                let abs::Type::Constructor(callee) = self.clone().parse_type(callee)? else {
                    Err(TypeCalleeIsNotAConstructorError).into_diagnostic()?
                };
                if callee.name.text == "local" {
                    return Ok(abs::Type::Local(argument.into()));
                }
                Ok(abs::Type::App(callee, argument.into()))
            }
            Parens(box type_repr) => {
                if let BinOp(_, BinOp::Comma, _) = type_repr {
                    let type_repr = self.sep_by(BinOp::Comma, type_repr)?
                                        .into_iter()
                                        .filter_map(|term| self.or_none(self.clone().parse_type(term)))
                                        .collect::<Vec<_>>();
                    Ok(abs::Type::VPair(type_repr))
                } else {
                    self.parse_type(type_repr) // (x)
                }
            }
            BinOp(_, BinOp::Star, _) => {
                let elements = self.sep_by(BinOp::Star, term)?
                                   .into_iter()
                                   .filter_map(|term| self.or_none(self.clone().parse_type(term)))
                                   .collect::<Vec<_>>();
                Ok(abs::Type::Pair(elements))
            }
            Var(name) => self.lookup_type(name)
                             .map(|definition| abs::Type::Constructor(definition.use_reference()))
                             .into_diagnostic(),
            _ => Err(TypeSyntaxError).into_diagnostic(),
        }
    }

    pub fn parse_parameter(&mut self, parameter: Term) -> miette::Result<(abs::Pattern, abs::Type)> {
        self.burn();

        match parameter {
            SrcPos(box term, loc) => {
                self.src_pos = loc;
                self.parse_parameter(term)
            }
            Parens(box BinOp(box Var(name), BinOp::Colon, box type_repr)) => {
                let name = self.new_variable(name);
                let type_repr = self.clone().parse_type(type_repr)?;
                Ok((abs::Variable(name), type_repr))
            }
            Parens(_) => Err(UnexpectedParameterAscriptionSyntaxError).into_diagnostic(),
            Var(name) => Ok((abs::Variable(self.new_variable(name)), abs::Type::Hole)),
            _ => Err(UnexpectedParameterSyntaxError).into_diagnostic(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_f_x_in_x() {
        let ctx = LoweringCtx::default();
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
            input: ctx.do_lowering(Let(Let { pattern: Term::Var("f".into()).into(),
                                             parameters: vec![Term::Var("x".into())],
                                             body: Term::Var("x".into()).into(),
                                             next: Term::Var("f".into()).into() }))
        }
    }
}
