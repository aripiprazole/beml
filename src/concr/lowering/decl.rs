use super::*;

pub struct Defer(pub Box<dyn FnOnce(&mut LoweringCtx) -> miette::Result<abstr::Decl>>);

impl LoweringCtx {
    pub fn do_declaration_lowering(&mut self, term: Term) -> miette::Result<Defer> {
        Ok(match term {
            SrcPos(box term, loc) => {
                self.src_pos = loc;
                return self.do_declaration_lowering(term);
            }
            TypeDecl(TypeDecl {
                name,
                box variable,
                cases,
            }) => {
                let src_pos = self.src_pos.clone();
                let name = self.new_type(name);
                let variables = self.parse_type_parameter(variable);
                let cases = self.parse_constructors(cases);

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
                let src_pos = self.src_pos.clone();
                let pattern = self.parse_pattern(pattern)?;

                Defer(Box::new(|ctx| {
                    let mut fun_type = abstr::Type::Hole;
                    let mut patterns = vec![];

                    for parameter in parameters.into_iter().rev() {
                        let (pattern, type_repr) = ctx.parse_parameter(parameter)?;
                        fun_type = abstr::Type::Fun(type_repr.into(), fun_type.into());
                        patterns.push(pattern);
                    }

                    let mut body = ctx.clone().do_lowering(body)?;

                    for pattern in patterns.into_iter() {
                        let name = ctx.new_fresh_variable();
                        let default_case = abstr::Case { pattern, body };
                        let new_body =
                            abstr::Match(abstr::Var(name.clone().use_reference()).into(), vec![default_case]);
                        body = abstr::Fun(name, new_body.into());
                    }

                    if let abstr::Variable(variable) = pattern {
                        Ok(abstr::Decl::LetDecl(abstr::LetDecl {
                            name: variable,
                            type_repr: fun_type,
                            body: abstr::Value(body),
                            loc: src_pos,
                        }))
                    } else {
                        Ok(abstr::Decl::LetDecl(abstr::LetDecl {
                            name: ctx.new_fresh_variable(),
                            type_repr: fun_type,
                            body: abstr::Value(abstr::Match(body.into(), vec![abstr::Case {
                                pattern,
                                body: abstr::Var(ctx.new_fresh_variable().use_reference()),
                            }])),
                            loc: src_pos,
                        }))
                    }
                }))
            }
            ValDecl(ValDecl { name, box type_repr }) => {
                let src_pos = self.src_pos.clone();
                let name = self.new_variable(name);
                Defer(Box::new(|ctx| {
                    let type_repr = ctx.clone().parse_type(type_repr)?;
                    Ok(abstr::Decl::LetDecl(abstr::LetDecl {
                        name,
                        type_repr,
                        body: abstr::No,
                        loc: src_pos,
                    }))
                }))
            }
            _ => return Err(DeclSyntaxError).into_diagnostic(),
        })
    }

    pub fn parse_type_parameter(&mut self, term: Term) -> Vec<crate::loc::Identifier> {
        let mut variables = vec![];
        match self.clone().parse_type(term) {
            Ok(abstr::Type::SrcPos(box abstr::Type::Tuple(elements), _) | abstr::Type::Tuple(elements)) => {
                for element in elements {
                    let abstr::Type::Meta(variable) = element else {
                        self.report_error(TypeSyntaxError);
                        continue;
                    };
                    variables.push(variable);
                }
            }
            Ok(abstr::Type::SrcPos(box abstr::Type::Meta(variable), _) | abstr::Type::Meta(variable)) => {
                variables.push(variable);
            }
            Ok(_) => self.report_error(TypeSyntaxError),
            Err(err) => self.report_direct_error(err),
        }
        variables
    }

    pub fn parse_constructors(&mut self, constructors: Vec<Term>) -> Vec<abstr::Constructor> {
        constructors
            .into_iter()
            .filter_map(|term| self.or_none(self.clone().parse_constructor(term)))
            .collect()
    }

    pub fn parse_constructor(&mut self, term: Term) -> miette::Result<abstr::Constructor> {
        match term {
            SrcPos(box term, loc) => {
                self.src_pos = loc;
                self.parse_constructor(term)
            }
            Constructor(Constructor {
                name,
                type_repr: Some(box type_repr),
            }) => {
                let name = self.new_constructor(name);
                let type_repr = self.clone().parse_type(type_repr)?;
                Ok(abstr::Constructor {
                    name,
                    type_repr: Some(type_repr),
                })
            }
            Constructor(Constructor { name, type_repr: None }) => {
                let name = self.new_constructor(name);
                Ok(abstr::Constructor { name, type_repr: None })
            }
            _ => Err(ConstructorSyntaxError).into_diagnostic(),
        }
    }
}
