use super::*;

impl LoweringCtx {
    pub fn parse_pattern(&mut self, case: Term) -> miette::Result<abs::Pattern> {
        match case {
            SrcPos(box term, loc) => {
                self.src_pos = loc;
                self.parse_pattern(term)
            }
            Var(name) if name.text.chars().nth(0).unwrap().is_uppercase() => {
                Ok(match self.lookup_constructor(name.clone()) {
                    Ok(def) => abs::Constructor(def.use_reference(), None),
                    Err(error) => {
                        let definition = self.new_variable(name);
                        self.report_error(UncapitalizeVariableError { error: Some(error) });
                        abs::Variable(definition)
                    }
                })
            }
            Var(name) => Ok(abs::Variable(self.new_variable(name))),
            App(box term, box arg) => {
                let abs::Constructor(name, parameters) =
                    self.parse_pattern(term)
                        .map_err(|error| PatternConstructorAppError { error: error.into() })
                        .into_diagnostic()?
                else {
                    return Err(ExpectedConstructorError).into_diagnostic();
                };

                if parameters.is_some() {
                    self.report_error(PatternArgumentAlreadyExistsError);
                }

                let parameters = self.parse_pattern(arg)?;

                Ok(abs::Constructor(name, Some(parameters.into())))
            }
            Parens(box varargs) => Ok(abs::Elements(self.sep_by(BinOp::Comma, varargs)?
                                                        .into_iter()
                                                        .map(|term| self.parse_pattern(term))
                                                        .collect::<miette::Result<Vec<_>>>()?)),
            _ => {
                self.report_error(UnexpectedPatternSyntaxError);
                let definition = self.new_variable(Identifier::new("_", self.src_pos.clone()));
                Ok(abs::Variable(definition))
            }
        }
    }

    pub fn parse_case(mut self, case: Term) -> miette::Result<abs::Case> {
        self.burn();

        match case {
            SrcPos(box term, loc) => {
                self.src_pos = loc;
                self.parse_case(term)
            }
            BinOp(box pattern, BinOp::DoubleArrow, box body) => {
                let pattern = self.parse_pattern(pattern)?;
                let body = self.do_lowering(body)?;
                Ok(abs::Case { pattern, body })
            }
            _ => Err(UnexpectedCaseSyntaxError).into_diagnostic(),
        }
    }

    pub fn parse_cases(self, cases: Vec<Term>) -> Vec<abs::Case> {
        cases.into_iter()
             .filter_map(|case| self.or_none(self.clone().parse_case(case)))
             .collect()
    }
}
