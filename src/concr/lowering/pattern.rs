use super::*;

impl LoweringCtx {
    pub fn parse_pattern(&mut self, case: Term) -> miette::Result<abstr::Pattern> {
        match case {
            SrcPos(box term, loc) => {
                self.src_pos = loc.clone();
                Ok(abstr::PatternSrcPos(self.parse_pattern(term)?.into(), loc))
            }
            Var(name) if name.text.chars().nth(0).unwrap().is_uppercase() => {
                Ok(match self.lookup_constructor(name.clone()) {
                    Ok(def) => abstr::Constructor(def.use_reference(), None),
                    Err(error) => {
                        let definition = self.new_variable(name);
                        self.report_error(UncapitalizeVariableError { error: Some(error) });
                        abstr::Variable(definition)
                    }
                })
            }
            Var(name) => Ok(abstr::Variable(self.new_variable(name))),
            App(box term, box arg) => {
                let abstr::Constructor(name, parameters) = self
                    .parse_pattern(term)
                    .map_err(|error| PatternConstructorAppError { error })
                    .into_diagnostic()?
                else {
                    return Err(ExpectedConstructorError).into_diagnostic();
                };

                if parameters.is_some() {
                    self.report_error(PatternArgumentAlreadyExistsError);
                }

                let parameters = self.parse_pattern(arg)?;

                Ok(abstr::Constructor(name, Some(parameters.into())))
            }
            Parens(box varargs) => Ok(abstr::Elements(
                self.sep_by(BinOp::Comma, varargs)?
                    .into_iter()
                    .map(|term| self.parse_pattern(term))
                    .collect::<miette::Result<Vec<_>>>()?,
            )),
            _ => {
                self.report_error(UnexpectedPatternSyntaxError);
                let definition = self.new_variable(Identifier::new("_", self.src_pos.clone()));
                Ok(abstr::Variable(definition))
            }
        }
    }

    pub fn parse_case(mut self, case: Term) -> miette::Result<abstr::Case> {
        self.burn();

        match case {
            SrcPos(box term, loc) => {
                self.src_pos = loc;
                self.parse_case(term)
            }
            BinOp(box pattern, BinOp::DoubleArrow, box body) => {
                let pattern = self.parse_pattern(pattern)?;
                let body = self.do_lowering(body)?;
                Ok(abstr::Case { pattern, body })
            }
            _ => Err(UnexpectedCaseSyntaxError).into_diagnostic(),
        }
    }

    pub fn parse_cases(self, cases: Vec<Term>) -> Vec<abstr::Case> {
        cases
            .into_iter()
            .filter_map(|case| self.or_none(self.clone().parse_case(case)))
            .collect()
    }
}
