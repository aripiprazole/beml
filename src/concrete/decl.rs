use super::*;

impl LoweringCtx {
    pub fn parse_type_parameter(&mut self, term: Term) -> Vec<Rc<abs::Definition>> {
        todo!()
    }

    pub fn parse_constructors(&mut self, constructors: Vec<Term>) -> Vec<abs::Constructor> {
        constructors.into_iter()
                    .filter_map(|term| self.or_none(self.clone().parse_constructor(term)))
                    .collect()
    }

    pub fn parse_constructor(&mut self, term: Term) -> miette::Result<abs::Constructor> {
        match term {
            SrcPos(box term, loc) => {
                self.src_pos = loc;
                self.parse_constructor(term)
            }
            _ => Err(ConstructorSyntaxError).into_diagnostic(),
        }
    }

    pub fn do_declaration_lowering(&mut self, term: Term) -> miette::Result<abs::Decl> {
        match term {
            SrcPos(box term, loc) => {
                self.src_pos = loc;
                self.do_declaration_lowering(term)
            }
            TypeDecl(TypeDecl { name,
                                box variable,
                                cases, }) => {
                let name = self.new_type(name);
                let variables = self.parse_type_parameter(variable);
                let cases = self.parse_constructors(cases);

                Ok(abs::Decl::TypeDecl(abs::TypeDecl { name,
                                                       variables,
                                                       cases,
                                                       loc: self.src_pos.clone() }))
            }
            LetDecl(LetDecl { box pattern,
                              box body,
                              parameters, }) => {
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

                for pattern in patterns.into_iter() {
                    let name = ctx.new_fresh_variable();
                    let default_case = abs::Case { pattern, body };
                    let new_body = abs::Match(abs::Var(name.clone().use_reference()).into(), vec![default_case]);
                    body = abs::Fun(name, new_body.into());
                }

                Ok(abs::Decl::LetDecl(abs::LetDecl { name: self.new_fresh_variable(),
                                                     type_repr: fun_type,
                                                     body: abs::Value(abs::Match(body.into(), vec![abs::Case { pattern, body: abs::Var(self.new_fresh_variable().use_reference()) }])),
                                                     loc: self.src_pos.clone() }))
            }
            ValDecl(ValDecl { name, box type_repr }) => {
                let name = self.new_variable(name);
                let type_repr = self.clone().parse_type(type_repr)?;
                Ok(abs::Decl::LetDecl(abs::LetDecl { name,
                                                     type_repr,
                                                     body: abs::No,
                                                     loc: self.src_pos.clone() }))
            }
            _ => Err(DeclSyntaxError).into_diagnostic(),
        }
    }
}
