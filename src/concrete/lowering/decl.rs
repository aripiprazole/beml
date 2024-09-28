use super::*;

pub struct Defer(pub Box<dyn FnOnce(&mut LoweringCtx) -> miette::Result<abs::Decl>>);

impl LoweringCtx {
    pub fn parse_type_parameter(&mut self, term: Term) -> Vec<crate::loc::Identifier> {
        let mut variables = vec![];
        match self.clone().parse_type(term) {
            Ok(abs::Type::VPair(elements)) => {
                for element in elements {
                    let abs::Type::Meta(variable) = element else {
                        self.report_error(TypeSyntaxError);
                        continue;
                    };
                    variables.push(variable);
                }
            }
            Ok(abs::Type::Meta(variable)) => {
                variables.push(variable);
            }
            Ok(_) => self.report_error(TypeSyntaxError),
            Err(err) => self.report_direct_error(err),
        }
        variables
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
            Constructor(Constructor { name,
                                      type_repr: Some(box type_repr), }) => {
                let name = self.new_constructor(name);
                let type_repr = self.clone().parse_type(type_repr)?;
                Ok(abs::Constructor { name,
                                      type_repr: Some(type_repr) })
            }
            Constructor(Constructor { name, type_repr: None }) => {
                let name = self.new_constructor(name);
                Ok(abs::Constructor { name, type_repr: None })
            }
            _ => Err(ConstructorSyntaxError).into_diagnostic(),
        }
    }

    pub fn do_declaration_lowering(&mut self, term: Term) -> miette::Result<Defer> {
        Ok(match term {
            SrcPos(box term, loc) => {
                self.src_pos = loc;
                return self.do_declaration_lowering(term);
            }
            TypeDecl(TypeDecl { name,
                                box variable,
                                cases, }) => {
                let src_pos = self.src_pos.clone();
                let name = self.new_type(name);
                let variables = self.parse_type_parameter(variable);
                let cases = self.parse_constructors(cases);

                Defer(Box::new(|_| {
                          Ok(abs::Decl::TypeDecl(abs::TypeDecl { name,
                                                                 variables,
                                                                 cases,
                                                                 loc: src_pos }))
                      }))
            }
            LetDecl(LetDecl { box pattern,
                              box body,
                              parameters, }) => {
                let src_pos = self.src_pos.clone();
                let pattern = self.parse_pattern(pattern)?;
                let mut ctx = self.clone();

                let mut fun_type = abs::Type::Hole;
                let mut patterns = vec![];
                for parameter in parameters.into_iter().rev() {
                    let (pattern, type_repr) = ctx.parse_parameter(parameter)?;
                    fun_type = abs::Type::Fun(type_repr.into(), fun_type.into());
                    patterns.push(pattern);
                }

                Defer(Box::new(|ctx| {
                          let mut body = ctx.clone().do_lowering(body)?;

                          for pattern in patterns.into_iter() {
                              let name = ctx.new_fresh_variable();
                              let default_case = abs::Case { pattern, body };
                              let new_body =
                                  abs::Match(abs::Var(name.clone().use_reference()).into(), vec![default_case]);
                              body = abs::Fun(name, new_body.into());
                          }

                          if let abs::Variable(variable) = pattern {
                              Ok(abs::Decl::LetDecl(abs::LetDecl { name: variable,
                                                                   type_repr: fun_type,
                                                                   body: abs::Value(body),
                                                                   loc: src_pos }))
                          } else {
                              Ok(abs::Decl::LetDecl(abs::LetDecl { name: ctx.new_fresh_variable(),
                                                                     type_repr: fun_type,
                                                                     body: abs::Value(abs::Match(body.into(), vec![abs::Case { pattern, body: abs::Var(ctx.new_fresh_variable().use_reference()) }])),
                                                                     loc: src_pos }))
                          }
                      }))
            }
            ValDecl(ValDecl { name, box type_repr }) => {
                let src_pos = self.src_pos.clone();
                let name = self.new_variable(name);
                Defer(Box::new(|ctx| {
                          let type_repr = ctx.clone().parse_type(type_repr)?;
                          Ok(abs::Decl::LetDecl(abs::LetDecl { name,
                                                               type_repr,
                                                               body: abs::No,
                                                               loc: src_pos }))
                      }))
            }
            _ => return Err(DeclSyntaxError).into_diagnostic(),
        })
    }
}
