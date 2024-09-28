use super::*;
use crate::concr::errors::*;

pub mod decl;
pub mod pattern;
pub mod term;

#[derive(Clone)]
pub struct LoweringCtx {
    src_pos: crate::loc::Loc,
    variables: HashMap<String, Rc<abstr::Definition>>,
    constructors: HashMap<String, Rc<abstr::Definition>>,
    types: HashMap<String, Rc<abstr::Definition>>,
    errors: Rc<RefCell<Vec<miette::Report>>>,
    counter: Rc<Cell<usize>>,
    #[cfg(debug_assertions)]
    gas: Rc<Cell<usize>>,
}

impl Default for LoweringCtx {
    fn default() -> Self {
        Self {
            src_pos: crate::loc::Loc::default(),
            variables: Default::default(),
            constructors: Default::default(),
            types: HashMap::from([
                (String::from("int"), Definition::new("int")),
                (String::from("string"), Definition::new("string")),
                (String::from("unit"), Definition::new("unit")),
                (String::from("local"), Definition::new("local")),
            ]),
            errors: Default::default(),
            counter: Default::default(),
            #[cfg(debug_assertions)]
            gas: Default::default(),
        }
    }
}

impl LoweringCtx {
    #[cfg(debug_assertions)]
    fn burn(&self) {
        if self.gas.get() == 10000 {
            panic!("gas exhausted");
        }

        self.gas.set(self.gas.get() + 1);
    }

    #[cfg(not(debug_assertions))]
    #[inline(always)]
    fn burn(&self) {}

    fn new_fresh_variable(&mut self) -> Rc<abstr::Definition> {
        self.counter.set(self.counter.get() + 1);
        let name = Identifier::new(&format!("_{}", self.counter.get()), self.src_pos.clone());
        let definition = Rc::new(abstr::Definition {
            name: name.clone(),
            loc: self.src_pos.clone(),
            references: Default::default(),
        });
        self.variables.insert(name.text.clone(), definition.clone());
        definition
    }

    fn new_constructor(&mut self, name: Identifier) -> Rc<abstr::Definition> {
        let definition = Rc::new(abstr::Definition {
            name: name.clone(),
            loc: self.src_pos.clone(),
            references: Default::default(),
        });
        self.constructors.insert(name.text.clone(), definition.clone());
        definition
    }

    fn new_type(&mut self, name: Identifier) -> Rc<abstr::Definition> {
        let definition = Rc::new(abstr::Definition {
            name: name.clone(),
            loc: self.src_pos.clone(),
            references: Default::default(),
        });
        self.types.insert(name.text.clone(), definition.clone());
        definition
    }

    fn new_variable(&mut self, name: Identifier) -> Rc<abstr::Definition> {
        let definition = Rc::new(abstr::Definition {
            name: name.clone(),
            loc: self.src_pos.clone(),
            references: Default::default(),
        });
        self.variables.insert(name.text.clone(), definition.clone());
        definition
    }

    fn report_error<T: miette::Diagnostic + std::error::Error + Send + Sync + 'static>(&self, error: T) {
        let report = Err::<(), T>(error).into_diagnostic().unwrap_err();
        self.report_direct_error(report);
    }

    fn report_direct_error(&self, error: miette::Report) {
        self.errors.borrow_mut().push(error);
    }

    fn lookup_variable(&self, name: Identifier) -> Result<Rc<abstr::Definition>, UnresolvedVariableError> {
        self.variables.get(&name.text).cloned().ok_or(UnresolvedVariableError)
    }

    fn lookup_type(&self, name: Identifier) -> Result<Rc<abstr::Definition>, UnresolvedTypeError> {
        self.types.get(&name.text).cloned().ok_or(UnresolvedTypeError)
    }

    fn lookup(&self, name: Identifier) -> Result<Rc<abstr::Definition>, UnresolvedSymbolError> {
        self.lookup_constructor(name.clone())
            .map_err(UnresolvedSymbolError::UnresolvedConstructorError)
            .or_else(|_| self.lookup_variable(name))
            .map_err(UnresolvedSymbolError::UnresolvedVariableError)
    }

    fn lookup_constructor(&self, name: Identifier) -> Result<Rc<abstr::Definition>, UnresolvedConstructorError> {
        self.constructors
            .get(&name.text)
            .cloned()
            .ok_or(UnresolvedConstructorError)
    }

    fn or_none<T>(&self, term: miette::Result<T>) -> Option<T> {
        match term {
            Ok(term) => Some(term),
            Err(err) => {
                self.report_direct_error(err);
                None
            }
        }
    }

    fn sep_by(&mut self, desired: BinOp, mut acc: Term) -> miette::Result<Vec<Term>> {
        self.burn();

        let mut terms = vec![];
        if let SrcPos(box term, _) = acc {
            acc = term;
        }

        while let BinOp(box lhs, op, box rhs) = acc {
            if desired == op {
                terms.push(lhs);
                acc = rhs;
            } else {
                break;
            }
        }

        Ok(terms)
    }
}
