use std::sync::Arc;

use miette::NamedSource;

use super::*;
use crate::concr::errors::*;

pub mod decl;
pub mod pattern;
pub mod term;

#[derive(Clone)]
pub struct LoweringCtx {
    src_pos: crate::loc::Loc,
    file: PathBuf,
    text: String,
    variables: HashMap<String, Arc<abstr::Definition>>,
    constructors: HashMap<String, Arc<abstr::Definition>>,
    types: HashMap<String, Arc<abstr::Definition>>,
    errors: Rc<RefCell<Vec<miette::Report>>>,
    counter: Rc<Cell<usize>>,
    #[cfg(debug_assertions)]
    gas: Rc<Cell<usize>>,
}

impl LoweringCtx {
    /// Creates a new lowering context
    pub fn new(file: PathBuf, text: String) -> Self {
        Self {
            file,
            text,
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

    /// Wraps the error with the source code and location
    fn wrap_error<A, T>(&self, source: T) -> miette::Result<A>
    where
        T: miette::Diagnostic + std::error::Error + Send + Sync + 'static, {
        Err(LoweringError {
            loc: self.src_pos.clone(),
            source_code: NamedSource::new(self.file.to_str().unwrap(), self.text.clone()),
            source,
        })?
    }

    fn new_fresh_variable(&mut self) -> Arc<abstr::Definition> {
        self.counter.set(self.counter.get() + 1);
        let name = Identifier::new(&format!("_{}", self.counter.get()), self.src_pos.clone());
        let definition = Arc::new(abstr::Definition {
            name: name.clone(),
            loc: self.src_pos.clone(),
            references: Default::default(),
        });
        self.variables.insert(name.text.clone(), definition.clone());
        definition
    }

    fn new_constructor(&mut self, name: Identifier) -> Arc<abstr::Definition> {
        let definition = Arc::new(abstr::Definition {
            name: name.clone(),
            loc: self.src_pos.clone(),
            references: Default::default(),
        });
        self.constructors.insert(name.text.clone(), definition.clone());
        definition
    }

    fn new_type(&mut self, name: Identifier) -> Arc<abstr::Definition> {
        let definition = Arc::new(abstr::Definition {
            name: name.clone(),
            loc: self.src_pos.clone(),
            references: Default::default(),
        });
        self.types.insert(name.text.clone(), definition.clone());
        definition
    }

    fn new_variable(&mut self, name: Identifier) -> Arc<abstr::Definition> {
        let definition = Arc::new(abstr::Definition {
            name: name.clone(),
            loc: self.src_pos.clone(),
            references: Default::default(),
        });
        self.variables.insert(name.text.clone(), definition.clone());
        definition
    }

    fn report_error<T: miette::Diagnostic + std::error::Error + Send + Sync + 'static>(&self, error: T) {
        let report = self.wrap_error::<(), T>(error).unwrap_err();
        self.report_direct_error(report);
    }

    fn report_direct_error(&self, error: miette::Report) {
        let error = error.with_source_code(NamedSource::new(self.file.to_str().unwrap(), self.text.clone()));
        self.errors.borrow_mut().push(error);
    }

    fn lookup_variable(&self, name: Identifier) -> Result<Arc<abstr::Definition>, UnresolvedVariableError> {
        self.variables
            .get(&name.text)
            .cloned()
            .ok_or(UnresolvedVariableError { name: name.text })
    }

    fn lookup_type(&self, name: Identifier) -> Result<Arc<abstr::Definition>, UnresolvedTypeError> {
        self.types
            .get(&name.text)
            .cloned()
            .ok_or(UnresolvedTypeError { name: name.text })
    }

    fn lookup(&self, name: Identifier) -> Result<Arc<abstr::Definition>, UnresolvedSymbolError> {
        self.lookup_constructor(name.clone())
            .map_err(UnresolvedSymbolError::UnresolvedConstructorError)
            .or_else(|_| self.lookup_variable(name))
            .map_err(UnresolvedSymbolError::UnresolvedVariableError)
    }

    fn lookup_constructor(&self, name: Identifier) -> Result<Arc<abstr::Definition>, UnresolvedConstructorError> {
        self.constructors
            .get(&name.text)
            .cloned()
            .ok_or(UnresolvedConstructorError { name: name.text })
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
