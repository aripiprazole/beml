use super::*;

use crate::loc::Identifier;

use std::{cell::{Cell, RefCell},
          collections::HashMap,
          path::PathBuf,
          rc::Rc};

use abs::{Declaration, Definition};
use loc::Text;
use miette::IntoDiagnostic;

pub use Term::*;

mod decl;
mod pattern;
mod term;

#[derive(Debug)]
pub struct TypeDecl {
    pub name: Identifier,
    pub variable: Box<Term>,
    pub cases: Vec<Term>,
}

#[derive(Debug)]
pub struct LetDecl {
    pub pattern: Box<Term>,
    pub parameters: Vec<Term>,
    pub body: Box<Term>,
}

#[derive(Debug)]
pub struct ValDecl {
    pub name: Identifier,
    pub type_repr: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Colon,       // :
    Comma,       // ,
    Star,        // * : pair type
    Arrow,       // ->
    DoubleArrow, // =>
    Bar,         // |
    UserDefined(Identifier),
}

#[derive(Debug)]
pub struct Function {
    pub cases: Vec<Term>,
}

#[derive(Debug)]
pub struct Match {
    pub scrutinee: Box<Term>,
    pub cases: Vec<Term>,
}

#[derive(Debug)]
pub struct Let {
    pub pattern: Box<Term>,
    pub parameters: Vec<Term>,
    pub body: Box<Term>,
    pub next: Box<Term>,
}

#[derive(Debug)]
pub struct If {
    pub condition: Box<Term>,
    pub then: Box<Term>,
    pub otherwise: Box<Term>,
}

#[derive(Debug)]
pub struct Constructor {
    pub name: Identifier,
    pub type_repr: Box<Term>,
}

#[derive(Debug)]
pub enum Term {
    SrcPos(Box<Term>, crate::loc::Loc),
    Parens(Box<Term>),
    Brackets(Box<Term>),
    Braces(Box<Term>),
    TypeDecl(TypeDecl),
    LetDecl(LetDecl),
    ValDecl(ValDecl),
    Constructor(Constructor),
    BinOp(Box<Term>, BinOp, Box<Term>),
    Fun(Vec<Identifier>, Box<Term>),
    App(Box<Term>, Box<Term>),
    Var(Identifier),
    Meta(Identifier),
    Function(Function),
    Match(Match),
    If(If),
    Let(Let),
    Int(i64),
    Text(Text),
}

#[derive(Debug)]
pub struct File {
    pub path: PathBuf,
    pub shebang: Option<String>,
    pub terms: Vec<Term>,
}

// can't find the constructor
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("can't find the constructor")]
pub struct UnresolvedConstructorError;

// can't find the constructor
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("can't find the variable")]
pub struct UnresolvedVariableError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("can't find the type")]
pub struct UnresolvedTypeError;

// can't find the constructor, so if it is a variable pattern, uncapitalize it
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("can't find the constructor")]
pub struct UncapitalizeVariableError {
    pub error: Option<UnresolvedConstructorError>,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("pattern constructor application")]
pub struct PatternConstructorAppError {
    pub error: miette::Report,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("expected a constructor")]
pub struct ExpectedConstructorError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("pattern argument already exists")]
pub struct PatternArgumentAlreadyExistsError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected pattern syntax")]
pub struct UnexpectedPatternSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected case syntax")]
pub struct UnexpectedCaseSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected parameter ascription syntax")]
pub struct UnexpectedParameterAscriptionSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected parameter syntax")]
pub struct UnexpectedParameterSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected term syntax")]
pub struct TermSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("type callee is not a constructor")]
pub struct TypeCalleeIsNotAConstructorError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected type syntax")]
pub struct TypeSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected decl syntax")]
pub struct DeclSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected decl syntax")]
pub struct ConstructorSyntaxError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected type syntax at term level")]
pub struct TypeSyntaxAtTermLevelError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected term syntax at decl level")]
pub struct TermSyntaxAtDeclLevelError;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unexpected symbol error")]
pub enum UnresolvedSymbolError {
    #[error("can't find the symbol")]
    UnresolvedConstructorError(UnresolvedConstructorError),

    #[error("can't find the symbol")]
    UnresolvedVariableError(UnresolvedVariableError),
}

#[derive(Clone)]
struct LoweringCtx {
    src_pos: crate::loc::Loc,
    variables: HashMap<Identifier, Rc<abs::Definition>>,
    constructors: HashMap<Identifier, Rc<abs::Definition>>,
    types: HashMap<Identifier, Rc<abs::Definition>>,
    errors: Rc<RefCell<Vec<miette::Report>>>,
    counter: Rc<Cell<usize>>,
    #[cfg(debug_assertions)]
    gas: Rc<Cell<usize>>,
}

impl Default for LoweringCtx {
    fn default() -> Self {
        Self { src_pos: crate::loc::Loc::default(),
               variables: Default::default(),
               constructors: Default::default(),
               types: HashMap::from([(Identifier::from("int"), Definition::new("int")),
                                     (Identifier::from("string"), Definition::new("string")),
                                     (Identifier::from("local"), Definition::new("local"))]),
               errors: Default::default(),
               counter: Default::default(),
               #[cfg(debug_assertions)]
               gas: Default::default() }
    }
}

impl LoweringCtx {
    #[cfg(debug_assertions)]
    fn burn(&self) {
        if self.gas.get() == 1000 {
            panic!("gas exhausted");
        }

        self.gas.set(self.gas.get() + 1);
    }

    #[cfg(not(debug_assertions))]
    #[inline(always)]
    fn burn(&self) {}

    fn new_fresh_variable(&mut self) -> Rc<abs::Definition> {
        self.counter.set(self.counter.get() + 1);
        let name = Identifier::new(&format!("_{}", self.counter.get()), self.src_pos.clone());
        let definition = Rc::new(abs::Definition { name: name.clone(),
                                                   loc: self.src_pos.clone(),
                                                   references: Default::default() });
        self.variables.insert(name.clone(), definition.clone());
        definition
    }

    fn new_constructor(&mut self, name: Identifier) -> Rc<abs::Definition> {
        let definition = Rc::new(abs::Definition { name: name.clone(),
                                                   loc: self.src_pos.clone(),
                                                   references: Default::default() });
        self.constructors.insert(name.clone(), definition.clone());
        definition
    }

    fn new_type(&mut self, name: Identifier) -> Rc<abs::Definition> {
        let definition = Rc::new(abs::Definition { name: name.clone(),
                                                   loc: self.src_pos.clone(),
                                                   references: Default::default() });
        self.types.insert(name.clone(), definition.clone());
        definition
    }

    fn new_variable(&mut self, name: Identifier) -> Rc<abs::Definition> {
        let definition = Rc::new(abs::Definition { name: name.clone(),
                                                   loc: self.src_pos.clone(),
                                                   references: Default::default() });
        self.variables.insert(name.clone(), definition.clone());
        definition
    }

    fn report_error<T: miette::Diagnostic + std::error::Error + Send + Sync + 'static>(&self, error: T) {
        let report = Err::<(), T>(error).into_diagnostic().unwrap_err();
        self.report_direct_error(report);
    }

    fn report_direct_error(&self, error: miette::Report) {
        self.errors.borrow_mut().push(error);
    }

    fn lookup_variable(&self, name: Identifier) -> Result<Rc<abs::Definition>, UnresolvedVariableError> {
        self.variables.get(&name).cloned().ok_or(UnresolvedVariableError)
    }

    fn lookup_type(&self, name: Identifier) -> Result<Rc<abs::Definition>, UnresolvedTypeError> {
        self.types.get(&name).cloned().ok_or(UnresolvedTypeError)
    }

    fn lookup(&self, name: Identifier) -> Result<Rc<abs::Definition>, UnresolvedSymbolError> {
        self.lookup_constructor(name.clone())
            .map_err(UnresolvedSymbolError::UnresolvedConstructorError)
            .or_else(|_| self.lookup_variable(name))
            .map_err(UnresolvedSymbolError::UnresolvedVariableError)
    }

    fn lookup_constructor(&self, name: Identifier) -> Result<Rc<abs::Definition>, UnresolvedConstructorError> {
        self.constructors.get(&name).cloned().ok_or(UnresolvedConstructorError)
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

pub fn lower_file(file: File) -> miette::Result<abs::File> {
    let mut ctx = LoweringCtx::default();
    let mut declarations = HashMap::new();
    let terms = file.terms
                    .into_iter()
                    .map(|decl| ctx.do_declaration_lowering(decl))
                    .collect::<miette::Result<Vec<_>>>()?;
    for decl::Defer(f) in terms {
        let decl = f(&mut ctx)?;

        declarations.insert(decl.name().clone(), decl);
    }

    Ok(abs::File { path: file.path,
                   shebang: file.shebang,
                   declarations })
}
