use super::*;

#[repr(transparent)]
pub struct PrettyPrint<'a, T>(pub &'a T);

impl Debug for PrettyPrint<'_, Term> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            SrcPos(box t, _) => t.pretty_print().fmt(f),
            List(elements) => {
                write!(f, "[")?;
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    e.pretty_print().fmt(f)?;
                }
                write!(f, "]")
            }
            Pair(elements) => {
                write!(f, "(")?;
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    e.pretty_print().fmt(f)?;
                }
                write!(f, ")")
            }
            Fun(param, box body) => {
                write!(f, "fun {}", param.name.text)?;
                write!(f, " => ")?;
                body.pretty_print().fmt(f)
            }
            Match(box scrutinee, arms) => {
                write!(f, "match ")?;
                scrutinee.pretty_print().fmt(f)?;
                write!(f, " with ")?;
                for arm in arms {
                    write!(f, " | ")?;
                    arm.pretty_print().fmt(f)?;
                }
                Ok(())
            }
            Ascription(box value, type_repr) => {
                value.pretty_print().fmt(f)?;
                write!(f, ": ")?;
                type_repr.fmt(f)
            }
            App(box callee, box argument) => {
                callee.pretty_print().fmt(f)?;
                write!(f, " ")?;
                argument.pretty_print().fmt(f)
            }
            Var(var) => write!(f, "{}", var.name.text),
            Int(i) => write!(f, "{}", i),
            Text(s) => write!(f, "{:?}", s.value),
            If(box t1, box t2, box t3) => {
                write!(f, "if ")?;
                t1.pretty_print().fmt(f)?;
                write!(f, " then ")?;
                t2.pretty_print().fmt(f)?;
                write!(f, " else ")?;
                t3.pretty_print().fmt(f)?;
                write!(f, " end")
            }
            Let(definition, box value, box next) => {
                write!(f, "let {}", definition.name.text)?;
                write!(f, " = ")?;
                value.pretty_print().fmt(f)?;
                write!(f, " in ")?;
                next.pretty_print().fmt(f)
            }
        }
    }
}

impl Debug for PrettyPrint<'_, TypeRepr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            TypeRepr::SrcPos(box t, _) => t.pretty_print().fmt(f),
            TypeRepr::Pair(elements) => {
                write!(f, "(")?;
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, " * ")?;
                    }
                    e.pretty_print().fmt(f)?;
                }
                write!(f, ")")
            }
            TypeRepr::Tuple(elements) => {
                write!(f, "(")?;
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    e.pretty_print().fmt(f)?;
                }
                write!(f, ")")
            }
            TypeRepr::Fun(box domain, box codomain) => {
                domain.pretty_print().fmt(f)?;
                write!(f, " -> ")?;
                codomain.pretty_print().fmt(f)
            }
            TypeRepr::App(callee, box argument) => {
                argument.pretty_print().fmt(f)?;
                write!(f, " {}", callee.name.text)
            }
            TypeRepr::Local(box value) => {
                value.pretty_print().fmt(f)?;
                write!(f, " local")
            }
            TypeRepr::Meta(name) => {
                write!(f, "'{}", name.text)
            }
            TypeRepr::Constructor(constructor) => {
                write!(f, "{}", constructor.name.text)
            }
            TypeRepr::Hole => write!(f, "_"),
        }
    }
}

impl Debug for PrettyPrint<'_, Pattern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            PatternSrcPos(box p, _) => p.pretty_print().fmt(f),
            Pattern::Constructor(def, Some(box parameter)) => {
                write!(f, "{}", def.name.text)?;
                write!(f, " ")?;
                parameter.pretty_print().fmt(f)
            }
            Pattern::Constructor(def, None) => write!(f, "{}", def.name.text),
            Elements(elements) => {
                write!(f, "(")?;
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    e.pretty_print().fmt(f)?;
                }
                write!(f, ")")
            }
            Variable(var) => write!(f, "{}", var.name.text),
        }
    }
}

impl Debug for PrettyPrint<'_, Case> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.pattern.pretty_print().fmt(f)?;
        write!(f, " => ")?;
        self.0.body.pretty_print().fmt(f)
    }
}

impl Pattern {
    pub fn pretty_print(&self) -> PrettyPrint<'_, Pattern> {
        PrettyPrint(self)
    }
}

impl Case {
    pub fn pretty_print(&self) -> PrettyPrint<'_, Case> {
        PrettyPrint(self)
    }
}

impl TypeRepr {
    pub fn pretty_print(&self) -> PrettyPrint<'_, TypeRepr> {
        PrettyPrint(self)
    }
}

impl Term {
    pub fn pretty_print(&self) -> PrettyPrint<'_, Term> {
        PrettyPrint(self)
    }
}
