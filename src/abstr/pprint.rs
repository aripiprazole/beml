use super::*;

pub struct PrettyPrintTerm(pub Term);

impl Debug for PrettyPrintTerm {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
