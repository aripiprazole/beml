use super::*;
use crate::{concrete::*, loc::Identifier};

const EXPR_FIRST: &[Token] = &[Token::Let,
                               Token::Match,
                               Token::Function,
                               Token::If,
                               Token::Int,
                               Token::Ident,
                               Token::Text,
                               Token::LParen,
                               Token::LBracket];

const DECL_FIRST: &[Token] = &[Token::Let, Token::Type, Token::Val];

const INFIX_OPERATORS: &[Token] = &[Token::Gt,
                                    Token::Gte,
                                    Token::Lt,
                                    Token::Lte,
                                    Token::Equals,
                                    Token::NotEquals,
                                    Token::Star,
                                    Token::Arrow,
                                    Token::DoubleArrow,
                                    Token::Sum,
                                    Token::Sub,
                                    Token::Star,
                                    Token::Div];

const PRIMARY_FIRST: &[Token] = &[Token::Int, Token::Ident, Token::Text, Token::LParen, Token::LBracket];

#[derive(Clone, Copy)]
enum Lvl {
    Term,
    Type,
}

pub fn decl(p: &mut Parser) -> miette::Result<Term> {
    close!(p, match () {
        _ if p.check(Token::Let) => let_decl(p),
        _ if p.check(Token::Val) => val_decl(p),
        _ if p.check(Token::Type) => type_decl(p),
        _ => Ok(recover!(p, p.unexpected_token(DECL_FIRST))),
    })
}

fn let_decl(p: &mut Parser) -> miette::Result<Term> {
    expect_or_bail!(p, Token::Let);
    let pattern = recover!(p, primary(p, Lvl::Term));
    let parameters = parameters(p, Token::Equals)?;
    let body = recover!(p, expr(p, Lvl::Term, DECL_FIRST));

    Ok(LetDecl(LetDecl { pattern: pattern.into(),
                         body: body.into(),
                         parameters }))
}

fn val_decl(p: &mut Parser) -> miette::Result<Term> {
    expect_or_bail!(p, Token::Val);
    let name = identifier(p)?;
    expect_or_bail!(p, Token::Colon);
    let type_repr = recover!(p, expr(p, Lvl::Type, DECL_FIRST));
    Ok(ValDecl(ValDecl { name,
                         type_repr: type_repr.into() }))
}

fn type_decl(p: &mut Parser) -> miette::Result<Term> {
    expect_or_bail!(p, Token::Type);
    let variable = recover!(p, expr(p, Lvl::Type, &[Token::Ident]));
    let name = identifier(p)?;
    expect_or_bail!(p, Token::Equals);
    if p.check(Token::Bar) {
        p.eat(Token::Bar)?;
    }
    let mut cases = vec![recover!(p, constructor(p))];

    while p.at_any(&[Token::Bar]) {
        p.eat(Token::Bar)?;
        let constructor = recover!(p, constructor(p));
        cases.push(constructor);
    }
    Ok(TypeDecl(TypeDecl { name,
                           cases,
                           variable: variable.into() }))
}

fn constructor(p: &mut Parser) -> miette::Result<Term> {
    let name = identifier(p)?;
    if p.check(Token::Of) {
        expect_or_bail!(p, Token::Of);
        let type_repr = recover!(p, expr(p, Lvl::Type, &[Token::Bar]));
        return Ok(Constructor(Constructor { name,
                                            type_repr: Some(type_repr.into()) }));
    }
    Ok(Constructor(Constructor { name, type_repr: None }))
}

fn expr(p: &mut Parser, level: Lvl, stop_by: &[Token]) -> miette::Result<Term> {
    close!(p, match () {
        _ if p.check(Token::If) => if_expr(p),
        _ if p.check(Token::Let) => let_expr(p),
        _ if p.check(Token::Match) => match_expr(p),
        _ if p.check(Token::Fun) => fun_expr(p),
        _ if p.check(Token::Function) => function_expr(p),
        _ => ascription(p, level, stop_by),
    })
}

fn ascription(p: &mut Parser, level: Lvl, stop_by: &[Token]) -> miette::Result<Term> {
    let mut lhs = recover!(p, bin_op(p, level, stop_by));
    while p.at_any(&[Token::Colon]) {
        p.eat(Token::Colon)?;
        let rhs = recover!(p, bin_op(p, Lvl::Type, stop_by));
        lhs = BinOp(lhs.into(), BinOp::Colon, rhs.into())
    }
    Ok(lhs)
}

fn bin_op(p: &mut Parser, level: Lvl, stop_by: &[Token]) -> miette::Result<Term> {
    let mut lhs = recover!(p, equality(p, level, stop_by));
    while p.at_any(&[Token::DoubleArrow, Token::Arrow]) {
        let op = match p.curr {
            Some(Token::Arrow) => BinOp::Arrow,
            Some(Token::DoubleArrow) => BinOp::DoubleArrow,
            t => panic!("unexpected: {t:?}"),
        };
        p.bump();
        let rhs = recover!(p, equality(p, level, stop_by));
        lhs = BinOp(lhs.into(), op, rhs.into())
    }
    Ok(lhs)
}

fn equality(p: &mut Parser, level: Lvl, stop_by: &[Token]) -> miette::Result<Term> {
    let mut lhs = recover!(p, comparison(p, level, stop_by));
    while p.at_any(&[Token::Equals, Token::NotEquals]) {
        let (_, text, loc) = p.next()?;
        let op = crate::loc::Identifier { text: text.into(), loc };
        let rhs = recover!(p, comparison(p, level, stop_by));
        lhs = BinOp(lhs.into(), BinOp::UserDefined(op), rhs.into())
    }
    Ok(lhs)
}

fn comparison(p: &mut Parser, level: Lvl, stop_by: &[Token]) -> miette::Result<Term> {
    let mut lhs = recover!(p, term(p, level, stop_by));
    while p.at_any(&[Token::Gt, Token::Gte, Token::Lt, Token::Lte]) {
        let (_, text, loc) = p.next()?;
        let op = crate::loc::Identifier { text: text.into(), loc };
        let rhs = recover!(p, term(p, level, stop_by));
        lhs = BinOp(lhs.into(), BinOp::UserDefined(op), rhs.into())
    }
    Ok(lhs)
}

fn term(p: &mut Parser, level: Lvl, stop_by: &[Token]) -> miette::Result<Term> {
    let mut lhs = recover!(p, factor(p, level, stop_by));
    while p.at_any(&[Token::Sub, Token::Sum]) {
        let (_, text, loc) = p.next()?;
        let op = crate::loc::Identifier { text: text.into(), loc };
        let rhs = recover!(p, factor(p, level, stop_by));
        lhs = BinOp(lhs.into(), BinOp::UserDefined(op), rhs.into())
    }
    Ok(lhs)
}

fn factor(p: &mut Parser, level: Lvl, stop_by: &[Token]) -> miette::Result<Term> {
    let mut lhs = recover!(p, app(p, level, stop_by));
    while p.at_any(&[Token::Star, Token::Div]) {
        let (k, text, loc) = p.next()?;
        let op = match (level, k) {
            (Lvl::Type, Token::Star) => BinOp::Star,
            (_, _) => BinOp::UserDefined(crate::loc::Identifier { text: text.into(), loc }),
        };
        let rhs = recover!(p, app(p, level, stop_by));
        lhs = BinOp(lhs.into(), op, rhs.into())
    }
    Ok(lhs)
}

fn if_expr(p: &mut Parser) -> miette::Result<Term> {
    expect_or_bail!(p, Token::If);
    let condition = recover!(p, expr(p, Lvl::Term, &[Token::Then]));
    expect_or_bail!(p, Token::Then);
    let then = recover!(p, expr(p, Lvl::Term, &[Token::Else]));
    expect_or_bail!(p, Token::Else);
    let otherwise = recover!(p, expr(p, Lvl::Term, &[]));
    Ok(If(If { condition: condition.into(),
               then: then.into(),
               otherwise: otherwise.into() }))
}

fn let_expr(p: &mut Parser) -> miette::Result<Term> {
    expect_or_bail!(p, Token::Let);
    let pattern = recover!(p, primary(p, Lvl::Term));
    let parameters = parameters(p, Token::Equals)?;
    let body = recover!(p, expr(p, Lvl::Term, &[Token::In]));
    expect_or_bail!(p, Token::In);
    let next = recover!(p, expr(p, Lvl::Term, &[]));
    Ok(Let(Let { pattern: pattern.into(),
                 parameters,
                 body: body.into(),
                 next: next.into() }))
}

fn function_expr(p: &mut Parser) -> miette::Result<Term> {
    expect_or_bail!(p, Token::Function);
    if p.check(Token::Bar) {
        p.eat(Token::Bar)?;
    }
    let mut cases = vec![recover!(p, expr(p, Lvl::Term, &[Token::Bar]))];
    while p.at_any(&[Token::Bar]) {
        p.eat(Token::Bar)?;
        let case = recover!(p, expr(p, Lvl::Term, &[Token::Bar]));
        cases.push(case);
    }
    Ok(Function(Function { cases }))
}

fn match_expr(p: &mut Parser) -> miette::Result<Term> {
    expect_or_bail!(p, Token::Match);
    let scrutinee = recover!(p, expr(p, Lvl::Term, &[Token::With]));
    expect_or_bail!(p, Token::With);
    if p.check(Token::Bar) {
        p.eat(Token::Bar)?;
    }
    let mut cases = vec![recover!(p, expr(p, Lvl::Term, &[Token::Bar]))];
    while p.at_any(&[Token::Bar]) {
        p.eat(Token::Bar)?;
        let case = recover!(p, expr(p, Lvl::Term, &[Token::Bar]));
        cases.push(case);
    }
    Ok(Match(Match { scrutinee: scrutinee.into(),
                     cases }))
}

fn fun_expr(p: &mut Parser) -> miette::Result<Term> {
    expect_or_bail!(p, Token::Fun);
    let mut parameters = vec![];
    while p.check(Token::Ident) {
        let (_, text, loc) = p.next()?;
        parameters.push(crate::loc::Identifier { text: text.into(), loc })
    }
    expect_or_bail!(p, Token::Arrow);
    let body = recover!(p, expr(p, Lvl::Term, &[]));
    Ok(Fun(parameters, body.into()))
}

fn parameters(p: &mut Parser, stop_by: Token) -> miette::Result<Vec<Term>> {
    let mut parameters = vec![];
    loop {
        if p.check(stop_by) {
            p.bump();
            break;
        }

        parameters.push(recover!(p, primary(p, Lvl::Term)))
    }

    Ok(parameters)
}

fn app(p: &mut Parser, level: Lvl, stop_by: &[Token]) -> miette::Result<Term> {
    let mut lhs = recover!(p, primary(p, level));
    loop {
        if p.at_any(stop_by) || p.at_any(INFIX_OPERATORS) {
            break Ok(lhs);
        }

        if p.at_any(PRIMARY_FIRST) {
            let rhs = recover!(p, primary(p, level));
            lhs = Term::App(lhs.into(), rhs.into());
        } else {
            break Ok(lhs);
        }
    }
}

fn identifier(p: &mut Parser) -> miette::Result<Identifier> {
    match () {
        _ if p.check(Token::LParen) => {
            p.eat(Token::LParen)?;
            let op = if p.at_any(INFIX_OPERATORS) {
                let (_, text, loc) = p.next()?;
                crate::loc::Identifier { loc, text: text.into() }
            } else {
                p.unexpected_token(INFIX_OPERATORS)?
            };
            p.eat(Token::RParen)?;
            Ok(op)
        }
        _ if p.check(Token::Ident) => {
            let (text, loc) = p.eat(Token::Ident)?;
            Ok(crate::loc::Identifier { text: text.into(), loc })
        }
        _ => p.unexpected_token(&[Token::Ident, Token::LParen]),
    }
}

fn primary(p: &mut Parser, level: Lvl) -> miette::Result<Term> {
    close!(p, match () {
        _ if p.check(Token::Int) => {
            let (text, _) = p.eat(Token::Int)?;
            Ok(Int(text.parse().unwrap()))
        }
        _ if p.check(Token::Ident) => {
            let (text, loc) = p.eat(Token::Ident)?;
            Ok(Var(crate::loc::Identifier { text: text.into(), loc }))
        }
        _ if p.check(Token::Meta) => {
            let (text, loc) = p.eat(Token::Meta)?;
            Ok(Meta(crate::loc::Identifier { text: text[1..text.len()].into(),
                                             loc }))
        }
        _ if p.check(Token::Text) => {
            let (text, loc) = p.eat(Token::Text)?;
            Ok(Text(crate::loc::Text { value: text[1..text.len() - 1].into(),
                                       loc }))
        }
        _ if p.check(Token::LParen) => group_by(p, level, Token::LParen, Token::RParen, Parens),
        _ if p.check(Token::LBracket) => group_by(p, level, Token::LBracket, Token::RBracket, Brackets),
        _ => Ok(recover!(p, p.unexpected_token(EXPR_FIRST))),
    })
}

fn group_by<F>(p: &mut Parser, l: Lvl, initial: Token, end: Token, f: F) -> miette::Result<Term>
    where F: FnOnce(Box<Term>) -> Term {
    if p.check(initial) {
        expect_or_bail!(p, initial);
        let mut term = recover!(p, expr(p, l, &[Token::Comma]));
        while p.check(Token::Comma) {
            let rhs = recover!(p, expr(p, l, &[Token::Comma, end]));
            term = BinOp(term.into(), BinOp::Comma, rhs.into());
        }
        expect_or_bail!(p, end);
        Ok(f(term.into()))
    } else {
        Ok(recover!(p, p.unexpected_token(EXPR_FIRST)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(text: &str, f: impl FnOnce(&mut Parser) -> miette::Result<Term>) -> miette::Result<Term> {
        let mut p = Parser { file: PathBuf::new(),
                             lexer: Token::lexer(text),
                             curr: None,
                             text: text.into(),
                             errors: vec![],
                             terms: vec![],
                             gas: Cell::new(0) };
        p.next()?;
        f(&mut p)
    }

    #[test]
    fn test_expr() {
        assert!(parse("f x", |p| expr(p, Lvl::Term, &[])).is_ok());
    }

    #[test]
    fn test_let_expr() {
        assert!(parse("let x = 10 in x", |p| expr(p, Lvl::Term, &[])).is_ok());
    }

    #[test]
    fn test_let_f_x() {
        assert!(parse("let f x = x in f", |p| expr(p, Lvl::Term, &[])).is_ok());
    }

    #[test]
    fn test_let_mul_expr() {
        assert!(parse("20 * 10", |p| expr(p, Lvl::Term, &[])).is_ok());
    }

    #[test]
    fn test_let_pair_type() {
        assert!(parse("'a * 'a", |p| expr(p, Lvl::Type, &[])).is_ok());
    }

    #[test]
    fn test_app_type() {
        assert!(parse("'a list", |p| expr(p, Lvl::Type, &[])).is_ok());
    }

    #[test]
    fn test_app_group() {
        assert!(parse("(a list)", |p| expr(p, Lvl::Term, &[])).is_ok());
    }

    #[test]
    fn test_match() {
        assert!(parse("match list with | Nil => 0 | Cons x xs => 1", |p| expr(p, Lvl::Term, &[])).is_ok());
    }

    #[test]
    fn test_function() {
        assert!(parse("function | Nil => 0 | Cons x xs => 1", |p| expr(p, Lvl::Term, &[])).is_ok());
    }

    #[test]
    fn test_if() {
        assert!(parse("if true then false else true", |p| expr(p, Lvl::Term, &[])).is_ok());
    }

    #[test]
    fn test_app() {
        assert!(parse("f x", |p| app(p, Lvl::Term, &[])).is_ok());
    }
}
