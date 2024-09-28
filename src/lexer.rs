use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,
    #[regex(r"'[a-zA-Z_][a-zA-Z0-9_]*")]
    Meta,
    #[regex(r"[0-9]+")]
    Int,
    #[regex(r#""(?:[^"\\]|\\.)*""#)]
    Text,
    #[token("of")]
    Of,
    #[token("let")]
    Let,
    #[token("val")]
    Val,
    #[token("type")]
    Type,
    #[token("in")]
    In,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("match")]
    Match,
    #[token("function")]
    Function,
    #[token("with")]
    With,
    #[token("fun")]
    Fun,
    #[token("|")]
    Bar,
    #[token("=")]
    Equals,
    #[token("!=")]
    NotEquals,
    #[token(">")]
    Gt,
    #[token(">=")]
    Gte,
    #[token("<")]
    Lt,
    #[token("<=")]
    Lte,
    #[token("=>")]
    DoubleArrow,
    #[token("->")]
    Arrow,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("*")]
    Star,
    #[token("/")]
    Div,
    #[token("+")]
    Sum,
    #[token("-")]
    Sub,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[regex("//.*", logos::skip)]
    #[token("(*", lex_block_comment)]
    Skip,
}

#[derive(Logos)]
#[logos(skip "(?s).")]
enum BlockCommentToken {
    #[token("(*")]
    Open,
    #[token("*)")]
    Close,
}

pub fn lex_block_comment(lexer: &mut logos::Lexer<'_, Token>) -> logos::Skip {
    let mut comment_lexer = BlockCommentToken::lexer(lexer.source());
    comment_lexer.bump(lexer.span().end);
    let mut depth = 1;
    for token in &mut comment_lexer {
        match token {
            Ok(BlockCommentToken::Open) => depth += 1,
            Ok(BlockCommentToken::Close) => depth -= 1,
            Err(_) => unreachable!(),
        }
        if depth == 0 {
            break;
        }
    }
    lexer.bump(comment_lexer.span().end - lexer.span().end);
    logos::Skip
}
