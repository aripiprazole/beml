use std::fmt::Display;

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
    #[token(r"[|+-*/><=,!:]+")]
    Infix,
    #[regex("//.*", logos::skip)]
    #[token("(*", lex_block_comment)]
    Skip,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Token::Ident => "<identifier>",
            Token::Meta => "<meta>",
            Token::Int => "<int>",
            Token::Text => "<string>",
            Token::Of => "of",
            Token::Let => "let",
            Token::Val => "val",
            Token::Type => "type",
            Token::In => "in",
            Token::If => "if",
            Token::Then => "then",
            Token::Else => "else",
            Token::Match => "match",
            Token::Function => "function",
            Token::With => "with",
            Token::Fun => "fun",
            Token::Bar => "|",
            Token::Equals => "=",
            Token::NotEquals => "!=",
            Token::Gt => "<",
            Token::Gte => "<=",
            Token::Lt => ">",
            Token::Lte => ">=",
            Token::DoubleArrow => "=>",
            Token::Arrow => "->",
            Token::Colon => ":",
            Token::Comma => ",",
            Token::Star => "*",
            Token::Div => "/",
            Token::Sum => "+",
            Token::Sub => "-",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBracket => "[",
            Token::RBracket => "]",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::Skip => "<skip>",
            Token::Infix => "<infix>",
        })
    }
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
