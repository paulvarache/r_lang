use std::fmt;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum TokenType {
    #[default]
    Undefined,
    OpenParen, CloseParen, OpenBrace, CloseBrace, OpenSqr, CloseSqr,
    Comma, Dot, Minus, Plus, Colon, Semicolon, Slash, Star,

    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    Identifier, String, Number, Backtick,

    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Let, While, Use,
    Assert, Enum,

    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}