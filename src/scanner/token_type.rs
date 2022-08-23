use std::fmt;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum TokenType {
    #[default]
    Undefined,
    OpenParen, CloseParen, OpenBrace, CloseBrace,
    Comma, Dot, Minus, Plus, Colon, Semicolon, Slash, Star,

    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    Identifier, String, Number,

    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Let, While, Use,

    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}