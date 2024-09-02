use super::Number;

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    LPar,
    RPar,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Plus,
    Minus,
    Star,
    Slash,
    Ampersand,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    ShiftLeft,
    ShiftRight,
    Percent,
    LogicalAnd,
    LogicalOr,
    LogicalNot,

    Dot,
    Comma,
    Colon,
    Semicolon,
    QuestionMark,
    At,
    Octothorp,
    Dollar,

    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Equals,
    NotEquals,

    Assignment,

    ModuloEq,
    Modulo,
    DivideEq,
    TimesEq,
    MinusEq,
    PlusEq,
    RangeInclusive,
    RangeExclusive,
    SmallRightArrow,
    BigRightArrow,
    OrEq,
    AndEq,
    XorEq,
    ShiftRightEq,
    ShiftLeftEq,

    Fn,
    Return,
    If,
    Else,
    While,
    Loop,
    Let,
    For,

    Struct,
    Enum,
    Union,
    Break,

    Label(&'a str),
    Ident(&'a str),

    StringLiteral(&'a str),
    NumericLiteral(Number<'a>),
    CharLiteral(char),

    FalseLiteral,
    TrueLiteral,

    SingleLineComment(&'a str),
    MultiLineComment(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub val: T,
}

impl<T> Spanned<T> {
    pub fn new(val: T, span: Span) -> Self {
        Self { val, span }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub line: u32,
    pub col: u32,
    pub offset: u32,
    pub len: u32,
}
impl Span {
    pub(crate) fn start_end(start: super::Position, end: super::Position) -> Self {
        Span {
            line: start.line as u32,
            col: start.col as u32,
            offset: start.offset as u32,
            len: (end.offset - start.offset) as u32,
        }
    }
}
