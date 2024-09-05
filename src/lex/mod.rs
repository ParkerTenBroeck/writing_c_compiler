pub mod number;
pub mod sstr;
pub mod token;

pub use number::*;
use std::{iter::Peekable, str::Chars};
pub use token::*;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(super) struct Position {
    offset: usize,
    line: usize,
    col: usize,
}

type LexerResult<'a> = Result<Spanned<Token<'a>>, Box<Spanned<LexError<'a>>>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexError<'a> {
    InvalidChar(char),
    EmptyCharLiteral,
    UnclosedCharLiteral,
    CharLiteralTooBig,
    UnclosedMultiLineComment,
    InvalidEscape(&'a str),
    UnfinishedEscapeSequence(&'a str),
    UnclosedStringLiteral,
    EmptyExponent,
    InvalidBase2Digit(char),
    NoNumberAfterBasePrefix,
    NumberParseError(NumberError),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum EscapeReturn {
    String,
    Char,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum State {
    Default,

    Plus,
    Minus,
    Times,
    Divide,
    Mod,
    Equal,
    Gt,
    GtGt,
    Lt,
    LtLt,
    Not,
    Or,
    And,
    Xor,

    Dot,
    DotDot,

    Ident,

    SingleLine,
    MultiLine(u16),
    MultiLineOpen1(u16),
    MultiLineClose1(u16),

    CharLiteral,
    CharLiteralEnd,
    CharLiteralLarge,

    String,

    EscapeStart(EscapeReturn),

    Eof,

    NumericStartZero,
    NumericStart,
    NumericDecimal,
    NumericDecimalNumberE,
    NumericDecimalNumberENumber,
    NumericBinStart,
    NumericHexStart,
    NumericDecimalNumberEPM,
    NumericBin,
    NumericHex,
    //EscapeSkipWhitespace(EscapeReturn),
    NumericSuffix,
}

pub struct Lexer<'a> {
    str: &'a str,
    chars: Peekable<Chars<'a>>,
    state: State,

    start: Position,
    current: Position,
    escape_start: Position,

    numeric_start: usize,
    type_hint: TypeHint,
    suffix_start: usize,

    include_comments: bool,
}

fn ident(ident: &str) -> Token {
    match ident {
        "true" => Token::TrueLiteral,
        "false" => Token::FalseLiteral,
        "return" => Token::Return,
        "let" => Token::Let,
        "for" => Token::For,
        "fn" => Token::Fn,
        "while" => Token::While,
        "loop" => Token::Loop,
        "if" => Token::If,
        o => Token::Ident(o),
    }
}

impl<'a> Lexer<'a> {
    pub fn new(str: &'a str) -> Self {
        Self {
            str,
            chars: str.chars().peekable(),
            state: State::Default,
            start: Position::default(),
            current: Position::default(),
            escape_start: Position::default(),
            include_comments: false,
            numeric_start: 0,
            suffix_start: 0,
            type_hint: TypeHint::Int,
        }
    }

    pub fn include_comments(mut self) -> Self {
        self.include_comments = true;
        self
    }

    // fn escape_char_finish(&mut self, char: char, er: EscapeReturn, c_lit_char: &mut char) {
    //     match er {
    //         EscapeReturn::String => {
    //             match self.str_builder.take() {
    //                 SSBuilder::None => {
    //                     let mut buf = StrBuf::new();
    //                     // should never fail
    //                     buf.write_char(char).unwrap();
    //                     self.str_builder = SSBuilder::Small(buf)
    //                 }
    //                 SSBuilder::Ref(str) => {
    //                     if str.len() + char.len_utf8() <= 15 {
    //                         let mut buf = StrBuf::new();
    //                         // should never fail
    //                         buf.write_str(str).unwrap();
    //                         buf.write_char(char).unwrap();
    //                         self.str_builder = SSBuilder::Small(buf)
    //                     } else {
    //                         let mut buf = str.to_string();
    //                         buf.push(char);
    //                         self.str_builder = SSBuilder::Alloc(buf);
    //                     }
    //                 }
    //                 SSBuilder::Small(mut buf) => {
    //                     if buf.write_char(char).is_ok() {
    //                         self.str_builder = SSBuilder::Small(buf);
    //                     } else {
    //                         let mut buf = buf.to_string();
    //                         buf.push(char);
    //                         self.str_builder = SSBuilder::Alloc(buf);
    //                     }
    //                 }
    //                 SSBuilder::Alloc(mut buf) => {
    //                     buf.push(char);
    //                     self.str_builder = SSBuilder::Alloc(buf);
    //                 }
    //             }
    //             self.state = State::String;
    //         }
    //         EscapeReturn::Char => {
    //             self.state = State::CharLiteralEnd;
    //             *c_lit_char = char;
    //         }
    //     }
    // }

    // fn unfinished_escape_sequence(
    //     &mut self,
    //     ret_state: EscapeReturn,
    //     processing: &Position,
    // ) -> (
    //     Option<Result<Token<'a>, TokenizerError<'a>>>,
    //     Option<TokenMeta>,
    //     State,
    // ) {
    //     let meta = TokenMeta {
    //         line: self.escape_start.line as u32,
    //         col: self.escape_start.col as u32,
    //         offset: self.escape_start.offset as u32,
    //         len: (processing.offset - self.escape_start.offset) as u32,
    //     };
    //     let state = match ret_state {
    //         EscapeReturn::String => State::String,
    //         EscapeReturn::Char => State::CharLiteralEnd,
    //     };
    //     let ret = Some(Err(TokenizerError::UnfinishedEscapeSequence(
    //         &self.str[self.escape_start.offset..processing.offset],
    //     )));
    //     let error_meta = Some(meta);
    //     (ret, error_meta, state)
    // }

    // fn invalid_escape_sequence(
    //     &mut self,
    //     ret_state: EscapeReturn,
    //     processing: &Position,
    // ) -> (
    //     Option<Result<Token<'a>, TokenizerError<'a>>>,
    //     Option<TokenMeta>,
    //     State,
    // ) {
    //     let meta = TokenMeta {
    //         line: self.escape_start.line as u32,
    //         col: self.escape_start.col as u32,
    //         offset: self.escape_start.offset as u32,
    //         len: (processing.offset - self.escape_start.offset) as u32,
    //     };
    //     let state = match ret_state {
    //         EscapeReturn::String => State::String,
    //         EscapeReturn::Char => State::CharLiteralEnd,
    //     };
    //     let ret = Some(Err(TokenizerError::InvalidEscape(
    //         &self.str[self.escape_start.offset..processing.offset],
    //     )));
    //     let error_meta = Some(meta);
    //     (ret, error_meta, state)
    // }

    fn disambiguate_dot(&self) -> DisambiguateDot {
        let mut clone = self.chars.clone();
        clone.next();
        match clone.next() {
            Some(c) if c == '_' || c.is_alphabetic() => DisambiguateDot::Dot,
            _ => DisambiguateDot::Numeric,
        }
    }
}

enum DisambiguateDot {
    Numeric,
    Dot,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexerResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut char_lit = '\0';
        let mut update_start_on_error = true;
        let mut error_meta = None;

        let mut ret = None;
        let ok_ret_state = State::Default;
        let mut err_ret_state = State::Default;
        loop {
            let c = self.chars.peek().copied();
            // self.str.chars().clone()
            let mut consume = true;

            let processing = if let Some(char) = c {
                let mut tmp = self.current;
                tmp.offset += char.len_utf8();
                if char == '\n' {
                    tmp.line += 1;
                    tmp.col = 0;
                } else {
                    tmp.col += 1;
                }
                tmp
            } else {
                self.current
            };

            macro_rules! eof_none {
                ($expr:expr) => {
                    if let Some(char) = $expr {
                        char
                    } else {
                        self.state = State::Eof;
                        return None;
                    }
                };
            }

            macro_rules! unconsume_ret {
                ($sel:ident, $expr:expr) => {{
                    consume = false;
                    ret = Some($expr);
                }};
            }

            match self.state {
                State::Default => match eof_none!(c) {
                    '|' => self.state = State::Or,
                    '^' => self.state = State::Xor,
                    '/' => self.state = State::Divide,
                    '%' => self.state = State::Mod,
                    '-' => self.state = State::Minus,
                    '+' => self.state = State::Plus,
                    '*' => self.state = State::Times,
                    '=' => self.state = State::Equal,
                    '.' => self.state = State::Dot,
                    '<' => self.state = State::Lt,
                    '>' => self.state = State::Gt,
                    '!' => self.state = State::Not,
                    '&' => self.state = State::And,
                    '"' => self.state = State::String,
                    '\'' => self.state = State::CharLiteral,

                    '(' => ret = Some(Ok(Token::LPar)),
                    ')' => ret = Some(Ok(Token::RPar)),
                    '{' => ret = Some(Ok(Token::LBrace)),
                    '}' => ret = Some(Ok(Token::RBrace)),
                    '[' => ret = Some(Ok(Token::LBracket)),
                    ']' => ret = Some(Ok(Token::RBracket)),
                    '~' => ret = Some(Ok(Token::BitwiseNot)),
                    ',' => ret = Some(Ok(Token::Comma)),
                    '?' => ret = Some(Ok(Token::QuestionMark)),
                    ';' => ret = Some(Ok(Token::Semicolon)),
                    ':' => ret = Some(Ok(Token::Colon)),
                    '@' => ret = Some(Ok(Token::At)),
                    '$' => ret = Some(Ok(Token::Ampersand)),
                    '#' => ret = Some(Ok(Token::Octothorp)),

                    '0' => {
                        self.numeric_start = self.current.offset;
                        self.state = State::NumericStartZero;
                    }
                    '1'..='9' => {
                        self.numeric_start = self.current.offset;
                        self.state = State::NumericStart;
                    }

                    c if c.is_whitespace() => self.start = processing,
                    c if c.is_alphabetic() || c == '_' => self.state = State::Ident,

                    c => ret = Some(Err(LexError::InvalidChar(c))),
                },
                State::Plus => match c {
                    Some('=') => ret = Some(Ok(Token::PlusEq)),
                    Some('+') => ret = Some(Ok(Token::Inc)),
                    _ => unconsume_ret!(self, Ok(Token::Plus)),
                },
                State::Minus => match c {
                    Some('>') => ret = Some(Ok(Token::SmallRightArrow)),
                    Some('=') => ret = Some(Ok(Token::MinusEq)),
                    Some('-') => ret = Some(Ok(Token::Dec)),
                    _ => unconsume_ret!(self, Ok(Token::Minus)),
                },
                State::Times => match c {
                    Some('=') => ret = Some(Ok(Token::TimesEq)),
                    _ => unconsume_ret!(self, Ok(Token::Star)),
                },
                State::Divide => match c {
                    Some('=') => ret = Some(Ok(Token::DivideEq)),
                    Some('/') => self.state = State::SingleLine,
                    Some('*') => self.state = State::MultiLine(0),
                    _ => unconsume_ret!(self, Ok(Token::Slash)),
                },
                State::Mod => match c {
                    Some('=') => ret = Some(Ok(Token::ModuloEq)),
                    _ => unconsume_ret!(self, Ok(Token::Percent)),
                },
                State::Equal => match c {
                    Some('>') => ret = Some(Ok(Token::BigRightArrow)),
                    Some('=') => ret = Some(Ok(Token::Assignment)),
                    _ => unconsume_ret!(self, Ok(Token::Equals)),
                },
                State::Gt => match c {
                    Some('=') => ret = Some(Ok(Token::GreaterThanEq)),
                    Some('>') => self.state = State::GtGt,
                    _ => unconsume_ret!(self, Ok(Token::GreaterThan)),
                },
                State::GtGt => match c {
                    Some('=') => ret = Some(Ok(Token::ShiftRightEq)),
                    _ => unconsume_ret!(self, Ok(Token::ShiftRight)),
                },
                State::Lt => match c {
                    Some('=') => ret = Some(Ok(Token::LessThanEq)),
                    Some('<') => self.state = State::LtLt,
                    _ => unconsume_ret!(self, Ok(Token::LessThan)),
                },
                State::LtLt => match c {
                    Some('=') => ret = Some(Ok(Token::ShiftLeftEq)),
                    _ => unconsume_ret!(self, Ok(Token::ShiftLeft)),
                },
                State::Not => match c {
                    Some('=') => ret = Some(Ok(Token::NotEquals)),
                    _ => unconsume_ret!(self, Ok(Token::LogicalNot)),
                },
                State::Or => match c {
                    Some('=') => ret = Some(Ok(Token::OrEq)),
                    Some('|') => ret = Some(Ok(Token::LogicalOr)),
                    _ => unconsume_ret!(self, Ok(Token::BitwiseOr)),
                },
                State::And => match c {
                    Some('=') => ret = Some(Ok(Token::AndEq)),
                    Some('&') => ret = Some(Ok(Token::LogicalAnd)),
                    _ => unconsume_ret!(self, Ok(Token::Ampersand)),
                },
                State::Xor => match c {
                    Some('=') => ret = Some(Ok(Token::XorEq)),
                    _ => unconsume_ret!(self, Ok(Token::BitwiseXor)),
                },
                State::Dot => match c {
                    Some('.') => self.state = State::DotDot,
                    _ => unconsume_ret!(self, Ok(Token::Dot)),
                },
                State::DotDot => match c {
                    Some('=') => ret = Some(Ok(Token::RangeInclusive)),
                    _ => unconsume_ret!(self, Ok(Token::RangeExclusive)),
                },
                State::Ident => match c {
                    Some(c) if c.is_alphanumeric() || c == '_' => {}
                    _ => unconsume_ret!(
                        self,
                        Ok(ident(&self.str[self.start.offset..self.current.offset]))
                    ),
                },
                State::CharLiteral => match c {
                    Some('\'') => ret = Some(Err(LexError::EmptyCharLiteral)),
                    Some('\n') => ret = Some(Err(LexError::UnclosedCharLiteral)),
                    Some('\\') => {
                        self.escape_start = self.current;
                        self.state = State::EscapeStart(EscapeReturn::Char);
                    }
                    Some(c) => {
                        self.state = State::CharLiteralEnd;
                        char_lit = c;
                    }
                    None => ret = Some(Err(LexError::UnclosedCharLiteral)),
                },
                State::CharLiteralEnd => match c {
                    Some('\'') => ret = Some(Ok(Token::CharLiteral(char_lit))),
                    None | Some('\n') => ret = Some(Err(LexError::UnclosedCharLiteral)),
                    Some(_) => self.state = State::CharLiteralLarge,
                },
                State::CharLiteralLarge => match c {
                    Some('\'') => ret = Some(Err(LexError::CharLiteralTooBig)),
                    None | Some('\n') => ret = Some(Err(LexError::UnclosedCharLiteral)),
                    _ => {}
                },

                State::String => match c {
                    Some('"') => {
                        ret = Some(Ok(Token::StringLiteral(
                            self.str[self.start.offset + 1..self.current.offset].into(),
                        )))
                    }
                    Some('\\') => {
                        self.escape_start = self.current;
                        self.state = State::EscapeStart(EscapeReturn::String);
                    }
                    Some(_) => {}
                    None => ret = Some(Err(LexError::UnclosedStringLiteral)),
                },
                State::EscapeStart(_ret_state) => todo!(),
                // match c {
                //     Some('0') => self.escape_char_finish('\0', ret_state, &mut char_lit),
                //     Some('n') => self.escape_char_finish('\n', ret_state, &mut char_lit),
                //     Some('r') => self.escape_char_finish('\r', ret_state, &mut char_lit),
                //     Some('t') => self.escape_char_finish('\t', ret_state, &mut char_lit),
                //     Some('\\') => self.escape_char_finish('\\', ret_state, &mut char_lit),
                //     Some('\'') => self.escape_char_finish('\'', ret_state, &mut char_lit),
                //     Some('"') => self.escape_char_finish('"', ret_state, &mut char_lit),
                //     Some('\n') if ret_state == EscapeReturn::String => {
                //         match self.str_builder.take() {
                //             SSBuilder::None => {
                //                 self.str_builder = SSBuilder::Small(str_buf::StrBuf::new())
                //             }
                //             SSBuilder::Ref(str) => {
                //                 if str.len() <= 15 {
                //                     let mut tmp = str_buf::StrBuf::new();
                //                     // this should never fail
                //                     assert_eq!(tmp.push_str(str), str.len());
                //                     self.str_builder = SSBuilder::Small(tmp);
                //                 } else {
                //                     self.str_builder = SSBuilder::Alloc(str.into());
                //                 }
                //             }
                //             i @ (SSBuilder::Small(_) | SSBuilder::Alloc(_)) => self.str_builder = i,
                //         }
                //         self.state = State::EscapeSkipWhitespace(ret_state);
                //     }
                //     Some(_) => {
                //         update_start_on_error = false;
                //         (ret, error_meta, err_ret_state) =
                //             self.invalid_escape_sequence(ret_state, &processing)
                //     }

                //     None => {
                //         update_start_on_error = false;
                //         (ret, error_meta, err_ret_state) =
                //             self.unfinished_escape_sequence(ret_state, &processing)
                //     }
                // },
                // State::EscapeSkipWhitespace(ret_state) => match c {
                //     Some(c) if c.is_whitespace() => {}
                //     _ => {
                //         consume = false;
                //         match ret_state {
                //             EscapeReturn::Char => self.state = State::CharLiteralEnd,
                //             EscapeReturn::String => self.state = State::String,
                //         }
                //     }
                // },
                State::SingleLine => match c {
                    Some('\n') => {
                        ret = Some(Ok(Token::SingleLineComment(
                            self.str[self.start.offset + 2 * '/'.len_utf8()..self.current.offset]
                                .into(),
                        )))
                    }
                    Some(_) => {}
                    None => {
                        ret = Some(Ok(Token::SingleLineComment(
                            self.str[self.start.offset + 2 * '/'.len_utf8()..self.current.offset]
                                .into(),
                        )))
                    }
                },
                State::MultiLine(indent) => match c {
                    Some('/') => self.state = State::MultiLineOpen1(indent),
                    Some('*') => self.state = State::MultiLineClose1(indent),
                    Some(_) => {}
                    None => ret = Some(Err(LexError::UnclosedMultiLineComment)),
                },
                State::MultiLineOpen1(indent) => match c {
                    Some('*') => self.state = State::MultiLine(indent + 1),
                    Some(_) => {}
                    None => ret = Some(Err(LexError::UnclosedMultiLineComment)),
                },
                State::MultiLineClose1(indent) => match (indent, c) {
                    (0, Some('/')) => {
                        ret = Some(Ok(Token::MultiLineComment(
                            self.str[self.start.offset + ('*'.len_utf8() + '/'.len_utf8())
                                ..processing.offset - ('*'.len_utf8() + '/'.len_utf8())]
                                .into(),
                        )))
                    }
                    (indent, Some('/')) => self.state = State::MultiLine(indent - 1),
                    (_, None) => ret = Some(Err(LexError::UnclosedMultiLineComment)),
                    _ => {}
                },

                State::NumericStart => match c {
                    Some('0'..='9') => {}
                    Some('.') => match self.disambiguate_dot() {
                        DisambiguateDot::Numeric => self.state = State::NumericDecimal,
                        DisambiguateDot::Dot => {
                            consume = false;
                            ret = Some(
                                Number::new(
                                    &self.str[self.numeric_start..self.current.offset],
                                    TypeHint::Int,
                                )
                                .map(Token::NumericLiteral)
                                .map_err(LexError::NumberParseError),
                            );
                        }
                    },
                    Some('e') => {
                        self.state = State::NumericDecimalNumberE;
                    }
                    Some('_') => {}
                    Some('a'..='z' | 'A'..='Z') => {
                        self.suffix_start = self.current.offset;
                        self.type_hint = TypeHint::Int;
                        self.state = State::NumericSuffix;
                    }
                    _ => {
                        consume = false;
                        //TODO remove unwrap
                        ret = Some(
                            Number::new(
                                &self.str[self.numeric_start..self.current.offset],
                                TypeHint::Int,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::NumericStartZero => match c {
                    Some('b') => {
                        self.numeric_start = processing.offset;
                        self.state = State::NumericBinStart;
                    }
                    Some('x') => {
                        self.numeric_start = processing.offset;
                        self.state = State::NumericHexStart;
                    }
                    Some('0'..='9') => {
                        self.state = State::NumericStart;
                    }
                    Some('.') => match self.disambiguate_dot() {
                        DisambiguateDot::Numeric => self.state = State::NumericDecimal,
                        DisambiguateDot::Dot => {
                            consume = false;
                            ret = Some(
                                Number::new(
                                    &self.str[self.numeric_start..self.current.offset],
                                    TypeHint::Int,
                                )
                                .map(Token::NumericLiteral)
                                .map_err(LexError::NumberParseError),
                            );
                        }
                    },
                    Some('_') => {}
                    Some('a'..='z' | 'A'..='Z') => {
                        self.suffix_start = self.current.offset;
                        self.type_hint = TypeHint::Int;
                        self.state = State::NumericSuffix;
                    }
                    _ => {
                        consume = false;
                        ret = Some(
                            Number::new(
                                &self.str[self.numeric_start..self.current.offset],
                                TypeHint::Int,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::NumericDecimal => match c {
                    Some('0'..='9') => {}
                    Some('e') => {
                        self.state = State::NumericDecimalNumberE;
                    }
                    Some('_') => {}
                    Some('a'..='z' | 'A'..='Z') => {
                        self.suffix_start = self.current.offset;
                        self.type_hint = TypeHint::Float;
                        self.state = State::NumericSuffix;
                    }
                    _ => {
                        consume = false;
                        ret = Some(
                            Number::new(
                                &self.str[self.numeric_start..self.current.offset],
                                TypeHint::Float,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::NumericDecimalNumberE => match c {
                    Some('0'..='9') => {
                        self.state = State::NumericDecimalNumberENumber;
                    }
                    Some('+' | '-') => {
                        self.state = State::NumericDecimalNumberEPM;
                    }
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(Err(LexError::EmptyExponent));
                    }
                },
                State::NumericDecimalNumberEPM => match c {
                    Some('0'..='9') => {
                        self.state = State::NumericDecimalNumberENumber;
                    }
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(Err(LexError::EmptyExponent));
                    }
                },
                State::NumericDecimalNumberENumber => match c {
                    Some('0'..='9' | '_') => {}
                    Some('a'..='z' | 'A'..='Z') => {
                        self.suffix_start = self.current.offset;
                        self.type_hint = TypeHint::Float;
                        self.state = State::NumericSuffix;
                    }
                    _ => {
                        consume = false;
                        ret = Some(
                            Number::new(
                                &self.str[self.numeric_start..self.current.offset],
                                TypeHint::Float,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::NumericBinStart => match c {
                    Some('0'..='1') => {
                        self.state = State::NumericBin;
                    }
                    Some('_') => {}
                    Some(c @ '2'..='9') => {
                        err_ret_state = State::NumericBin;
                        error_meta = Some(Span::start_end(self.current, processing));
                        update_start_on_error = false;
                        ret = Some(Err(LexError::InvalidBase2Digit(c)))
                    }
                    _ => {
                        consume = false;
                        ret = Some(Err(LexError::NoNumberAfterBasePrefix))
                    }
                },
                State::NumericBin => match c {
                    Some('0'..='1') => {}
                    Some('_') => {}
                    Some(c @ '2'..='9') => {
                        err_ret_state = State::NumericBin;
                        error_meta = Some(Span::start_end(self.current, processing));
                        update_start_on_error = false;
                        ret = Some(Err(LexError::InvalidBase2Digit(c)))
                    }
                    Some('a'..='z' | 'A'..='Z') => {
                        self.suffix_start = self.current.offset;
                        self.type_hint = TypeHint::Bin;
                        self.state = State::NumericSuffix;
                    }
                    _ => {
                        consume = false;
                        ret = Some(
                            Number::new(
                                &self.str[self.numeric_start..self.current.offset],
                                TypeHint::Bin,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::NumericHexStart => match c {
                    Some('0'..='9' | 'a'..='f' | 'A'..='F') => {
                        self.state = State::NumericHex;
                    }
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(Err(LexError::NoNumberAfterBasePrefix))
                    }
                },
                State::NumericHex => match c {
                    Some('0'..='9' | 'a'..='f' | 'A'..='F') => {}
                    Some('_') => {}
                    Some('g'..='z' | 'G'..='Z') => {
                        self.suffix_start = self.current.offset;
                        self.type_hint = TypeHint::Hex;
                        self.state = State::NumericSuffix;
                    }
                    _ => {
                        consume = false;
                        ret = Some(
                            Number::new(
                                &self.str[self.numeric_start..self.current.offset],
                                TypeHint::Hex,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::NumericSuffix => match c {
                    Some('0'..='9') => {}
                    _ => {
                        consume = false;
                        let len = self.suffix_start - self.numeric_start;
                        ret = Some(
                            Number::new_with_suffix(
                                &self.str[self.numeric_start..self.current.offset],
                                len,
                                self.type_hint,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::Eof => return None,
            }

            if consume {
                self.chars.next();
                self.current = processing;
            }

            if let Some(ret_res) = ret {
                match ret_res {
                    Ok(token) => {
                        let meta = Span::start_end(self.start, self.current);
                        self.start = self.current;
                        self.state = ok_ret_state;
                        if matches!(
                            token,
                            Token::MultiLineComment(_) | Token::SingleLineComment(_)
                        ) && !self.include_comments
                        {
                            ret = None;
                            continue;
                        }
                        return Some(Ok(Spanned::new(token, meta)));
                    }
                    Err(err) => {
                        let meta = error_meta.unwrap_or(Span::start_end(self.start, self.current));
                        if update_start_on_error {
                            self.start = self.current;
                        }
                        self.state = err_ret_state;
                        return Some(Err(Box::new(Spanned::new(err, meta))));
                    }
                }
            }
        }
    }
}
