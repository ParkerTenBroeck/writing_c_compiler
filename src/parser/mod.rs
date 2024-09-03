use crate::lex::{self, Lexer, Spanned, Token};

pub mod ast;

#[derive(Debug)]
pub enum ParserError<'a> {
    LexerError(Spanned<lex::LexError<'a>>),
    UnexpectedToken(Spanned<Token<'a>>),
    ExpectedTokenFoundNone,
}

pub struct Parser<'a> {
    lex: Lexer<'a>,
    peek: Option<Option<Spanned<Token<'a>>>>,
    errors: Vec<ParserError<'a>>,
}

macro_rules! tok {
    ($pat:pat) => {
        Spanned { val: $pat, .. }
    };
}

macro_rules! expect_tok {
    ($self:ident, $tok:pat $( => $blk:expr)?) => {
        match $self.next_tok(){
            Some(tok!($tok)) => {Ok(($($blk)?))}
            Some(tok) => {
                $self.errors.push(ParserError::UnexpectedToken(tok));
                Err(())
            }
            None => {
                $self.errors.push(ParserError::ExpectedTokenFoundNone);
                Err(())
            },
        }
    };
}

macro_rules! consume_if {
    (@consume, $self:ident, $tok:pat => $blk:expr) => {
        match $self.next_tok(){
            $tok => $blk,
            _ => unreachable!(),
        }
    };
    (@, $self:ident, $tok:pat => $blk:expr) => {
        $blk
    };
    ($self:ident, $( $(@$consume:ident)? $tok:pat => $blk:expr $(,)?)* $(,)?) => {
        match $self.peek_next_tok(){
            $(
                #[allow(unused)]
                $tok => {
                    consume_if!(@$(
                        $consume
                    )?, $self, $tok => $blk)
                },
            )*
        }
    };
}

macro_rules! is_tok {
    ($self:ident, $tok:pat) => {
        match self.peek_next_tok() {
            Some(tok!($tok)) => true,
            _ => false,
        }
    };
}

impl<'a> Parser<'a> {
    pub fn new(str: &'a str) -> Self {
        Self {
            lex: Lexer::new(str),
            peek: None,
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Result<ast::Program<'a>, Vec<ParserError<'a>>> {
        let err = self.parse_impl();
        while let Some(_) = self.next_tok() {}
        match err {
            Ok(ok) => {
                if self.errors.is_empty() {
                    Ok(ok)
                } else {
                    Err(self.errors)
                }
            }
            Err(_) => Err(self.errors),
        }
    }

    fn next_tok(&mut self) -> Option<Spanned<Token<'a>>> {
        if let Some(peek) = self.peek.take() {
            return peek;
        }

        for tok in &mut self.lex {
            match tok {
                Ok(ok) => return Some(ok),
                Err(err) => self.errors.push(ParserError::LexerError(*err)),
            }
        }

        None
    }

    fn peek_next_tok(&mut self) -> Option<&Spanned<Token<'a>>> {
        if self.peek.is_none() {
            for tok in &mut self.lex {
                match tok {
                    Ok(ok) => {
                        self.peek = Some(Some(ok));
                        break;
                    }
                    Err(err) => self.errors.push(ParserError::LexerError(*err)),
                }
            }
        }
        if let Some(peek) = &self.peek {
            return peek.as_ref();
        }
        None
    }

    fn parse_impl(&mut self) -> Result<ast::Program<'a>, ()> {
        Ok(ast::Program::FunctionDef(self.parse_function()?))
    }

    fn parse_function(&mut self) -> Result<ast::FunctionDef<'a>, ()> {
        expect_tok!(self, Token::Ident("int"))?;
        let ident = expect_tok!(self, Token::Ident(ident) => ident)?;
        expect_tok!(self, Token::LPar)?;
        expect_tok!(self, Token::Ident("void"))?;
        expect_tok!(self, Token::RPar)?;
        expect_tok!(self, Token::LBrace)?;
        let body = self.parse_statement()?;
        expect_tok!(self, Token::RBrace)?;
        Ok(ast::FunctionDef { name: ident, body })
    }

    fn parse_statement(&mut self) -> Result<ast::Statement<'a>, ()> {
        let stmt = match self.next_tok() {
            Some(tok!(Token::Return)) => ast::Statement::Return(self.parse_expression()?),
            Some(tok) => {
                self.errors.push(ParserError::UnexpectedToken(tok));
                return Err(());
            }
            None => {
                self.errors.push(ParserError::ExpectedTokenFoundNone);
                return Err(());
            }
        };
        match self.next_tok() {
            Some(tok!(Token::Semicolon)) => Ok(stmt),
            Some(tok) => {
                self.errors.push(ParserError::UnexpectedToken(tok));
                Err(())
            }
            None => {
                self.errors.push(ParserError::ExpectedTokenFoundNone);
                Err(())
            }
        }
    }

    fn parse_expression(&mut self) -> Result<ast::Expr<'a>, ()> {
        self.parse_expression_14()
    }

    fn parse_expression_14(&mut self) -> Result<ast::Expr<'a>, ()> {
        consume_if!(self,
            @consume Some(tok!(Token::Minus)) => Ok(ast::Expr::Unary(ast::UnaryOp::Minus, Box::new(self.parse_expression_14()?))),
            @consume Some(tok!(Token::BitwiseNot)) => Ok(ast::Expr::Unary(ast::UnaryOp::Not, Box::new(self.parse_expression_14()?))),

            @consume Some(tok!(Token::Inc)) => Ok(ast::Expr::Unary(ast::UnaryOp::PreInc, Box::new(self.parse_expression_14()?))),
            @consume Some(tok!(Token::Dec)) => Ok(ast::Expr::Unary(ast::UnaryOp::PreDec, Box::new(self.parse_expression_14()?))),

            _ => self.parse_expression_15()
        )
    }

    fn parse_expression_15(&mut self) -> Result<ast::Expr<'a>, ()> {
        consume_if!(self,
            @consume Some(tok!(Token::NumericLiteral(num))) => Ok(ast::Expr::Constant(ast::Literal::Number(num))),
            @consume Some(tok!(Token::FalseLiteral)) => Ok(ast::Expr::Constant(ast::Literal::Bool(false))),
            @consume Some(tok!(Token::TrueLiteral)) => Ok(ast::Expr::Constant(ast::Literal::Bool(true))),
            @consume Some(tok!(Token::CharLiteral(char))) => Ok(ast::Expr::Constant(ast::Literal::Char(char))),

            @consume Some(tok!(Token::LPar)) => {
                let expr = self.parse_expression()?;
                expect_tok!(self, Token::RPar)?;
                Ok(expr)
            },

            @consume Some(tok) => {
                let err = ParserError::UnexpectedToken(tok);
                self.errors.push(err);
                Err(())
            }
            None => {
                self.errors.push(ParserError::ExpectedTokenFoundNone);
                Err(())
            }
        )
    }
}
