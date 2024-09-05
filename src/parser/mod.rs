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
    (@consume, $self:ident, $tok:pat $(if $if:expr)? => $blk:expr) => {
        match $self.next_tok(){
            $tok $(if $if)? => $blk,
            _ => unreachable!(),
        }
    };
    (@, $self:ident, $tok:pat => $blk:expr) => {
        $blk
    };
    ($self:ident, $( $(@$consume:ident)? $tok:pat $(if $if:expr)? => $blk:expr $(,)?)* $(,)?) => {
        match $self.peek_next_tok(){
            $(
                #[allow(unused)]
                $tok $(if $if)? => {
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
        while self.next_tok().is_some() {}
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
        Ok(ast::Program(vec![ast::TopLevel::FunctionDef(self.parse_function()?)]))
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
        self.parse_expression_13(0)
    }

    fn parse_expression_13(&mut self, min_prec: usize) -> Result<ast::Expr<'a>, ()>{
        let mut lhs = self.parse_expression_14()?;
        loop{
            let op = consume_if!(self, 
                @consume Some(tok!(Token::Plus)) if ast::BinaryOp::Addition.precedence() >= min_prec => ast::BinaryOp::Addition,
                @consume Some(tok!(Token::Minus)) if ast::BinaryOp::Subtract.precedence() >= min_prec => ast::BinaryOp::Subtract,

                @consume Some(tok!(Token::Star)) if ast::BinaryOp::Multiply.precedence() >= min_prec => ast::BinaryOp::Multiply,
                @consume Some(tok!(Token::Slash)) if ast::BinaryOp::Divide.precedence() >= min_prec => ast::BinaryOp::Divide,
                @consume Some(tok!(Token::Percent)) if ast::BinaryOp::Remainder.precedence() >= min_prec => ast::BinaryOp::Remainder,

                @consume Some(tok!(Token::ShiftLeft)) if ast::BinaryOp::ShiftLeft.precedence() >= min_prec => ast::BinaryOp::ShiftLeft,
                @consume Some(tok!(Token::ShiftRight)) if ast::BinaryOp::ShiftRight.precedence() >= min_prec => ast::BinaryOp::ShiftRight,

                @consume Some(tok!(Token::Equals)) if ast::BinaryOp::Eq.precedence() >= min_prec => ast::BinaryOp::Eq,
                @consume Some(tok!(Token::NotEquals)) if ast::BinaryOp::Ne.precedence() >= min_prec => ast::BinaryOp::Ne,

                @consume Some(tok!(Token::LessThan)) if ast::BinaryOp::Lt.precedence() >= min_prec => ast::BinaryOp::Lt,
                @consume Some(tok!(Token::LessThanEq)) if ast::BinaryOp::Lte.precedence() >= min_prec => ast::BinaryOp::Lte,
                @consume Some(tok!(Token::GreaterThan)) if ast::BinaryOp::Gt.precedence() >= min_prec => ast::BinaryOp::Gt,
                @consume Some(tok!(Token::GreaterThanEq)) if ast::BinaryOp::Gte.precedence() >= min_prec => ast::BinaryOp::Gte,

                @consume Some(tok!(Token::Ampersand)) if ast::BinaryOp::BitAnd.precedence() >= min_prec => ast::BinaryOp::BitAnd,
                @consume Some(tok!(Token::BitwiseXor)) if ast::BinaryOp::BitXor.precedence() >= min_prec => ast::BinaryOp::BitXor,
                @consume Some(tok!(Token::BitwiseOr)) if ast::BinaryOp::BitOr.precedence() >= min_prec => ast::BinaryOp::BitOr,
                @consume Some(tok!(Token::LogicalAnd)) if ast::BinaryOp::LogAnd.precedence() >= min_prec => ast::BinaryOp::LogAnd,
                @consume Some(tok!(Token::LogicalOr)) if ast::BinaryOp::LogOr.precedence() >= min_prec => ast::BinaryOp::LogOr,
                _ => break
            );
            let rhs = self.parse_expression_13(op.precedence() + 1)?;
            lhs = ast::Expr::Binary { op, lhs: Box::new(lhs), rhs: Box::new(rhs) }
        }
        Ok(lhs)
    }

    fn parse_expression_14(&mut self) -> Result<ast::Expr<'a>, ()> {
        consume_if!(self,
            @consume Some(tok!(Token::Minus)) => Ok(ast::Expr::Unary(ast::UnaryOp::Neg, Box::new(self.parse_expression_14()?))),
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
