use ast::Ident;

use crate::{
    lex::{Lexer, Span, Token},
    util::{
        error::ErrorNode,
        info::{self, Node, Source},
    },
};

pub mod ast;

pub struct Parser<'a, 'b> {
    lex: Lexer<'a>,
    peek: Option<Option<Node<Token<'a>>>>,
    info: &'b mut info::CompilerInfo<'a>,
    source: Source<'a>,

    last_span: Span,
    curr_span: Span,
    eof: bool,
}

macro_rules! tok {
    ($pat:pat) => {
        Node($pat, _)
    };
}

const EMPTY_EXPR: ast::Expr<'static> = ast::Expr::Constant(ast::Literal::Bool(false));
const EMPTY_IDENT: &str = "__COMPILER__ERROR__PLACE__HOLDER__";

macro_rules! expect_tok {
    ($self:ident, $tok:pat, $span:pat $( => $blk:expr)?, $err:literal) => {
        match $self.next_tok(){
            Some(Node($tok, $span)) => {Some(($($blk)?))}
            Some(tok) => {
                $self.info.report_error(tok.error($self.info, format!($err, tok.0)));
                None
            }
            None => {
                $self.info.report_error(ErrorNode::span($self.source.eof(), $self.source, format!($err, "EOF")));
                None
            },
        }
    };
}

macro_rules! expect_tok_after {
    ($self:ident, $tok:pat, $span:pat $( => $blk:expr)?, $err:literal) => {
        match $self.next_tok(){
            Some(Node($tok, $span)) => {Some(($($blk)?))}
            Some(tok) => {
                $self.info.report_error(ErrorNode::span($self.last_span.immediately_after(), $self.source,  format!($err, tok.0)));
                None
            }
            None => {
                $self.info.report_error(ErrorNode::span($self.last_span.immediately_after(), $self.source, format!($err, "EOF")));
                None
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
        match $self.peek_next_tok() {
            Some(tok!($tok)) => true,
            _ => false,
        }
    };
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(source: Source<'a>, info: &'b mut info::CompilerInfo<'a>) -> Self {
        Self {
            lex: Lexer::new(source.contents),
            source,
            peek: None,
            info,
            last_span: Span::default(),
            curr_span: Span::default(),
            eof: false,
        }
    }

    pub fn parse(mut self) -> Result<ast::Program<'a>, ()> {
        let program = self.parse_impl();
        while let Some(tok) = self.next_tok() {
            self.info
                .report_error(tok.error(self.info, format!("unexpected token {:?}", tok.0)))
        }
        if self.info.has_errors() {
            Err(())
        } else {
            Ok(program)
        }
    }

    fn next_tok(&mut self) -> Option<Node<Token<'a>>> {
        if let Some(peek) = self.peek.take() {
            self.last_span = self.curr_span;
            self.curr_span = peek
                .as_ref()
                .map_or(self.source.eof(), |v| self.info.get_node_source(v.1).0);
            return peek;
        }

        for tok in &mut self.lex {
            match tok {
                Ok(ok) => {
                    self.last_span = self.curr_span;
                    self.curr_span = ok.span;
                    return Some(self.info.create_node(self.source, ok));
                }
                Err(err) => {
                    let node = self.info.create_node(self.source, *err);
                    self.info
                        .report_error(node.error(self.info, format!("{}", node.0)));
                }
            }
        }
        if !self.eof {
            self.last_span = self.curr_span;
            self.curr_span = self.source.eof();
        }
        self.eof = true;

        None
    }

    fn peek_next_tok(&mut self) -> Option<&Node<Token<'a>>> {
        if self.peek.is_none() {
            for tok in &mut self.lex {
                match tok {
                    Ok(ok) => {
                        self.peek = Some(Some(self.info.create_node(self.source, ok)));
                        break;
                    }
                    Err(err) => {
                        let node = self.info.create_node(self.source, *err);
                        self.info
                            .report_error(node.error(self.info, format!("{}", node.0)));
                    }
                }
            }
        }
        if let Some(peek) = &self.peek {
            return peek.as_ref();
        }
        None
    }

    fn parse_impl(&mut self) -> ast::Program<'a> {
        ast::Program(vec![ast::TopLevel::FunctionDef(self.parse_function())])
    }

    fn parse_path(&mut self) -> &'a str {
        expect_tok!(self, Token::Ident(ident), _ => ident, "expected path found {:?}")
            .unwrap_or(EMPTY_IDENT)
    }

    fn parse_type(&mut self) {
        expect_tok!(self, Token::Ident("i32"), _, "expected type found {:?}");
    }

    fn parse_function(&mut self) -> ast::FunctionDef<'a> {
        expect_tok!(self, Token::Fn, _, "expected 'fn' found {:?}");
        let ident = self.parse_path();
        expect_tok_after!(self, Token::LPar, _, "expected '(' found {:?}");
        expect_tok_after!(self, Token::RPar, _, "expected ')' found {:?}");
        consume_if!(self,
            @consume Some(tok!(Token::SmallRightArrow)) => self.parse_type()
            _ => {}
        );
        expect_tok_after!(self, Token::LBrace, _, "expected '{{' found {:?}");
        let mut body = Vec::new();
        while !is_tok!(self, Token::RBrace) && self.peek_next_tok().is_some() {
            body.push(self.parse_block_item());
        }
        expect_tok_after!(self, Token::RBrace, _, "expected '}}' found {:?}");
        ast::FunctionDef { name: ident, body }
    }

    fn parse_block_item(&mut self) -> ast::BlockItem<'a> {
        consume_if!(self,
            @consume Some(tok!(Token::Let)) =>  ast::BlockItem::Declaration(self.parse_declaration()),
            _ => ast::BlockItem::Statement(self.parse_statement())
        )
    }

    fn parse_declaration(&mut self) -> ast::Declaration<'a> {
        let name = self.parse_path();
        self.parse_colon();
        self.parse_type();
        let expr = consume_if!(self,
            @consume Some(tok!(Token::Assignment)) => Some(self.parse_expression()),
            _ => None
        );
        self.parse_semicolon();
        ast::Declaration {
            name: ast::Ident::new(name),
            expr,
        }
    }

    fn parse_semicolon(&mut self) {
        match self.next_tok() {
            Some(Node(Token::Semicolon, _)) => {}
            Some(Node(tok, _)) => {
                self.info.report_error(ErrorNode::span(
                    self.last_span.immediately_after(),
                    self.source,
                    format!("expected ';' found {:?}", tok),
                ));
            }
            None => {
                self.info.report_error(ErrorNode::span(
                    self.last_span.immediately_after(),
                    self.source,
                    "expected ';' found EOF".into(),
                ));
            }
        }
    }

    fn parse_colon(&mut self) {
        match self.next_tok() {
            Some(Node(Token::Colon, _)) => {}
            Some(Node(tok, _)) => {
                self.info.report_error(ErrorNode::span(
                    self.last_span.immediately_after(),
                    self.source,
                    format!("expected ';' found {:?}", tok),
                ));
            }
            None => {
                self.info.report_error(ErrorNode::span(
                    self.last_span.immediately_after(),
                    self.source,
                    "expected ';' found EOF".into(),
                ));
            }
        }
    }

    fn parse_statement(&mut self) -> ast::Statement<'a> {
        let stmt = consume_if!(self,
            @consume Some(tok!(Token::Return)) => ast::Statement::Return(self.parse_expression())
            Some(tok!(Token::Semicolon)) => ast::Statement::Empty,
            Some(tok) => ast::Statement::Expression(self.parse_expression()),
            None => {
                self.info.report_error(ErrorNode::span(self.source.eof(), self.source, "expected statement but is at EOF".into()));
                return ast::Statement::Empty;
            }
        );
        self.parse_semicolon();
        stmt
    }

    fn parse_expression(&mut self) -> ast::Expr<'a> {
        self.parse_expression_13(0)
    }

    fn parse_expression_13(&mut self, min_prec: usize) -> ast::Expr<'a> {
        let mut lhs = self.parse_expression_14();
        loop {
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

                @consume Some(tok!(Token::Assignment)) if ast::BinaryOp::Assignment.precedence() >= min_prec => ast::BinaryOp::Assignment,
                @consume Some(tok!(Token::PlusEq)) if ast::BinaryOp::PlusEq.precedence() >= min_prec => ast::BinaryOp::PlusEq,
                @consume Some(tok!(Token::MinusEq)) if ast::BinaryOp::MinusEq.precedence() >= min_prec => ast::BinaryOp::MinusEq,
                @consume Some(tok!(Token::TimesEq)) if ast::BinaryOp::TimesEq.precedence() >= min_prec => ast::BinaryOp::TimesEq,
                @consume Some(tok!(Token::DivideEq)) if ast::BinaryOp::DivideEq.precedence() >= min_prec => ast::BinaryOp::DivideEq,
                @consume Some(tok!(Token::ModuloEq)) if ast::BinaryOp::ModuloEq.precedence() >= min_prec => ast::BinaryOp::ModuloEq,
                @consume Some(tok!(Token::ShiftLeftEq)) if ast::BinaryOp::ShiftLeftEq.precedence() >= min_prec => ast::BinaryOp::ShiftLeftEq,
                @consume Some(tok!(Token::ShiftRightEq)) if ast::BinaryOp::ShiftRightEq.precedence() >= min_prec => ast::BinaryOp::ShiftRightEq,
                @consume Some(tok!(Token::AndEq)) if ast::BinaryOp::AndEq.precedence() >= min_prec => ast::BinaryOp::AndEq,
                @consume Some(tok!(Token::OrEq)) if ast::BinaryOp::OrEq.precedence() >= min_prec => ast::BinaryOp::OrEq,
                @consume Some(tok!(Token::XorEq)) if ast::BinaryOp::XorEq.precedence() >= min_prec => ast::BinaryOp::XorEq,
                _ => break
            );
            if op.left_to_right() {
                let rhs = self.parse_expression_13(op.precedence() + 1);
                lhs = ast::Expr::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            } else {
                let rhs = self.parse_expression_13(op.precedence());
                lhs = ast::Expr::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }
        }
        lhs
    }

    fn parse_expression_14(&mut self) -> ast::Expr<'a> {
        consume_if!(self,
            @consume Some(tok!(Token::Minus)) => ast::Expr::Unary(ast::UnaryOp::Neg, Box::new(self.parse_expression_14())),
            @consume Some(tok!(Token::BitwiseNot)) => ast::Expr::Unary(ast::UnaryOp::BitNot, Box::new(self.parse_expression_14())),
            @consume Some(tok!(Token::LogicalNot)) => ast::Expr::Unary(ast::UnaryOp::LogNot, Box::new(self.parse_expression_14())),

            @consume Some(tok!(Token::Inc)) => ast::Expr::Unary(ast::UnaryOp::PreInc, Box::new(self.parse_expression_14())),
            @consume Some(tok!(Token::Dec)) => ast::Expr::Unary(ast::UnaryOp::PreDec, Box::new(self.parse_expression_14())),

            _ => {
                let mut expr = self.parse_expression_15();
                loop{
                    consume_if!(self,
                        @consume Some(tok!(Token::Inc)) => expr = ast::Expr::Unary(ast::UnaryOp::PostInc, Box::new(expr)),
                        @consume Some(tok!(Token::Dec)) => expr = ast::Expr::Unary(ast::UnaryOp::PostDec, Box::new(expr)),
                        _ => return expr
                    );
                }

            }
        )
    }

    fn parse_expression_15(&mut self) -> ast::Expr<'a> {
        consume_if!(self,
            @consume Some(tok!(Token::NumericLiteral(num))) => ast::Expr::Constant(ast::Literal::Number(num)),
            @consume Some(tok!(Token::FalseLiteral)) => ast::Expr::Constant(ast::Literal::Bool(false)),
            @consume Some(tok!(Token::TrueLiteral)) => ast::Expr::Constant(ast::Literal::Bool(true)),
            @consume Some(tok!(Token::CharLiteral(char))) => ast::Expr::Constant(ast::Literal::Char(char)),
            @consume Some(tok!(Token::Ident(name))) => ast::Expr::Ident(Ident::new(name)),

            @consume Some(tok!(Token::LPar)) => {
                let expr = self.parse_expression();
                expect_tok!(self, Token::RPar, _, "expected ')' found {:?}");
                expr
            },

            @consume Some(tok) => {
                self.info.report_error(tok.error(self.info, format!("unexpected token {:?}", tok.0)));
                EMPTY_EXPR
            }
            None => {
                self.info.report_error(ErrorNode::span(self.curr_span.immediately_after(), self.source, "unexpected EOF".into()));
                EMPTY_EXPR
            }
        )
    }
}
