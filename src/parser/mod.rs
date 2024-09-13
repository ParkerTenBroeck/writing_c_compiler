use crate::{
    lex::{Lexer, Span, Token},
    util::{
        error::ErrorNode,
        info::{self, Node, NodeId, Source},
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

    node_stack: Vec<Span>,
}

macro_rules! tok {
    ($pat:pat, $span:pat) => {
        Node($pat, $span)
    };
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
            node_stack: Vec::new(),
        }
    }

    pub fn start_node(&mut self) {
        let next = self.next_span();
        self.node_stack.push(next);
    }

    pub fn end_node_id(&mut self) -> Option<NodeId> {
        self.info.create_node_id(
            self.source,
            self.curr_span
                .combine(self.node_stack.pop().unwrap_or(self.curr_span)),
        )
    }

    pub fn end_node<T>(&mut self, node: T) -> Node<T> {
        Node(node, self.end_node_id())
    }

    pub fn parse(mut self) -> ast::Program<'a> {
        let program = self.parse_impl();
        while let Some(tok) = self.next_tok() {
            self.info
                .report_error(tok.error(self.info, format!("unexpected token {:?}", tok.0)))
        }
        program
    }

    fn next_tok(&mut self) -> Option<Node<Token<'a>>> {
        if let Some(peek) = self.peek.take() {
            self.last_span = self.curr_span;
            self.curr_span = peek.as_ref().map_or(self.source.eof(), |v| {
                v.1.map(|v| self.info.get_node_source(v).0)
                    .unwrap_or(self.source.eof())
            });
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
        let mut vec = Vec::new();
        loop {
            self.start_node();
            consume_if!(self,
                Some(Node(Token::Fn, node)) => {
                    let func = self.parse_function();
                    vec.push(self.end_node(ast::TopLevel::FunctionDef(func)));
                }
                @consume Some(tok) => {
                    self.end_node_id();
                    self.info.report_error(tok.error(self.info, format!("expected 'fn' 'static' 'const' 'struct' 'enum' 'union' found {:?}", tok.0)));
                }
                None => {
                    self.end_node_id();
                    break
                }
            )
        }
        ast::Program(vec)
    }

    fn parse_path(&mut self) -> Node<ast::Path<'a>> {
        self.start_node();
        let ident = expect_tok!(self, Token::Ident(ident), _ => ident, "expected path found {:?}")
            .unwrap_or(EMPTY_IDENT);
        self.end_node(ast::Path { ident })
    }

    fn parse_ident(&mut self) -> ast::Ident<'a> {
        ast::Ident {
            name: self.parse_path(),
            resolve: None,
        }
    }

    fn parse_type(&mut self) -> Node<ast::Type<'a>> {
        self.start_node();
        consume_if!(self,
            @consume Some(tok!(Token::Ident("void"))) => self.end_node(ast::Type::Void),
            @consume Some(tok!(Token::Ident("i8"))) => self.end_node(ast::Type::I8),
            @consume Some(tok!(Token::Ident("i16"))) => self.end_node(ast::Type::I16),
            @consume Some(tok!(Token::Ident("i32"))) => self.end_node(ast::Type::I32),
            @consume Some(tok!(Token::Ident("i64"))) => self.end_node(ast::Type::I64),
            @consume Some(tok!(Token::Ident("u8"))) => self.end_node(ast::Type::U8),
            @consume Some(tok!(Token::Ident("u16"))) => self.end_node(ast::Type::U16),
            @consume Some(tok!(Token::Ident("u32"))) => self.end_node(ast::Type::U32),
            @consume Some(tok!(Token::Ident("u64"))) => self.end_node(ast::Type::U64),
            @consume Some(tok!(Token::Ident("bool"))) => self.end_node(ast::Type::Bool),
            @consume Some(tok!(Token::Ident("char"))) => self.end_node(ast::Type::Char),
            @consume Some(tok!(Token::LBracket)) => {
                let ty = self.parse_type();
                expect_tok_after!(self, Token::Colon, _, "expected ':' found {:?}");
                let len = expect_tok_after!(self, Token::NumericLiteral(lit), _ => lit, "expected numeric literal found {:?}");
                expect_tok_after!(self, Token::RBracket, _, "expected ']' found {:?}");
                self.end_node(ast::Type::ConstArr(Box::new(ty), 0))
            },
            @consume Some(tok!(Token::Fn)) => {
                let args = {
                    let mut vec = Vec::new();
                    self.start_node();
                    expect_tok_after!(self, Token::LPar, _, "expected '(' found {:?}");
                    while !is_tok!(self, Token::RPar){
                        vec.push(self.parse_type());
                        consume_if!(self,
                            @consume Some(tok!(Token::Comma)) => {
                                expect_tok_after!(self, Token::LPar, _, "expected ')' found {:?}");
                                break;
                            },
                            _ => {}
                        )
                    }
                    expect_tok_after!(self, Token::RPar, _, "expected ')' found {:?}");
                    self.end_node(vec)
                };
                let ret = consume_if!(self,
                    @consume Some(tok!(Token::SmallRightArrow)) => Some(Box::new(self.parse_type()))
                    _ => None
                );
                self.end_node(ast::Type::FnPtr{ ret, args })
            },
            @consume Some(tok!(Token::Star)) => {
                let mutability = consume_if!(self,
                    @consume Some(tok!(Token::Mut)) => ast::Mutability::Mut,
                    @consume Some(tok!(Token::Const)) => ast::Mutability::Const,

                    @consume Some(tok) => {
                        self.info.report_error(tok.error(self.info, format!("expected 'mut' or 'const' but found {:?}", tok.0)));
                        ast::Mutability::Const
                    },
                    None => {
                        self.info.report_error(ErrorNode::span(self.source.eof(), self.source, format!("expected 'mut' or 'const' but found {:?}", "EOF")));
                        ast::Mutability::Const
                    }
                );

                let ty = self.parse_type();
                self.end_node(ast::Type::Ptr(mutability, Box::new(ty)))
            },
            _ => {
                let path = self.parse_path().0;
                self.end_node(ast::Type::User(path))
            }
        )
    }

    fn parse_function(&mut self) -> ast::FunctionDef<'a> {
        expect_tok!(self, Token::Fn, _, "expected 'fn' found {:?}");
        let ident = self.parse_path();
        let args = {
            let mut vec = Vec::new();
            self.start_node();
            expect_tok_after!(self, Token::LPar, _, "expected '(' found {:?}");
            while !is_tok!(self, Token::RPar) {
                self.start_node();
                let name = self.parse_ident();
                expect_tok_after!(self, Token::Colon, _, "expected ':' found {:?}");
                let ty = self.parse_type();

                vec.push(self.end_node(ast::Param { name, ty }));
                consume_if!(self,
                    @consume Some(tok!(Token::Comma)) => {
                        expect_tok_after!(self, Token::LPar, _, "expected ')' found {:?}");
                        break;
                    },
                    _ => {}
                )
            }
            expect_tok_after!(self, Token::RPar, _, "expected ')' found {:?}");
            self.end_node(vec)
        };
        let ret = consume_if!(self,
            @consume Some(tok!(Token::SmallRightArrow)) => Some(self.parse_type())
            _ => None
        );

        self.start_node();
        expect_tok_after!(self, Token::LBrace, _, "expected '{{' found {:?}");

        let mut body = Vec::new();
        while !is_tok!(self, Token::RBrace) && self.peek_next_tok().is_some() {
            body.push(self.parse_block_item());
        }
        expect_tok_after!(self, Token::RBrace, _, "expected '}}' found {:?}");
        ast::FunctionDef {
            name: ident,
            ret,
            args,
            body: self.end_node(ast::Block { body }),
        }
    }

    fn parse_block_item(&mut self) -> Node<ast::BlockItem<'a>> {
        self.start_node();
        consume_if!(self,
            @consume Some(tok!(Token::Let)) => {
                let decl = self.parse_declaration();
                self.end_node(ast::BlockItem::Declaration(decl))
            },
            _ => {
                let smt = self.parse_statement();
                self.end_node(ast::BlockItem::Statement(smt))
            }
        )
    }

    fn parse_declaration(&mut self) -> ast::Declaration<'a> {
        let name = self.parse_ident();
        self.parse_colon();
        let ty = self.parse_type();
        let expr = consume_if!(self,
            @consume Some(tok!(Token::Assignment)) => Some(self.parse_expression()),
            _ => None
        );
        self.parse_semicolon();
        ast::Declaration { name, ty, expr }
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

    fn parse_label_usage(&mut self) -> Option<Node<&'a str>> {
        self.start_node();
        consume_if!(self,
            @consume Some(tok!(Token::Dot)) => {
                let name = expect_tok_after!(self, Token::Ident(name), _ => name, "expected identifier found {:?}").unwrap_or(EMPTY_IDENT);
                Some(self.end_node(name))
            }
            _ => {
                self.node_stack.pop();
                None
            }
        )
    }

    fn parse_statement(&mut self) -> ast::Statement<'a> {
        let stmt = consume_if!(self,
            @consume Some(tok!(Token::Return)) => ast::Statement::Return(self.parse_expression()),
            @consume Some(tok!(Token::Break)) => {
                let label = self.parse_label_usage();
                let expr = if is_tok!(self, Token::Semicolon){
                    None
                }else{
                    Some(self.parse_expression())
                };

                ast::Statement::Break{
                    label,
                    expr
                }
            }
            @consume Some(tok!(Token::Continue)) => {
                let label = self.parse_label_usage();
                ast::Statement::Continue{
                    label
                }
            }
            Some(tok!(Token::Semicolon)) => ast::Statement::Empty,
            Some(tok) => ast::Statement::Expression(self.parse_expression()),
            None => {
                self.info.report_error(ErrorNode::span(self.source.eof(), self.source, "expected statement but is at EOF".into()));
                return ast::Statement::Empty;
            }
        );
        if !matches!(
            stmt,
            ast::Statement::Expression(Node(ast::Expr::If { .. }, _))
                | ast::Statement::Expression(Node(ast::Expr::While { .. }, _))
                | ast::Statement::Expression(Node(ast::Expr::Block { .. }, _))
        ) {
            self.parse_semicolon();
        }
        stmt
    }

    fn parse_expression(&mut self) -> Node<ast::Expr<'a>> {
        self.parse_expression_13(0)
    }

    fn parse_expression_13(&mut self, min_prec: usize) -> Node<ast::Expr<'a>> {
        self.start_node();
        let mut lhs = self.parse_expression_14();
        loop {
            let op = consume_if!(self,
                @consume Some(tok!(Token::Plus, node)) if ast::BinaryOp::Addition.precedence() >= min_prec => Node(ast::BinaryOp::Addition, node),
                @consume Some(tok!(Token::Minus, node)) if ast::BinaryOp::Subtract.precedence() >= min_prec => Node(ast::BinaryOp::Subtract, node),

                @consume Some(tok!(Token::Star, node)) if ast::BinaryOp::Multiply.precedence() >= min_prec => Node(ast::BinaryOp::Multiply, node),
                @consume Some(tok!(Token::Slash, node)) if ast::BinaryOp::Divide.precedence() >= min_prec => Node(ast::BinaryOp::Divide, node),
                @consume Some(tok!(Token::Percent, node)) if ast::BinaryOp::Remainder.precedence() >= min_prec => Node(ast::BinaryOp::Remainder, node),

                @consume Some(tok!(Token::ShiftLeft, node)) if ast::BinaryOp::ShiftLeft.precedence() >= min_prec => Node(ast::BinaryOp::ShiftLeft, node),
                @consume Some(tok!(Token::ShiftRight, node)) if ast::BinaryOp::ShiftRight.precedence() >= min_prec => Node(ast::BinaryOp::ShiftRight, node),

                @consume Some(tok!(Token::Equals, node)) if ast::BinaryOp::Eq.precedence() >= min_prec => Node(ast::BinaryOp::Eq, node),
                @consume Some(tok!(Token::NotEquals, node)) if ast::BinaryOp::Ne.precedence() >= min_prec => Node(ast::BinaryOp::Ne, node),

                @consume Some(tok!(Token::LessThan, node)) if ast::BinaryOp::Lt.precedence() >= min_prec => Node(ast::BinaryOp::Lt, node),
                @consume Some(tok!(Token::LessThanEq, node)) if ast::BinaryOp::Lte.precedence() >= min_prec => Node(ast::BinaryOp::Lte, node),
                @consume Some(tok!(Token::GreaterThan, node)) if ast::BinaryOp::Gt.precedence() >= min_prec => Node(ast::BinaryOp::Gt, node),
                @consume Some(tok!(Token::GreaterThanEq, node)) if ast::BinaryOp::Gte.precedence() >= min_prec => Node(ast::BinaryOp::Gte, node),

                @consume Some(tok!(Token::Ampersand, node)) if ast::BinaryOp::BitAnd.precedence() >= min_prec => Node(ast::BinaryOp::BitAnd, node),
                @consume Some(tok!(Token::BitwiseXor, node)) if ast::BinaryOp::BitXor.precedence() >= min_prec => Node(ast::BinaryOp::BitXor, node),
                @consume Some(tok!(Token::BitwiseOr, node)) if ast::BinaryOp::BitOr.precedence() >= min_prec => Node(ast::BinaryOp::BitOr, node),
                @consume Some(tok!(Token::LogicalAnd, node)) if ast::BinaryOp::LogAnd.precedence() >= min_prec => Node(ast::BinaryOp::LogAnd, node),
                @consume Some(tok!(Token::LogicalOr, node)) if ast::BinaryOp::LogOr.precedence() >= min_prec => Node(ast::BinaryOp::LogOr, node),

                @consume Some(tok!(Token::Assignment, node)) if ast::BinaryOp::Assignment.precedence() >= min_prec => Node(ast::BinaryOp::Assignment, node),
                @consume Some(tok!(Token::PlusEq, node)) if ast::BinaryOp::PlusEq.precedence() >= min_prec => Node(ast::BinaryOp::PlusEq, node),
                @consume Some(tok!(Token::MinusEq, node)) if ast::BinaryOp::MinusEq.precedence() >= min_prec => Node(ast::BinaryOp::MinusEq, node),
                @consume Some(tok!(Token::TimesEq, node)) if ast::BinaryOp::TimesEq.precedence() >= min_prec => Node(ast::BinaryOp::TimesEq, node),
                @consume Some(tok!(Token::DivideEq, node)) if ast::BinaryOp::DivideEq.precedence() >= min_prec => Node(ast::BinaryOp::DivideEq, node),
                @consume Some(tok!(Token::ModuloEq, node)) if ast::BinaryOp::ModuloEq.precedence() >= min_prec => Node(ast::BinaryOp::ModuloEq, node),
                @consume Some(tok!(Token::ShiftLeftEq, node)) if ast::BinaryOp::ShiftLeftEq.precedence() >= min_prec => Node(ast::BinaryOp::ShiftLeftEq, node),
                @consume Some(tok!(Token::ShiftRightEq, node)) if ast::BinaryOp::ShiftRightEq.precedence() >= min_prec => Node(ast::BinaryOp::ShiftRightEq, node),
                @consume Some(tok!(Token::AndEq, node)) if ast::BinaryOp::AndEq.precedence() >= min_prec => Node(ast::BinaryOp::AndEq, node),
                @consume Some(tok!(Token::OrEq, node)) if ast::BinaryOp::OrEq.precedence() >= min_prec => Node(ast::BinaryOp::OrEq, node),
                @consume Some(tok!(Token::XorEq, node)) if ast::BinaryOp::XorEq.precedence() >= min_prec => Node(ast::BinaryOp::XorEq, node),
                _ => break
            );
            if op.0.left_to_right() {
                let rhs = self.parse_expression_13(op.0.precedence() + 1);
                lhs = self.end_node(ast::Expr::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            } else {
                let rhs = self.parse_expression_13(op.0.precedence());

                lhs = self.end_node(ast::Expr::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            }
            self.start_node();
        }
        self.node_stack.pop();
        lhs
    }

    fn next_span(&mut self) -> Span {
        if let Some(some) = self.next_node_id() {
            self.info.get_node_source(some).0
        } else {
            self.source.eof()
        }
    }

    fn next_node_id(&mut self) -> Option<info::NodeId> {
        self.peek_next_tok()?.1
    }

    fn create_node<T>(&mut self, span: Span, node: T) -> Node<T> {
        let id = self.info.create_node_id(self.source, span);
        Node(node, id)
    }

    fn parse_expression_14(&mut self) -> Node<ast::Expr<'a>> {
        self.start_node();
        if let Some(op) = consume_if!(self,
            @consume Some(tok!(Token::Minus, node)) => Some(Node(ast::UnaryOp::Neg, node)),
            @consume Some(tok!(Token::BitwiseNot, node)) => Some(Node(ast::UnaryOp::BitNot, node)),
            @consume Some(tok!(Token::LogicalNot, node)) => Some(Node(ast::UnaryOp::LogNot, node)),

            @consume Some(tok!(Token::Inc, node)) => Some(Node(ast::UnaryOp::PreInc, node)),
            @consume Some(tok!(Token::Dec, node)) => Some(Node(ast::UnaryOp::PreDec, node)),

            @consume Some(tok!(Token::Star, node)) => Some(Node(ast::UnaryOp::Dereference, node)),
            @consume Some(tok!(Token::Ampersand, node)) => Some(Node(ast::UnaryOp::Reference, node)),
            _ => None
        ) {
            let expr = self.parse_expression_14();
            return self.end_node(ast::Expr::Unary(op, Box::new(expr)));
        } else {
            self.node_stack.pop();
        }

        let mut start = self.next_span();
        let mut expr = self.parse_expression_15();
        loop {
            consume_if!(self,
                @consume Some(tok!(Token::Inc, node)) =>
                    expr =self.create_node(
                        start.combine(self.curr_span),
                        ast::Expr::Unary(Node(ast::UnaryOp::PostInc, node), Box::new(expr))
                    ),
                @consume Some(tok!(Token::Dec, node)) =>
                    expr = self.create_node(
                        start.combine(self.curr_span),
                        ast::Expr::Unary(Node(ast::UnaryOp::PostDec, node), Box::new(expr))
                    ),
                _ => return expr
            );
            start = self.next_span();
        }
    }

    fn parse_block(&mut self) -> Node<ast::Block<'a>> {
        self.start_node();
        expect_tok_after!(self, Token::LBrace, _, "expected '{{' found {:?}");

        let mut body = Vec::new();
        while !is_tok!(self, Token::RBrace) && self.peek_next_tok().is_some() {
            body.push(self.parse_block_item());
        }
        expect_tok_after!(self, Token::RBrace, _, "expected '}}' found {:?}");
        self.end_node(ast::Block { body })
    }

    fn parse_block_expr(&mut self) -> Node<ast::Expr<'a>> {
        self.start_node();
        let label = consume_if!(self,
            @consume Some(tok!(Token::Dot)) => {
                let name = expect_tok_after!(self, Token::Ident(name), _ => name, "expected identifier found {:?}").unwrap_or(EMPTY_IDENT);
                expect_tok_after!(self, Token::Colon, _, "expected ':' found {:?}");
                Some(self.end_node(name))
            }
            _ => {
                self.node_stack.pop();
                None
            }
        );
        consume_if!(self,
            Some(tok!(Token::If)) => {
                todo!()
            }
            @consume Some(tok!(Token::Loop)) => {
                self.start_node();
                let body = self.parse_block();
                self.end_node(ast::Expr::While {
                    cond: Box::new(ast::LoopCond::Infinite),
                    body,
                    label
                })
            }
            @consume Some(tok!(Token::While)) => {
                self.start_node();
                let cond = self.parse_expression();
                let body = self.parse_block();
                self.end_node(ast::Expr::While {
                    cond: Box::new(ast::LoopCond::While(cond)),
                    body,
                    label
                })
            }
            Some(tok!(Token::LBrace)) => {
                self.start_node();
                let inner = self.parse_block();
                self.end_node(ast::Expr::Block { label, inner })
            }
            @consume Some(tok) => {
                self.info.report_error(tok.error(self.info, format!("expected block found token {:?}", tok.0)));
                Node(EMPTY_EXPR, None)
            }
            None => {
                self.info.report_error(ErrorNode::span(self.curr_span.immediately_after(), self.source, "expected block found EOF".into()));
                Node(EMPTY_EXPR, None)
            }
        )
    }

    fn parse_expression_15(&mut self) -> Node<ast::Expr<'a>> {
        consume_if!(self,
            @consume Some(tok!(Token::NumericLiteral(num), node)) => Node(ast::Expr::Constant(ast::Literal::Number(num)), node),
            @consume Some(tok!(Token::FalseLiteral, node)) => Node(ast::Expr::Constant(ast::Literal::Bool(false)), node),
            @consume Some(tok!(Token::TrueLiteral, node)) => Node(ast::Expr::Constant(ast::Literal::Bool(true)), node),
            @consume Some(tok!(Token::CharLiteral(char), node)) => Node(ast::Expr::Constant(ast::Literal::Char(char)), node),
            Some(tok!(Token::Ident(_))) => {
                self.start_node();
                let ident = self.parse_ident();
                self.end_node(ast::Expr::Ident(ident))
            },
            Some(tok!(Token::Dot | Token::If | Token::While | Token::LBrace | Token::Loop)) => {
                self.parse_block_expr()
            }

            @consume Some(tok!(Token::LPar)) => {
                let expr = self.parse_expression();
                expect_tok!(self, Token::RPar, _, "expected ')' found {:?}");
                expr
            },

            @consume Some(tok) => {
                self.info.report_error(tok.error(self.info, format!("unexpected token {:?}", tok.0)));
                Node(EMPTY_EXPR, None)
            }
            None => {
                self.info.report_error(ErrorNode::span(self.curr_span.immediately_after(), self.source, "unexpected EOF".into()));
                Node(EMPTY_EXPR, None)
            }
        )
    }
}
