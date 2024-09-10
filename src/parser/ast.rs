use crate::{
    lex::Number,
    util::info::{Node, VarId},
};

#[derive(Debug)]
pub struct Program<'a>(pub Vec<Node<TopLevel<'a>>>);

#[derive(Debug)]
pub enum TopLevel<'a> {
    FunctionDef(FunctionDef<'a>),
}

#[derive(Debug)]
pub struct Path<'a> {
    pub ident: &'a str,
}

#[derive(Debug)]
pub struct FunctionDef<'a> {
    pub name: Node<Path<'a>>,
    pub body: Node<Vec<Node<BlockItem<'a>>>>,
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    Statement(Statement<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug)]
pub struct Ident<'a> {
    pub name: Node<Path<'a>>,
    pub resolve: Option<VarId>,
}

impl<'a> Ident<'a> {
    pub fn new(name: &'a str) -> Self {
        Self {
            name: Node(Path { ident: name }, None),
            resolve: None,
        }
    }
}

#[derive(Debug)]
pub struct Declaration<'a> {
    pub name: Node<Path<'a>>,
    pub expr: Option<Node<Expr<'a>>>,
}

#[derive(Debug)]
pub enum Statement<'a> {
    Return(Node<Expr<'a>>),
    Expression(Node<Expr<'a>>),
    Empty,
}

#[derive(Debug)]
pub enum Expr<'a> {
    Constant(Literal<'a>),
    Unary(Node<UnaryOp>, Box<Node<Expr<'a>>>),
    Binary {
        op: Node<BinaryOp>,
        lhs: Box<Node<Expr<'a>>>,
        rhs: Box<Node<Expr<'a>>>,
    },

    Ident(Ident<'a>),
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    BitNot,
    LogNot,

    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

#[derive(Debug)]
pub enum BinaryOp {
    Addition,
    Subtract,

    Multiply,
    Divide,
    Remainder,

    ShiftLeft,
    ShiftRight,

    Lt,
    Lte,
    Gt,
    Gte,

    Eq,
    Ne,

    BitAnd,
    BitXor,
    BitOr,

    LogAnd,
    LogOr,

    Assignment,
    PlusEq,
    MinusEq,
    TimesEq,
    DivideEq,
    ModuloEq,
    ShiftLeftEq,
    ShiftRightEq,
    AndEq,
    OrEq,
    XorEq,
}

impl BinaryOp {
    pub fn precedence(&self) -> usize {
        match self {
            BinaryOp::Addition => 20 - 4,
            BinaryOp::Subtract => 20 - 4,

            BinaryOp::Multiply => 20 - 3,
            BinaryOp::Divide => 20 - 3,
            BinaryOp::Remainder => 20 - 3,

            BinaryOp::ShiftLeft => 20 - 5,
            BinaryOp::ShiftRight => 20 - 5,

            BinaryOp::Lt => 20 - 6,
            BinaryOp::Lte => 20 - 6,
            BinaryOp::Gt => 20 - 6,
            BinaryOp::Gte => 20 - 6,

            BinaryOp::Eq => 20 - 7,
            BinaryOp::Ne => 20 - 7,

            BinaryOp::BitAnd => 20 - 8,
            BinaryOp::BitXor => 20 - 9,
            BinaryOp::BitOr => 20 - 10,
            BinaryOp::LogAnd => 20 - 11,
            BinaryOp::LogOr => 20 - 12,

            BinaryOp::Assignment => 20 - 14,
            BinaryOp::PlusEq => 20 - 14,
            BinaryOp::MinusEq => 20 - 14,
            BinaryOp::TimesEq => 20 - 14,
            BinaryOp::DivideEq => 20 - 14,
            BinaryOp::ModuloEq => 20 - 14,
            BinaryOp::ShiftLeftEq => 20 - 14,
            BinaryOp::ShiftRightEq => 20 - 14,
            BinaryOp::AndEq => 20 - 14,
            BinaryOp::OrEq => 20 - 14,
            BinaryOp::XorEq => 20 - 14,
        }
    }

    pub fn left_to_right(&self) -> bool {
        match self {
            BinaryOp::Addition => true,
            BinaryOp::Subtract => true,

            BinaryOp::Multiply => true,
            BinaryOp::Divide => true,
            BinaryOp::Remainder => true,

            BinaryOp::ShiftLeft => true,
            BinaryOp::ShiftRight => true,

            BinaryOp::Lt => true,
            BinaryOp::Lte => true,
            BinaryOp::Gt => true,
            BinaryOp::Gte => true,

            BinaryOp::Eq => true,
            BinaryOp::Ne => true,

            BinaryOp::BitAnd => true,
            BinaryOp::BitXor => true,
            BinaryOp::BitOr => true,
            BinaryOp::LogAnd => true,
            BinaryOp::LogOr => true,

            BinaryOp::Assignment => false,
            BinaryOp::PlusEq => false,
            BinaryOp::MinusEq => false,
            BinaryOp::TimesEq => false,
            BinaryOp::DivideEq => false,
            BinaryOp::ModuloEq => false,
            BinaryOp::ShiftLeftEq => false,
            BinaryOp::ShiftRightEq => false,
            BinaryOp::AndEq => false,
            BinaryOp::OrEq => false,
            BinaryOp::XorEq => false,
        }
    }
}

#[derive(Debug)]
pub enum Literal<'a> {
    Number(Number<'a>),
    String(&'a str),
    Char(char),
    Bool(bool),
}
