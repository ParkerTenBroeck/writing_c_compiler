use crate::lex::Number;

#[derive(Debug)]
pub struct Program<'a> (pub Vec<TopLevel<'a>>);

#[derive(Debug)]
pub enum TopLevel<'a>{
    FunctionDef(FunctionDef<'a>),
}

#[derive(Debug)]
pub struct FunctionDef<'a> {
    pub name: &'a str,
    pub body: Statement<'a>,
}

#[derive(Debug)]
pub enum Statement<'a> {
    Return(Expr<'a>),
}

#[derive(Debug)]
pub enum Expr<'a> {
    Constant(Literal<'a>),
    Unary(UnaryOp, Box<Expr<'a>>),
    Binary{
        op: BinaryOp,
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>,
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,

    PreInc,
    PreDec,
    PostInc,
    PostDec
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
}

impl BinaryOp{
    pub fn precedence(&self) -> usize{
        match self{
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
