use crate::lex::Number;

#[derive(Debug)]
pub enum Program<'a> {
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
}

impl BinaryOp{
    pub fn precedence(&self) -> usize{
        match self{
            BinaryOp::Addition => 47,
            BinaryOp::Subtract => 47,
            BinaryOp::Multiply => 51,
            BinaryOp::Divide => 51,
            BinaryOp::Remainder => 51,
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
