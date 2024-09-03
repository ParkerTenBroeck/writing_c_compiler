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
}

#[derive(Debug)]
pub enum UnaryOp {
    Minus,
    Not,

    PreInc,
    PreDec,
    PostInc,
    PostDec
}

#[derive(Debug)]
pub enum Literal<'a> {
    Number(Number<'a>),
    String(&'a str),
    Char(char),
    Bool(bool),
}
