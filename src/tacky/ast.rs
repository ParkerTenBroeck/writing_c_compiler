

#[derive(Debug)]
pub struct Program<'a> (pub Vec<TopLevel<'a>>);

#[derive(Debug)]
pub enum TopLevel<'a>{
    FunctionDef(FunctionDef<'a>),
}

#[derive(Debug)]
pub struct FunctionDef<'a> {
    pub name: &'a str,
    pub temps: usize,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Return(Val),
    Unary{
        op: UnaryOp,
        dest: Val,
        src: Val,
    },
    Binary{
        op: BinaryOp,
        lhs: Val,
        rhs: Val,
        dest: Val,
    }
}


#[derive(Debug)]
pub enum BinaryOp {
    Addition,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitXor,
    BitOr,
    BitAnd,
    ShiftRight,
    ShiftLeft,
}

#[derive(Debug)]
pub enum UnaryOp {
    Minus,
    BitNot,

    PreInc,
    PreDec,
    PostInc,
    PostDec
}

#[derive(Debug, Clone, Copy)]
pub enum Val {
    Const(i32),
    Var(usize),
}