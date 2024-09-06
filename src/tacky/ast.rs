

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
    },
    LocalLabel(Label),
    Copy{src: Val, dest: Val},
    Jump{ target: Label },
    JumpIfZero{cond: Val, target: Label},
    JumpIfNotZero{cond: Val, target: Label},
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Label(pub usize);


#[derive(Debug)]
pub enum BinaryOp {
    Addition,
    Subtract,
    
    Multiply,
    Divide,
    Remainder,

    ShiftRight,
    ShiftLeft,
    BitXor,
    BitOr,
    BitAnd,

    Eq,
    Ne,
    Gt,
    Lt,
    Gte,
    Lte,
}

#[derive(Debug)]
pub enum UnaryOp {
    Minus,
    BitNot,
    LogNot,

    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

#[derive(Debug, Clone, Copy)]
pub enum Val {
    Const(i32),
    Var(usize),
}