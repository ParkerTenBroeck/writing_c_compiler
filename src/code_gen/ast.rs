use crate::util::info::Label;

#[derive(Debug)]
pub struct Program<'a>(pub Vec<TopLevel<'a>>);

#[derive(Debug)]
pub enum TopLevel<'a> {
    FunctionDef(FunctionDef<'a>),
}

#[derive(Debug)]
pub struct FunctionDef<'a> {
    pub name: &'a str,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Mov {
        src: Operand,
        dest: Operand,
    },
    UnaryOp {
        op: UnaryOp,
        src_dest: Operand,
    },
    AllocStack(usize),
    Ret,
    Cdq,
    Idiv {
        rhs: Operand,
    },
    BinaryOp {
        op: BinaryOp,
        lhs_dest: Operand,
        rhs: Operand,
    },
    Cmp {
        lhs: Operand,
        rhs: Operand,
    },
    Jmp {
        target: Label,
    },
    JmpCC {
        cnd: CmpKind,
        target: Label,
    },
    SetCC {
        cnd: CmpKind,
        dest: Operand,
    },
    LocalLabel(Label),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    Imm(i32),
    Reg(Register),
    Pseudo(usize),
    Stack(usize),
}

impl Operand {
    pub fn is_mem(&self) -> bool {
        match self {
            Operand::Imm(_) => false,
            Operand::Reg(_) => false,
            Operand::Pseudo(_) => false,
            Operand::Stack(_) => true,
        }
    }

    pub fn is_const(&self) -> bool {
        match self {
            Operand::Imm(_) => true,
            Operand::Reg(_) => false,
            Operand::Pseudo(_) => false,
            Operand::Stack(_) => false,
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    BitNot,
    LogNot,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Addition,
    Subtract,
    Multiply,
    BitXor,
    BitOr,
    BitAnd,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    AX,
    BX,
    DX,
    CX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    SP,
    BP,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpKind {
    Eq,
    Ne,
    Gt,
    Gte,
    Lt,
    Lte,
}
