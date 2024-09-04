#[derive(Debug)]
pub enum Program<'a> {
    FunctionDef(FunctionDef<'a>),
}

#[derive(Debug)]
pub struct FunctionDef<'a> {
    pub name: &'a str,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Mov { src: Operand, dest: Operand },
    UnaryOp{ op: UnaryOp, src: Operand, dest: Operand},
    AllocStack(usize),
    Ret,
    BinaryOp { op: BinaryOp, lhs: Operand, rhs: Operand, dest: Operand },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    Imm(i32),
    Reg(Register),
    Pseudo(usize),
    Stack(usize)
}

impl Operand{
    pub fn is_mem(&self) -> bool{
        match self{
            Operand::Imm(_) => false,
            Operand::Reg(_) => false,
            Operand::Pseudo(_) => true,
            Operand::Stack(_) => true,
        }
    }
    
    pub fn is_const(&self) -> bool {
        match self{
            Operand::Imm(_) => true,
            Operand::Reg(_) => false,
            Operand::Pseudo(_) => false,
            Operand::Stack(_) => false,
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp{
    Neg,
    Not
}

#[derive(Debug)]
pub enum BinaryOp{
    Addition,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    Eax,
    Rax,
    R10, 
}
