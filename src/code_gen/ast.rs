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
    UnaryOp{ op: UnaryOp, in_place: Operand},
    AllocStack(usize),
    Ret,
}

#[derive(Debug, Clone, Copy)]
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
}

#[derive(Debug)]
pub enum UnaryOp{
    Neg,
    Not
}

#[derive(Debug, Clone, Copy)]
pub enum Register {
    Eax,
    Rax,
    R10, 
}
