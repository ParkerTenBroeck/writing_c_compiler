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
    Mov { from: Operand, to: Operand },
    Ret,
}

#[derive(Debug)]
pub enum Operand {
    Imm(i32),
    Reg(Register),
}

#[derive(Debug)]
pub enum Register {
    Eax,
}
