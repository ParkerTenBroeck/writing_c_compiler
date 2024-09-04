pub mod ast;

use crate::tacky;

pub fn asm_gen(
    out: impl std::fmt::Write,
    ast: crate::tacky::ast::Program<'_>,
) -> std::fmt::Result {
    let ast = gen::AsmGen::new().gen(ast);
    emmision::AsmEmission::new(out).emit_asm(ast)
}

pub mod gen {
    use crate::{code_gen, tacky};

    pub struct AsmGen{

    }

    impl AsmGen{
        pub fn new() -> Self{
            Self{}
        }

        pub fn gen<'a>(&mut self, ast: tacky::ast::Program<'a>) -> code_gen::ast::Program<'a> {
            match ast {
                tacky::ast::Program::FunctionDef(func) => code_gen::ast::Program::FunctionDef(self.gen_function_def(func)),
            }
        }
    
        fn gen_function_def<'a>(&mut self, ast: tacky::ast::FunctionDef<'a>) -> code_gen::ast::FunctionDef<'a> {
            let mut ins = Vec::new();
            ins.push(code_gen::ast::Instruction::AllocStack(ast.temps * 4));
            self.gen_instructions(&mut ins, ast.instructions);
            ins.push(code_gen::ast::Instruction::Ret);
            code_gen::ast::FunctionDef {
                name: ast.name,
                instructions: ins,
            }
        }
    
        fn gen_instructions(&mut self, ins: &mut Vec<code_gen::ast::Instruction>, ins_in: Vec<tacky::ast::Instruction>) {
            for tacky in ins_in{
                self.gen_instruction(ins, tacky)
            }
        }

        fn gen_instruction(&mut self, ins: &mut Vec<code_gen::ast::Instruction>, ins_in: tacky::ast::Instruction) {
            match ins_in{
                tacky::ast::Instruction::Return(val) => {
                    ins.push(code_gen::ast::Instruction::Mov { src: self.convert_val(val), dest: code_gen::ast::Operand::Reg(code_gen::ast::Register::Eax) });
                    ins.push(code_gen::ast::Instruction::Ret);
                },
                tacky::ast::Instruction::Unary { op, dest, src } => {
                    let src = self.convert_val(src);
                    let dest = self.convert_val(dest);
                    ins.push(code_gen::ast::Instruction::UnaryOp { op: match op{
                        tacky::ast::UnaryOp::Minus => code_gen::ast::UnaryOp::Neg,
                        tacky::ast::UnaryOp::Not => code_gen::ast::UnaryOp::Not,
                        tacky::ast::UnaryOp::PreInc => todo!(),
                        tacky::ast::UnaryOp::PreDec => todo!(),
                        tacky::ast::UnaryOp::PostInc => todo!(),
                        tacky::ast::UnaryOp::PostDec => todo!(),
                    }, src, dest });
                },
                tacky::ast::Instruction::Binary { op, lhs, rhs, dest } => {
                    let lhs = self.convert_val(lhs);
                    let rhs = self.convert_val(rhs);
                    let dest = self.convert_val(dest);
                    let op = match op{
                        tacky::ast::BinaryOp::Addition => code_gen::ast::BinaryOp::Addition,
                        tacky::ast::BinaryOp::Subtract => code_gen::ast::BinaryOp::Subtract,
                        tacky::ast::BinaryOp::Multiply => code_gen::ast::BinaryOp::Multiply,
                        tacky::ast::BinaryOp::Divide => code_gen::ast::BinaryOp::Divide,
                        tacky::ast::BinaryOp::Remainder => code_gen::ast::BinaryOp::Remainder,
                    };

                    ins.push(code_gen::ast::Instruction::BinaryOp { op, lhs, rhs, dest });
                },
            }
        }

        fn convert_val(&mut self, val: tacky::ast::Val) -> code_gen::ast::Operand{
            match val{
                tacky::ast::Val::Const(val) => code_gen::ast::Operand::Imm(val),
                tacky::ast::Val::Var(var) => code_gen::ast::Operand::Pseudo(var),
            }
        }
    }
}

pub mod emmision {
    use crate::code_gen;
    pub struct AsmEmission<W: std::fmt::Write>{
        out: W,
    }

    impl<W: std::fmt::Write> AsmEmission<W>{
        pub fn new(out: W) -> Self{
            Self{out}
        }
        pub fn emit_asm(
            &mut self,
            ast: code_gen::ast::Program<'_>,
        ) -> std::fmt::Result {

            match ast{
                code_gen::ast::Program::FunctionDef(func) => self.emit_function(func)?,
            }

            self.out.write_str(r#".section .note.GNU-stack,"",@progbits"#)?;
            self.out.write_str("\n")?;
            Ok(())
        }
        fn emit_function(
            &mut self,
            func: code_gen::ast::FunctionDef
        ) -> std::fmt::Result{
            writeln!(self.out, ".global {:}\
                            \n{:}:\
                            \n\tpushq %rbp\
                            \n\tmovq %rsp, %rbp\n", func.name, func.name)?;

            for inst in func.instructions {
                self.emit_instruction(inst)?;
            }
            self.out.write_str("\n")?;


            Ok(())
        }
        fn emit_instruction(
            &mut self,
            inst: code_gen::ast::Instruction,
        ) -> std::fmt::Result {
            match inst {
                code_gen::ast::Instruction::Mov { src: from, dest: to } => {
                    if from.is_mem() && to.is_mem(){
                        self.out.write_str("\tmovl ")?;
                        self.emit_operand( from)?;
                        self.out.write_str(", ")?;
                        self.emit_operand( code_gen::ast::Operand::Reg(code_gen::ast::Register::R10))?;
                        self.out.write_str("\n")?;

                        self.out.write_str("\tmovl ")?;
                        self.emit_operand(code_gen::ast::Operand::Reg(code_gen::ast::Register::R10))?;
                        self.out.write_str(", ")?;
                        self.emit_operand(to)?;
                        self.out.write_str("\n")?;
                    }else{
                        self.out.write_str("\tmovl ")?;
                        self.emit_operand(from)?;
                        self.out.write_str(", ")?;
                        self.emit_operand(to)?;
                        self.out.write_str("\n")?;
                    }
                }
                code_gen::ast::Instruction::Ret => self.out.write_str("\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret\n")?,
                code_gen::ast::Instruction::AllocStack(stack) => writeln!(self.out, "\tsubq ${}, %rsp", stack)?,
                code_gen::ast::Instruction::UnaryOp { op, src, dest } => {
                    self.emit_instruction(code_gen::ast::Instruction::Mov { src, dest })?;

                    match op{
                        code_gen::ast::UnaryOp::Neg => self.out.write_str("\tnegl ")?,
                        code_gen::ast::UnaryOp::Not => self.out.write_str("\tnotl ")?,
                    } 
                    self.emit_operand(dest)?;
                    self.out.write_str("\n")?;
                },
                code_gen::ast::Instruction::BinaryOp { op, lhs, rhs, dest } => {

                },
            }
            Ok(())
        }
        fn emit_operand(
            &mut self,
            op: code_gen::ast::Operand,
        ) -> std::fmt::Result {
            match op {
                code_gen::ast::Operand::Imm(imm) => write!(self.out, "${imm}"),
                code_gen::ast::Operand::Reg(reg) => match reg {
                    code_gen::ast::Register::Eax => self.out.write_str("%eax"),
                    code_gen::ast::Register::Rax => self.out.write_str("%rax"),
                    code_gen::ast::Register::R10 => self.out.write_str("%r10d"),
                },
                code_gen::ast::Operand::Pseudo(val) => write!(self.out, "-{}(%rbp)", (val+1)*4),
                code_gen::ast::Operand::Stack(val) =>  write!(self.out, "-{}(%rbp)", (val+1)*4),
            }
        }
    }
}
