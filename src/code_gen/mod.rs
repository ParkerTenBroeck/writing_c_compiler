pub mod ast;

use crate::tacky;

pub fn asm_gen(
    out: impl std::fmt::Write,
    ast: crate::tacky::ast::Program<'_>,
) -> std::fmt::Result {
    let ast = gen::AsmGen::new().gen(ast);
    emmision::AsmEmission::new().emit_asm(out, ast)
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
                    ins.push(code_gen::ast::Instruction::Mov { from: (), to: code_gen::ast::Operand::Reg(code_gen::ast::Register::Eax) });
                    ins.push(code_gen::ast::Instruction::Ret);
                },
                tacky::ast::Instruction::Unary { op, dest, src } => {

                },
            }
        }
    }
}

pub mod emmision {
    use crate::code_gen;
    pub struct AsmEmission {}

    impl AsmEmission {
        pub fn new() -> Self {
            Self {}
        }
        pub fn emit_asm(
            &mut self,
            mut out: impl std::fmt::Write,
            ast: code_gen::ast::Program<'_>,
        ) -> std::fmt::Result {

            match ast{
                code_gen::ast::Program::FunctionDef(func) => self.emit_function(&mut out, func)?,
            }

            out.write_str(r#".section .note.GNU-stack,"",@progbits"#)?;
            out.write_str("\n")?;
            Ok(())
        }
        fn emit_function(
            &mut self,
            mut out: impl std::fmt::Write,
            func: code_gen::ast::FunctionDef
        ) -> std::fmt::Result{
            writeln!(out, ".global {:}\n{:}:", func.name, func.name)?;

            for inst in func.instructions {
                self.emit_instruction(&mut out, inst)?;
            }
            out.write_str("\n")?;


            Ok(())
        }
        fn emit_instruction(
            &mut self,
            mut out: impl std::fmt::Write,
            inst: code_gen::ast::Instruction,
        ) -> std::fmt::Result {
            match inst {
                super::ast::Instruction::Mov { from, to } => {
                    out.write_str("movl ")?;
                    self.emit_operand(&mut out, from)?;
                    out.write_str(", ")?;
                    self.emit_operand(&mut out, to)?;
                    out.write_str("\n")?;
                }
                super::ast::Instruction::Ret => out.write_str("ret\n")?,
            }
            Ok(())
        }
        fn emit_operand(
            &mut self,
            mut out: impl std::fmt::Write,
            op: code_gen::ast::Operand,
        ) -> std::fmt::Result {
            match op {
                code_gen::ast::Operand::Imm(imm) => write!(out, "${imm}"),
                code_gen::ast::Operand::Reg(reg) => match reg {
                    code_gen::ast::Register::Eax => out.write_str("%eax"),
                    code_gen::ast::Register::Ax => todo!(),
                    code_gen::ast::Register::R10 => todo!(),
                },
                code_gen::ast::Operand::Pseudo(_) => todo!(),
                code_gen::ast::Operand::Stack(_) => todo!(),
            }
        }
    }
}
