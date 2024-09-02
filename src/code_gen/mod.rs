pub mod ast;

pub fn asm_gen(
    out: impl std::fmt::Write,
    ast: crate::parser::ast::TopLevel<'_>,
) -> std::fmt::Result {
    let ast = gen::gen(ast);
    emmision::AsmEmission::new().emit_asm(out, ast)
}

pub mod gen {
    use crate::{code_gen, parser};

    pub fn gen(ast: parser::ast::TopLevel) -> code_gen::ast::FunctionDef {
        match ast {
            parser::ast::TopLevel::FunctionDef(func) => gen_function_def(func),
        }
    }

    fn gen_function_def(ast: parser::ast::FunctionDef) -> code_gen::ast::FunctionDef {
        let mut ins = Vec::new();
        gen_statement(&mut ins, ast.body);
        ins.push(code_gen::ast::Instruction::Ret);
        code_gen::ast::FunctionDef {
            name: ast.ident,
            instructions: ins,
        }
    }

    fn gen_statement(ins: &mut Vec<code_gen::ast::Instruction>, body: parser::ast::Statement<'_>) {
        match body {
            parser::ast::Statement::Return(expr) => {
                gen_expr(ins, expr);
                ins.push(code_gen::ast::Instruction::Ret);
            }
        }
    }

    fn gen_expr(ins: &mut Vec<code_gen::ast::Instruction>, expr: parser::ast::Expr<'_>) {
        match expr {
            parser::ast::Expr::Constant(val) => {
                let val = match val {
                    parser::ast::Literal::Number(num) => num.get_num().parse::<i32>().unwrap(),
                    parser::ast::Literal::Char(char) => char as i32,
                    parser::ast::Literal::Bool(bool) => {
                        if bool {
                            1
                        } else {
                            0
                        }
                    }
                    parser::ast::Literal::String(_) => todo!(),
                };
                ins.push(code_gen::ast::Instruction::Mov {
                    from: code_gen::ast::Operand::Imm(val),
                    to: code_gen::ast::Operand::Reg(code_gen::ast::Register::Eax),
                })
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
            ast: code_gen::ast::FunctionDef<'_>,
        ) -> std::fmt::Result {
            writeln!(out, ".global {:}\n{:}:", ast.name, ast.name)?;

            for inst in ast.instructions {
                self.emit_instruction(&mut out, inst)?;
            }
            out.write_str("\n")?;

            out.write_str(r#".section .note.GNU-stack,"",@progbits"#)?;
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
                },
            }
        }
    }
}
