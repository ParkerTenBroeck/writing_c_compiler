use crate::code_gen;
pub struct AsmEmission<W: std::fmt::Write> {
    out: W,
}

impl<W: std::fmt::Write> AsmEmission<W> {
    pub fn new(out: W) -> Self {
        Self { out }
    }
    pub fn emit_asm(&mut self, ast: code_gen::ast::Program<'_>) -> std::fmt::Result {
        for top in ast.0 {
            self.emit_top_level(top)?;
        }

        self.out
            .write_str(r#".section .note.GNU-stack,"",@progbits"#)?;
        self.out.write_str("\n")?;
        Ok(())
    }

    fn emit_top_level(&mut self, top: code_gen::ast::TopLevel<'_>) -> std::fmt::Result {
        match top {
            code_gen::ast::TopLevel::FunctionDef(func) => self.emit_function(func)?,
        }
        Ok(())
    }

    fn emit_function(&mut self, func: code_gen::ast::FunctionDef<'_>) -> std::fmt::Result {
        writeln!(
            self.out,
            ".global {:}\
                        \n{:}:\
                        \n\tpushq %rbp\
                        \n\tmovq %rsp, %rbp\n",
            func.name, func.name
        )?;

        for inst in func.instructions {
            self.emit_instruction(inst)?;
        }
        self.out.write_str("\n")?;

        Ok(())
    }
    fn emit_instruction(&mut self, inst: code_gen::ast::Instruction) -> std::fmt::Result {
        match inst {
            code_gen::ast::Instruction::Mov {
                src: from,
                dest: to,
            } => {
                self.out.write_str("\tmovl ")?;
                self.emit_operand(from, OperandSizeHint::U32)?;
                self.out.write_str(", ")?;
                self.emit_operand(to, OperandSizeHint::U32)?;
                self.out.write_str("\n")?;
            }
            code_gen::ast::Instruction::Ret => self
                .out
                .write_str("\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret\n")?,
            code_gen::ast::Instruction::AllocStack(stack) => {
                writeln!(self.out, "\tsubq ${}, %rsp", stack)?
            }
            code_gen::ast::Instruction::UnaryOp { op, src_dest } => {
                match op {
                    code_gen::ast::UnaryOp::Neg => self.out.write_str("\tnegl ")?,
                    code_gen::ast::UnaryOp::BitNot => self.out.write_str("\tnotl ")?,
                    code_gen::ast::UnaryOp::LogNot => todo!(),
                }
                self.emit_operand(src_dest, OperandSizeHint::U32)?;
                self.out.write_str("\n")?;
            }
            code_gen::ast::Instruction::BinaryOp { op, lhs_dest, rhs } => {
                match op {
                    code_gen::ast::BinaryOp::Addition => self.out.write_str("\taddl ")?,
                    code_gen::ast::BinaryOp::Subtract => self.out.write_str("\tsubl ")?,
                    code_gen::ast::BinaryOp::Multiply => self.out.write_str("\timull ")?,
                    code_gen::ast::BinaryOp::BitAnd => self.out.write_str("\tandl ")?,
                    code_gen::ast::BinaryOp::BitXor => self.out.write_str("\txorl ")?,
                    code_gen::ast::BinaryOp::BitOr => self.out.write_str("\torl ")?,

                    code_gen::ast::BinaryOp::ShiftLeft => self.out.write_str("\tshll ")?,
                    code_gen::ast::BinaryOp::ShiftRight => self.out.write_str("\tshrl ")?,
                }
                self.emit_operand(rhs, OperandSizeHint::U32)?;
                self.out.write_str(", ")?;
                self.emit_operand(lhs_dest, OperandSizeHint::U32)?;
                self.out.write_str("\n")?;
            }

            code_gen::ast::Instruction::Cdq => self.out.write_str("\tcdq\n")?,
            code_gen::ast::Instruction::Idiv { rhs: quote } => {
                self.out.write_str("\tidivl ")?;
                self.emit_operand(quote, OperandSizeHint::U32)?;
                self.out.write_str("\n")?;
            }
            code_gen::ast::Instruction::Cmp { lhs, rhs } => {
                self.out.write_str("\tcmpl ")?;
                self.emit_operand(lhs, OperandSizeHint::U32)?;
                self.out.write_str(", ")?;
                self.emit_operand(rhs, OperandSizeHint::U32)?;
                self.out.write_str("\n")?;
            }
            code_gen::ast::Instruction::Jmp { target } => {
                writeln!(self.out, "\tjmp .L{}", target.0)?
            }
            code_gen::ast::Instruction::JmpCC { cnd, target } => {
                self.out.write_str("\tj")?;
                let code = match cnd {
                    code_gen::ast::CmpKind::Eq => "e",
                    code_gen::ast::CmpKind::Ne => "ne",
                    code_gen::ast::CmpKind::Gt => "l",
                    code_gen::ast::CmpKind::Gte => "le",
                    code_gen::ast::CmpKind::Lt => "g",
                    code_gen::ast::CmpKind::Lte => "ge",
                };
                self.out.write_str(code)?;
                writeln!(self.out, " .L{}", target.0)?;
            }
            code_gen::ast::Instruction::SetCC { cnd, dest } => {
                self.out.write_str("\tset")?;
                let code = match cnd {
                    code_gen::ast::CmpKind::Eq => "e",
                    code_gen::ast::CmpKind::Ne => "ne",
                    code_gen::ast::CmpKind::Gt => "l",
                    code_gen::ast::CmpKind::Gte => "le",
                    code_gen::ast::CmpKind::Lt => "g",
                    code_gen::ast::CmpKind::Lte => "ge",
                };
                self.out.write_str(code)?;
                self.out.write_str(" ")?;
                self.emit_operand(dest, OperandSizeHint::U8)?;
                self.out.write_str("\n")?;
            }
            code_gen::ast::Instruction::LocalLabel(label) => {
                writeln!(self.out, ".L{}:", label.0)?
            }
        }
        Ok(())
    }

    fn emit_operand(
        &mut self,
        op: code_gen::ast::Operand,
        hint: OperandSizeHint,
    ) -> std::fmt::Result {
        use code_gen::ast::Register as R;
        use OperandSizeHint as H;
        match op {
            code_gen::ast::Operand::Imm(imm) => write!(self.out, "${imm}"),
            code_gen::ast::Operand::Reg(reg) => match (reg, hint) {
                (R::AX, H::U8) => self.out.write_str("%al"),
                (R::AX, H::U16) => self.out.write_str("%ax"),
                (R::AX, H::U32) => self.out.write_str("%eax"),

                (R::DX, H::U8) => self.out.write_str("%dl"),
                (R::DX, H::U16) => self.out.write_str("%dx"),
                (R::DX, H::U32) => self.out.write_str("%edx"),

                (R::R10, H::U8) => self.out.write_str("%r10b"),
                (R::R10, H::U32) => self.out.write_str("%r10d"),

                (R::R11, H::U8) => self.out.write_str("%r11b"),
                (R::R11, H::U32) => self.out.write_str("%r11d"),

                _ => todo!("unsupported"),
            },
            code_gen::ast::Operand::Pseudo(val) => {
                panic!("pseudo '{val}' regsiter exists at emission stage!")
            }
            code_gen::ast::Operand::Stack(val) => write!(self.out, "-{}(%rbp)", (val + 1) * 4),
        }
    }
}

#[allow(unused)]
enum OperandSizeHint {
    U8,
    U16,
    U32,
    U64,
}