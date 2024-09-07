pub mod ast;

pub fn code_gen(ast: crate::tacky::ast::Program<'_>) -> self::ast::Program<'_> {
    let mut ast = gen::AsmGen::new().gen(ast);
    register_allocation::RegAllocation::new().run_regalloc(&mut ast);
    fixup::AsmFixup::new().run_fixup(&mut ast);
    ast
}

pub mod register_allocation {
    use crate::code_gen::ast::*;

    #[derive(Default)]
    pub struct RegAllocation {}

    impl RegAllocation {
        pub fn new() -> Self {
            RegAllocation {}
        }

        pub fn run_regalloc(&mut self, prog: &mut Program<'_>) {
            for top in &mut prog.0 {
                self.regalloc_top(top);
            }
        }

        fn regalloc_top(&mut self, top: &mut TopLevel<'_>) {
            match top {
                TopLevel::FunctionDef(func) => self.regalloc_func(func),
            }
        }

        fn regalloc_func(&mut self, func: &mut FunctionDef<'_>) {
            for ins in &mut func.instructions {
                self.regalloc_ins(ins);
            }
        }

        fn regalloc_ins(&mut self, ins: &mut Instruction) {
            match ins {
                Instruction::Mov { src, dest } => {
                    self.regalloc_op(src);
                    self.regalloc_op(dest);
                }
                Instruction::UnaryOp { src_dest, .. } => {
                    self.regalloc_op(src_dest);
                }
                Instruction::BinaryOp { lhs_dest, rhs, .. } => {
                    self.regalloc_op(lhs_dest);
                    self.regalloc_op(rhs);
                }
                Instruction::Idiv { rhs: quote } => self.regalloc_op(quote),
                Instruction::Cmp { lhs, rhs } => {
                    self.regalloc_op(lhs);
                    self.regalloc_op(rhs);
                }
                Instruction::SetCC { dest, .. } => {
                    self.regalloc_op(dest);
                }

                Instruction::AllocStack(_)
                | Instruction::Jmp { .. }
                | Instruction::Cdq
                | Instruction::LocalLabel(_)
                | Instruction::JmpCC { .. }
                | Instruction::Ret => {}
            }
        }

        fn regalloc_op(&mut self, op: &mut Operand) {
            if let Operand::Pseudo(pseudo) = op {
                *op = Operand::Stack(*pseudo);
            }
        }
    }
}

pub mod fixup {
    use crate::code_gen::ast::*;

    #[derive(Default)]
    pub struct AsmFixup {}

    impl AsmFixup {
        pub fn new() -> Self {
            Self {}
        }

        pub fn run_fixup(&mut self, prog: &mut Program<'_>) {
            for top in &mut prog.0 {
                self.fixup_top(top);
            }
        }

        fn fixup_top(&self, top: &mut TopLevel<'_>) {
            match top {
                TopLevel::FunctionDef(func) => self.fixup_func(func),
            }
        }

        fn fixup_func(&self, func: &mut FunctionDef<'_>) {
            let mut new_ins = Vec::new();
            std::mem::swap(&mut new_ins, &mut func.instructions);
            for ins in new_ins {
                self.fixup_ins(ins, &mut func.instructions);
            }
        }

        fn fixup_ins(&self, ins: Instruction, inss: &mut Vec<Instruction>) {
            match ins {
                Instruction::Mov { src, dest } => {
                    if src.is_mem() && dest.is_mem() {
                        inss.push(Instruction::Mov {
                            src,
                            dest: Operand::Reg(Register::R10),
                        });
                        inss.push(Instruction::Mov {
                            src: Operand::Reg(Register::R10),
                            dest,
                        });
                    } else {
                        inss.push(ins);
                    }
                }
                Instruction::BinaryOp { op, lhs_dest, rhs } => {
                    if lhs_dest.is_mem() && op == BinaryOp::Multiply {
                        inss.push(Instruction::Mov {
                            src: lhs_dest,
                            dest: Operand::Reg(Register::R11),
                        });
                        inss.push(Instruction::BinaryOp {
                            op,
                            lhs_dest: Operand::Reg(Register::R11),
                            rhs,
                        });
                        inss.push(Instruction::Mov {
                            src: Operand::Reg(Register::R11),
                            dest: lhs_dest,
                        });
                    } else if lhs_dest.is_mem() && rhs.is_mem() {
                        inss.push(Instruction::Mov {
                            src: rhs,
                            dest: Operand::Reg(Register::R10),
                        });
                        inss.push(Instruction::BinaryOp {
                            op,
                            lhs_dest,
                            rhs: Operand::Reg(Register::R10),
                        });
                    } else {
                        inss.push(Instruction::BinaryOp { op, lhs_dest, rhs });
                    }
                }

                Instruction::Cmp { lhs, rhs } => {
                    if lhs.is_mem() && rhs.is_mem() || rhs.is_const() {
                        inss.push(Instruction::Mov {
                            src: rhs,
                            dest: Operand::Reg(Register::R10),
                        });
                        inss.push(Instruction::Cmp {
                            lhs,
                            rhs: Operand::Reg(Register::R10),
                        });
                    } else {
                        inss.push(Instruction::Cmp { lhs, rhs });
                    }
                }

                Instruction::UnaryOp { .. }
                | Instruction::AllocStack(_)
                | Instruction::Cdq
                | Instruction::Jmp { .. }
                | Instruction::JmpCC { .. }
                | Instruction::SetCC { .. }
                | Instruction::LocalLabel { .. }
                | Instruction::Ret => inss.push(ins),

                Instruction::Idiv { rhs } => {
                    if rhs.is_const() {
                        inss.push(Instruction::Mov {
                            src: rhs,
                            dest: Operand::Reg(Register::R10),
                        });
                        inss.push(Instruction::Idiv {
                            rhs: Operand::Reg(Register::R10),
                        });
                    } else {
                        inss.push(Instruction::Idiv { rhs });
                    }
                }
            }
        }
    }
}

pub mod gen {
    use crate::{code_gen, tacky};

    #[derive(Default)]
    pub struct AsmGen {}

    impl AsmGen {
        pub fn new() -> Self {
            Self {}
        }

        pub fn gen<'a>(&mut self, ast: tacky::ast::Program<'a>) -> code_gen::ast::Program<'a> {
            let mut prog = code_gen::ast::Program(Vec::new());
            for top in ast.0 {
                prog.0.push(self.gen_top_level(top));
            }
            prog
        }

        fn gen_top_level<'a>(
            &mut self,
            top: tacky::ast::TopLevel<'a>,
        ) -> code_gen::ast::TopLevel<'a> {
            match top {
                tacky::ast::TopLevel::FunctionDef(func) => {
                    code_gen::ast::TopLevel::FunctionDef(self.gen_function_def(func))
                }
            }
        }

        fn gen_function_def<'a>(
            &mut self,
            ast: tacky::ast::FunctionDef<'a>,
        ) -> code_gen::ast::FunctionDef<'a> {
            use code_gen::ast::Instruction as I;
            use code_gen::ast::Operand as O;
            use code_gen::ast::Register as R;

            let mut ins = Vec::new();
            ins.push(I::AllocStack(ast.temps * 4));
            self.gen_instructions(&mut ins, ast.instructions);

            if ast.name == "main" {
                ins.push(I::Mov {
                    src: O::Imm(0),
                    dest: O::Reg(R::AX),
                });
            }
            ins.push(I::Ret);

            code_gen::ast::FunctionDef {
                name: ast.name,
                instructions: ins,
            }
        }

        fn gen_instructions(
            &mut self,
            ins: &mut Vec<code_gen::ast::Instruction>,
            ins_in: Vec<tacky::ast::Instruction>,
        ) {
            for tacky in ins_in {
                self.gen_instruction(ins, tacky)
            }
        }

        fn gen_instruction(
            &mut self,
            ins: &mut Vec<code_gen::ast::Instruction>,
            ins_in: tacky::ast::Instruction,
        ) {
            match ins_in {
                tacky::ast::Instruction::Return(val) => {
                    ins.push(code_gen::ast::Instruction::Mov {
                        src: self.convert_val(val),
                        dest: code_gen::ast::Operand::Reg(code_gen::ast::Register::AX),
                    });
                    ins.push(code_gen::ast::Instruction::Ret);
                }
                tacky::ast::Instruction::Unary { op, dest, src } => {
                    let op = match op {
                        tacky::ast::UnaryOp::Minus => code_gen::ast::UnaryOp::Neg,
                        tacky::ast::UnaryOp::BitNot => code_gen::ast::UnaryOp::BitNot,
                        tacky::ast::UnaryOp::LogNot => {
                            let src = self.convert_val(src);
                            let dest = self.convert_val(dest);
                            ins.push(code_gen::ast::Instruction::Cmp {
                                lhs: code_gen::ast::Operand::Imm(0),
                                rhs: src,
                            });
                            ins.push(code_gen::ast::Instruction::Mov {
                                src: code_gen::ast::Operand::Imm(0),
                                dest,
                            });
                            ins.push(code_gen::ast::Instruction::SetCC {
                                cnd: code_gen::ast::CmpKind::Eq,
                                dest,
                            });
                            return;
                        }
                        tacky::ast::UnaryOp::PreInc => todo!(),
                        tacky::ast::UnaryOp::PreDec => todo!(),
                        tacky::ast::UnaryOp::PostInc => todo!(),
                        tacky::ast::UnaryOp::PostDec => todo!(),
                    };
                    let src = self.convert_val(src);
                    let dest = self.convert_val(dest);

                    ins.push(code_gen::ast::Instruction::Mov { src, dest });
                    ins.push(code_gen::ast::Instruction::UnaryOp { op, src_dest: dest });
                }
                tacky::ast::Instruction::Binary { op, lhs, rhs, dest } => {
                    let lhs = self.convert_val(lhs);
                    let rhs = self.convert_val(rhs);
                    let dest = self.convert_val(dest);
                    let op = match op {
                        tacky::ast::BinaryOp::Addition => code_gen::ast::BinaryOp::Addition,
                        tacky::ast::BinaryOp::Subtract => code_gen::ast::BinaryOp::Subtract,
                        tacky::ast::BinaryOp::Multiply => code_gen::ast::BinaryOp::Multiply,
                        tacky::ast::BinaryOp::BitAnd => code_gen::ast::BinaryOp::BitAnd,
                        tacky::ast::BinaryOp::BitOr => code_gen::ast::BinaryOp::BitOr,
                        tacky::ast::BinaryOp::BitXor => code_gen::ast::BinaryOp::BitXor,

                        tacky::ast::BinaryOp::ShiftLeft => code_gen::ast::BinaryOp::ShiftLeft,
                        tacky::ast::BinaryOp::ShiftRight => code_gen::ast::BinaryOp::ShiftRight,
                        tacky::ast::BinaryOp::Divide => {
                            let ax = code_gen::ast::Operand::Reg(code_gen::ast::Register::AX);
                            ins.push(code_gen::ast::Instruction::Mov { src: lhs, dest: ax });
                            ins.push(code_gen::ast::Instruction::Cdq);
                            ins.push(code_gen::ast::Instruction::Idiv { rhs });
                            ins.push(code_gen::ast::Instruction::Mov { src: ax, dest });
                            return;
                        }
                        tacky::ast::BinaryOp::Remainder => {
                            let ax = code_gen::ast::Operand::Reg(code_gen::ast::Register::AX);
                            let dx = code_gen::ast::Operand::Reg(code_gen::ast::Register::DX);
                            ins.push(code_gen::ast::Instruction::Mov { src: lhs, dest: ax });
                            ins.push(code_gen::ast::Instruction::Cdq);
                            ins.push(code_gen::ast::Instruction::Idiv { rhs });
                            ins.push(code_gen::ast::Instruction::Mov { src: dx, dest });
                            return;
                        }

                        tacky::ast::BinaryOp::Eq
                        | tacky::ast::BinaryOp::Ne
                        | tacky::ast::BinaryOp::Gt
                        | tacky::ast::BinaryOp::Lt
                        | tacky::ast::BinaryOp::Gte
                        | tacky::ast::BinaryOp::Lte => {
                            ins.push(code_gen::ast::Instruction::Cmp { lhs, rhs });

                            use code_gen::ast::CmpKind;
                            let op = match op {
                                tacky::ast::BinaryOp::Eq => CmpKind::Eq,
                                tacky::ast::BinaryOp::Ne => CmpKind::Ne,
                                tacky::ast::BinaryOp::Gt => CmpKind::Gt,
                                tacky::ast::BinaryOp::Lt => CmpKind::Lt,
                                tacky::ast::BinaryOp::Gte => CmpKind::Gte,
                                tacky::ast::BinaryOp::Lte => CmpKind::Lte,
                                _ => unreachable!(),
                            };

                            ins.push(code_gen::ast::Instruction::Mov {
                                src: code_gen::ast::Operand::Imm(0),
                                dest,
                            });
                            ins.push(code_gen::ast::Instruction::Cmp { lhs, rhs });
                            ins.push(code_gen::ast::Instruction::SetCC { cnd: op, dest });
                            return;
                        }
                    };

                    ins.push(code_gen::ast::Instruction::Mov { src: lhs, dest });
                    ins.push(code_gen::ast::Instruction::BinaryOp {
                        op,
                        lhs_dest: dest,
                        rhs,
                    });
                }
                tacky::ast::Instruction::LocalLabel(l) => ins.push(
                    code_gen::ast::Instruction::LocalLabel(code_gen::ast::Label(l.0)),
                ),
                tacky::ast::Instruction::Copy { src, dest } => {
                    let src = self.convert_val(src);
                    let dest = self.convert_val(dest);
                    ins.push(code_gen::ast::Instruction::Mov { src, dest })
                }
                tacky::ast::Instruction::Jump { target } => {
                    ins.push(code_gen::ast::Instruction::Jmp {
                        target: code_gen::ast::Label(target.0),
                    })
                }
                tacky::ast::Instruction::JumpIfZero { cond, target } => {
                    let cond = self.convert_val(cond);
                    ins.push(code_gen::ast::Instruction::Cmp {
                        lhs: cond,
                        rhs: code_gen::ast::Operand::Imm(0),
                    });
                    ins.push(code_gen::ast::Instruction::JmpCC {
                        cnd: code_gen::ast::CmpKind::Eq,
                        target: code_gen::ast::Label(target.0),
                    })
                }
                tacky::ast::Instruction::JumpIfNotZero { cond, target } => {
                    let cond = self.convert_val(cond);
                    ins.push(code_gen::ast::Instruction::Cmp {
                        lhs: cond,
                        rhs: code_gen::ast::Operand::Imm(0),
                    });
                    ins.push(code_gen::ast::Instruction::JmpCC {
                        cnd: code_gen::ast::CmpKind::Ne,
                        target: code_gen::ast::Label(target.0),
                    })
                }
            }
        }

        fn convert_val(&mut self, val: tacky::ast::Val) -> code_gen::ast::Operand {
            match val {
                tacky::ast::Val::Const(val) => code_gen::ast::Operand::Imm(val),
                tacky::ast::Val::Var(var) => code_gen::ast::Operand::Pseudo(var),
            }
        }
    }
}
