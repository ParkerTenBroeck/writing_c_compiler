pub mod ast;

pub fn asm_gen(
    out: impl std::fmt::Write,
    ast: crate::tacky::ast::Program<'_>,
) -> std::fmt::Result {
    let mut ast = gen::AsmGen::new().gen(ast);
    register_allocation::RegAllocation::new().run_regalloc(&mut ast);
    fixup::AsmFixup::new().run_fixup(&mut ast);
    emmision::AsmEmission::new(out).emit_asm(ast)
}

pub mod register_allocation{
    use crate::code_gen::ast::*;

    #[derive(Default)]
    pub struct RegAllocation{

    }

    impl RegAllocation{
        pub fn new() -> Self{
            RegAllocation {  }
        }

        pub fn run_regalloc(&mut self, prog: &mut Program<'_>) {
            for top in &mut prog.0{
                self.regalloc_top(top);
            }
        }
        
        fn regalloc_top(&mut self, top: &mut TopLevel<'_>) {
            match top{
                TopLevel::FunctionDef(func) => self.regalloc_func(func),
            }
        }
        
        fn regalloc_func(&mut self, func: &mut FunctionDef<'_>) {
            for ins in &mut func.instructions{
                self.regalloc_ins(ins);
            }
        }
        
        fn regalloc_ins(&mut self, ins: &mut Instruction) {
            match ins{
                Instruction::Mov { src, dest } => {
                    self.regalloc_op(src);
                    self.regalloc_op(dest);
                },
                Instruction::UnaryOp { src_dest, .. } => {
                    self.regalloc_op(src_dest);
                },
                Instruction::BinaryOp { lhs_dest, rhs , ..} => {
                    self.regalloc_op(lhs_dest);
                    self.regalloc_op(rhs);
                },
                Instruction::Idiv { rhs: quote } => {
                    self.regalloc_op(quote)
                },
                Instruction::Cmp { lhs, rhs } => {
                    self.regalloc_op(lhs);
                    self.regalloc_op(rhs);
                },
                Instruction::SetCC { dest, .. } => {
                    self.regalloc_op(dest);
                },

                Instruction::AllocStack(_) 
                | Instruction::Jmp { .. }
                | Instruction::Cdq 
                | Instruction::LocalLabel(_)
                | Instruction::JmpCC { .. } 
                | Instruction::Ret => {},
            }
        }

        fn regalloc_op(&mut self, op: &mut Operand){
            if let Operand::Pseudo(pseudo) = op {
                *op = Operand::Stack(*pseudo);
            }
        }
    }
}

pub mod fixup{
    use crate::code_gen::ast::*;

    #[derive(Default)]
    pub struct AsmFixup{

    }

    impl AsmFixup{
        pub fn new() -> Self{
            Self{ }
        }

        pub fn run_fixup(&mut self, prog: &mut Program<'_>) {
            for top in &mut prog.0{
                self.fixup_top(top);
            }
        }
        
        fn fixup_top(&self, top: &mut TopLevel<'_>) {
            match top{
                TopLevel::FunctionDef(func) => self.fixup_func(func),
            }
        }
        
        fn fixup_func(&self, func: &mut FunctionDef<'_>) {
            let mut new_ins = Vec::new();
            std::mem::swap(&mut new_ins, &mut func.instructions);
            for ins in new_ins{
                self.fixup_ins(ins, &mut func.instructions);
            }
        }
        
        fn fixup_ins(&self, ins: Instruction, inss: &mut Vec<Instruction>) {
            match ins{
                Instruction::Mov { src, dest } => {
                    if src.is_mem() && dest.is_mem(){
                        inss.push(Instruction::Mov { src, dest: Operand::Reg(Register::R10) });
                        inss.push(Instruction::Mov { src: Operand::Reg(Register::R10), dest });
                    }else{
                        inss.push(ins);
                    }
                },
                Instruction::BinaryOp { op, lhs_dest, rhs } => {
                    if lhs_dest.is_mem() && op == BinaryOp::Multiply{
                        inss.push(Instruction::Mov { src: lhs_dest, dest: Operand::Reg(Register::R11) });
                        inss.push(Instruction::BinaryOp { op, lhs_dest: Operand::Reg(Register::R11), rhs });
                        inss.push(Instruction::Mov { src: Operand::Reg(Register::R11), dest: lhs_dest });
                    }else if lhs_dest.is_mem() && rhs.is_mem(){
                        inss.push(Instruction::Mov { src: rhs, dest: Operand::Reg(Register::R10) });
                        inss.push(Instruction::BinaryOp { op, lhs_dest, rhs: Operand::Reg(Register::R10) });
                    }else{
                        inss.push(Instruction::BinaryOp { op, lhs_dest, rhs });
                    }
                },

                Instruction::Cmp { lhs, rhs } => {
                    if lhs.is_mem() && rhs.is_mem() || rhs.is_const(){
                        inss.push(Instruction::Mov { src: rhs, dest: Operand::Reg(Register::R10) });
                        inss.push(Instruction::Cmp { lhs, rhs: Operand::Reg(Register::R10) });
                    }else{
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
                    if rhs.is_const(){
                        inss.push(Instruction::Mov { src: rhs, dest: Operand::Reg(Register::R10) });
                        inss.push(Instruction::Idiv { rhs: Operand::Reg(Register::R10) });
                    }else{
                        inss.push(Instruction::Idiv { rhs });
                    }
                },
            }
        }
    }
}

pub mod gen {
    use crate::{code_gen, tacky};

    #[derive(Default)]
    pub struct AsmGen{

    }

    impl AsmGen{
        pub fn new() -> Self{
            Self{}
        }

        pub fn gen<'a>(&mut self, ast: tacky::ast::Program<'a>) -> code_gen::ast::Program<'a> {
            let mut prog = code_gen::ast::Program(Vec::new());
            for top in ast.0{
                prog.0.push(self.gen_top_level(top));
            }
            prog
        }

        fn gen_top_level<'a>(&mut self, top: tacky::ast::TopLevel<'a>) -> code_gen::ast::TopLevel<'a>{
            match top{
                tacky::ast::TopLevel::FunctionDef(func) => code_gen::ast::TopLevel::FunctionDef(self.gen_function_def(func)),
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
                    ins.push(code_gen::ast::Instruction::Mov { src: self.convert_val(val), dest: code_gen::ast::Operand::Reg(code_gen::ast::Register::AX) });
                    ins.push(code_gen::ast::Instruction::Ret);
                },
                tacky::ast::Instruction::Unary { op, dest, src } => {
                    let op = match op{
                        tacky::ast::UnaryOp::Minus => code_gen::ast::UnaryOp::Neg,
                        tacky::ast::UnaryOp::BitNot => code_gen::ast::UnaryOp::BitNot,
                        tacky::ast::UnaryOp::LogNot => {
                            let src = self.convert_val(src);
                            let dest = self.convert_val(dest);
                            ins.push(code_gen::ast::Instruction::Cmp { lhs: code_gen::ast::Operand::Imm(0), rhs: src });
                            ins.push(code_gen::ast::Instruction::Mov { src: code_gen::ast::Operand::Imm(0), dest });
                            ins.push(code_gen::ast::Instruction::SetCC { cnd: code_gen::ast::CmpKind::Eq, dest });
                            return;
                        },
                        tacky::ast::UnaryOp::PreInc => todo!(),
                        tacky::ast::UnaryOp::PreDec => todo!(),
                        tacky::ast::UnaryOp::PostInc => todo!(),
                        tacky::ast::UnaryOp::PostDec => todo!(),
                    };
                    let src = self.convert_val(src);
                    let dest = self.convert_val(dest);

                    ins.push(code_gen::ast::Instruction::Mov { src, dest });
                    ins.push(code_gen::ast::Instruction::UnaryOp { op, src_dest: dest });
                    
                },
                tacky::ast::Instruction::Binary { op, lhs, rhs, dest } => {
                    let lhs = self.convert_val(lhs);
                    let rhs = self.convert_val(rhs);
                    let dest = self.convert_val(dest);
                    let op = match op{
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
                        },
                        tacky::ast::BinaryOp::Remainder => {
                            let ax = code_gen::ast::Operand::Reg(code_gen::ast::Register::AX);
                            let dx = code_gen::ast::Operand::Reg(code_gen::ast::Register::DX);
                            ins.push(code_gen::ast::Instruction::Mov { src: lhs, dest: ax });
                            ins.push(code_gen::ast::Instruction::Cdq);
                            ins.push(code_gen::ast::Instruction::Idiv { rhs });
                            ins.push(code_gen::ast::Instruction::Mov { src: dx, dest });
                            return;
                        },
                        
                        tacky::ast::BinaryOp::Eq |
                        tacky::ast::BinaryOp::Ne |
                        tacky::ast::BinaryOp::Gt |
                        tacky::ast::BinaryOp::Lt |
                        tacky::ast::BinaryOp::Gte |
                        tacky::ast::BinaryOp::Lte => {
                            ins.push(code_gen::ast::Instruction::Cmp { lhs, rhs });
                            
                            use code_gen::ast::CmpKind;
                            let op = match op{
                                tacky::ast::BinaryOp::Eq => CmpKind::Eq,
                                tacky::ast::BinaryOp::Ne => CmpKind::Ne,
                                tacky::ast::BinaryOp::Gt => CmpKind::Gt,
                                tacky::ast::BinaryOp::Lt => CmpKind::Lt,
                                tacky::ast::BinaryOp::Gte => CmpKind::Gte,
                                tacky::ast::BinaryOp::Lte => CmpKind::Lte,
                                _ => unreachable!()
                            };

                            ins.push(code_gen::ast::Instruction::Mov { src: code_gen::ast::Operand::Imm(0), dest });
                            ins.push(code_gen::ast::Instruction::Cmp { lhs, rhs });
                            ins.push(code_gen::ast::Instruction::SetCC { cnd: op, dest });
                            return;
                        },
                    };

                    ins.push(code_gen::ast::Instruction::Mov { src: lhs, dest });
                    ins.push(code_gen::ast::Instruction::BinaryOp { op, lhs_dest: dest, rhs });
                },
                tacky::ast::Instruction::LocalLabel(l) => {
                    ins.push(code_gen::ast::Instruction::LocalLabel(code_gen::ast::Label(l.0)))
                },
                tacky::ast::Instruction::Copy { src, dest } => {
                    let src = self.convert_val(src);
                    let dest = self.convert_val(dest);
                    ins.push(code_gen::ast::Instruction::Mov { src, dest })
                },
                tacky::ast::Instruction::Jump { target } => {
                    ins.push(code_gen::ast::Instruction::Jmp{ target: code_gen::ast::Label(target.0) })
                },
                tacky::ast::Instruction::JumpIfZero { cond, target } => {
                    let cond = self.convert_val(cond);
                    ins.push(code_gen::ast::Instruction::Cmp { lhs: cond, rhs: code_gen::ast::Operand::Imm(0) });
                    ins.push(code_gen::ast::Instruction::JmpCC{ cnd: code_gen::ast::CmpKind::Eq, target: code_gen::ast::Label(target.0) })
                },
                tacky::ast::Instruction::JumpIfNotZero { cond, target } => {
                    let cond = self.convert_val(cond);
                    ins.push(code_gen::ast::Instruction::Cmp { lhs: cond, rhs: code_gen::ast::Operand::Imm(0) });
                    ins.push(code_gen::ast::Instruction::JmpCC{ cnd: code_gen::ast::CmpKind::Ne, target: code_gen::ast::Label(target.0) })
                
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

            for top in ast.0{
                self.emit_top_level(top)?;
            }

            self.out.write_str(r#".section .note.GNU-stack,"",@progbits"#)?;
            self.out.write_str("\n")?;
            Ok(())
        }

        fn emit_top_level(
            &mut self,
            top: code_gen::ast::TopLevel<'_>
        ) -> std::fmt::Result{
            match top{
                code_gen::ast::TopLevel::FunctionDef(func) => self.emit_function(func)?,
            }
            Ok(())
        }

        fn emit_function(
            &mut self,
            func: code_gen::ast::FunctionDef<'_>
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
                    self.out.write_str("\tmovl ")?;
                    self.emit_operand(from, OperandSizeHint::U32)?;
                    self.out.write_str(", ")?;
                    self.emit_operand(to, OperandSizeHint::U32)?;
                    self.out.write_str("\n")?;
                }
                code_gen::ast::Instruction::Ret => self.out.write_str("\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret\n")?,
                code_gen::ast::Instruction::AllocStack(stack) => writeln!(self.out, "\tsubq ${}, %rsp", stack)?,
                code_gen::ast::Instruction::UnaryOp { op, src_dest } => {
                    match op{
                        code_gen::ast::UnaryOp::Neg => self.out.write_str("\tnegl ")?,
                        code_gen::ast::UnaryOp::BitNot => self.out.write_str("\tnotl ")?,
                        code_gen::ast::UnaryOp::LogNot => todo!(),
                    }
                    self.emit_operand(src_dest, OperandSizeHint::U32)?;
                    self.out.write_str("\n")?;
                },
                code_gen::ast::Instruction::BinaryOp { op, lhs_dest, rhs } => {
                    match op{
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
                },


                code_gen::ast::Instruction::Cdq => self.out.write_str("\tcdq\n")?,
                code_gen::ast::Instruction::Idiv { rhs: quote } => {
                    self.out.write_str("\tidivl ")?;
                    self.emit_operand(quote, OperandSizeHint::U32)?;
                    self.out.write_str("\n")?;
                },
                code_gen::ast::Instruction::Cmp { lhs, rhs } =>{
                    self.out.write_str("\tcmpl ")?;
                    self.emit_operand(lhs, OperandSizeHint::U32)?;
                    self.out.write_str(", ")?;
                    self.emit_operand(rhs, OperandSizeHint::U32)?;
                    self.out.write_str("\n")?;
                },
                code_gen::ast::Instruction::Jmp { target } => writeln!(self.out, "\tjmp .L{}", target.0)?,
                code_gen::ast::Instruction::JmpCC { cnd, target } => {
                    self.out.write_str("\tj")?;
                    let code = match cnd{
                        code_gen::ast::CmpKind::Eq => "e",
                        code_gen::ast::CmpKind::Ne => "ne",
                        code_gen::ast::CmpKind::Gt => "l",
                        code_gen::ast::CmpKind::Gte => "le",
                        code_gen::ast::CmpKind::Lt => "g",
                        code_gen::ast::CmpKind::Lte => "ge",
                    };
                    self.out.write_str(code)?;
                    writeln!(self.out, " .L{}", target.0)?;
                },
                code_gen::ast::Instruction::SetCC { cnd, dest } => {
                    self.out.write_str("\tset")?;
                    let code = match cnd{
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
                    
                },
                code_gen::ast::Instruction::LocalLabel(label) => writeln!(self.out, ".L{}:", label.0)?,
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
                code_gen::ast::Operand::Pseudo(val) => panic!("pseudo '{val}' regsiter exists at emission stage!"),
                code_gen::ast::Operand::Stack(val) =>  write!(self.out, "-{}(%rbp)", (val+1)*4),
            }
        }
    }

    #[allow(unused)]
    enum OperandSizeHint{
        U8,
        U16,
        U32,
        U64,
    }
}
