use crate::tacky::ast::*;




pub struct ConstantFolding{

    folded: bool,
}

impl ConstantFolding{
    pub fn new() -> Self{
        ConstantFolding {

            folded: false
        }
    }

    pub fn changed(&self) -> bool{
        self.folded
    }

    pub fn run(mut self, program: &mut Program<'_>) -> Self{
        for item in &mut program.0{
            self.fold_top_level(item);
        }
        self
    }

    fn fold_top_level(&mut self, item: &mut TopLevel){
        match item{
            TopLevel::FunctionDef(func) => self.fold_func_def(func)
        }
    }
    
    fn fold_func_def(&mut self, func: &mut FunctionDef){
        for ins in &mut func.instructions{
            self.fold_ins(ins);
        }
    }

    fn fold_ins(&mut self, ins: &mut Instruction){
        match ins{
            Instruction::Unary { op, dest, src: Val::Const(val) } => {
                let mut folded = true;
                match op{
                    UnaryOp::Minus => *ins = Instruction::Copy { src: Val::Const(-*val), dest: *dest },
                    UnaryOp::BitNot => *ins = Instruction::Copy { src: Val::Const(!*val), dest: *dest },
                    UnaryOp::LogNot => *ins = Instruction::Copy { src: Val::Const(if *val == 0 {1}else{0}), dest: *dest },
                    _ => folded = false,
                }
                self.folded |= folded;
            },
            Instruction::Binary { op, lhs:  Val::Const(lhs), rhs: Val::Const(rhs), dest } => {
                match op{
                    BinaryOp::Addition => *ins = Instruction::Copy { src: Val::Const(lhs.wrapping_add(*rhs)), dest: *dest },
                    BinaryOp::Subtract => *ins = Instruction::Copy { src: Val::Const(lhs.wrapping_sub(*rhs)), dest: *dest },
                    BinaryOp::Multiply => *ins = Instruction::Copy { src: Val::Const(lhs.wrapping_mul(*rhs)), dest: *dest },
                    BinaryOp::Divide => *ins = Instruction::Copy { src: Val::Const(if*rhs == 0{0}else{lhs.wrapping_div(*rhs)}), dest: *dest },
                    BinaryOp::Remainder => *ins = Instruction::Copy { src: Val::Const(if*rhs == 0{0}else{lhs.wrapping_rem(*rhs)}), dest: *dest },
                    BinaryOp::ShiftRight => *ins = Instruction::Copy { src: Val::Const(lhs.wrapping_shr(*rhs as u32)), dest: *dest },
                    BinaryOp::ShiftLeft => *ins = Instruction::Copy { src: Val::Const(lhs.wrapping_shl(*rhs as u32)), dest: *dest },
                    BinaryOp::BitXor => *ins = Instruction::Copy { src: Val::Const(*lhs ^ *rhs), dest: *dest },
                    BinaryOp::BitOr => *ins = Instruction::Copy { src: Val::Const(*lhs | *rhs), dest: *dest },
                    BinaryOp::BitAnd => *ins = Instruction::Copy { src: Val::Const(*lhs & *rhs), dest: *dest },
                    BinaryOp::Eq => *ins = Instruction::Copy { src: Val::Const(if *lhs == *rhs {1}else{0}), dest: *dest },
                    BinaryOp::Ne => *ins = Instruction::Copy { src: Val::Const(if *lhs != *rhs {1}else{0}), dest: *dest },
                    BinaryOp::Gt => *ins = Instruction::Copy { src: Val::Const(if *lhs > *rhs {1}else{0}), dest: *dest },
                    BinaryOp::Lt => *ins = Instruction::Copy { src: Val::Const(if *lhs < *rhs {1}else{0}), dest: *dest },
                    BinaryOp::Gte => *ins = Instruction::Copy { src: Val::Const(if *lhs >= *rhs {1}else{0}), dest: *dest },
                    BinaryOp::Lte => *ins = Instruction::Copy { src: Val::Const(if *lhs <= *rhs {1}else{0}), dest: *dest },
                }
                self.folded = true;
            },
            _ => {}
        }
    }
}