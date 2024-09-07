pub mod ast;

use crate::parser;

#[derive(Default)]
pub struct TackyGen {
    tmp_variable: usize,
    tmp_label: usize,
}

impl TackyGen {
    pub fn new() -> Self {
        Self {
            tmp_variable: 0,
            tmp_label: 0,
        }
    }

    fn next_tmp_var(&mut self) -> ast::Val {
        self.tmp_variable += 1;
        ast::Val::Var(self.tmp_variable - 1)
    }

    fn next_tmp_label(&mut self) -> ast::Label {
        self.tmp_label += 1;
        ast::Label(self.tmp_label - 1)
    }

    pub fn ast_to_tacky<'a>(&mut self, input: parser::ast::Program<'a>) -> ast::Program<'a> {
        let mut prog = ast::Program(Vec::new());
        for top in input.0 {
            prog.0.push(self.top_level_to_tacky(top));
        }
        prog
    }

    pub fn top_level_to_tacky<'a>(
        &mut self,
        input: parser::ast::TopLevel<'a>,
    ) -> ast::TopLevel<'a> {
        match input {
            parser::ast::TopLevel::FunctionDef(func) => {
                let mut ins = Vec::new();
                for item in func.body {
                    self.block_item_to_tacky(&mut ins, item);
                }
                ast::TopLevel::FunctionDef(ast::FunctionDef {
                    name: func.name,
                    temps: self.tmp_variable,
                    instructions: ins,
                })
            }
        }
    }

    pub fn block_item_to_tacky<'a>(
        &mut self,
        ins: &mut Vec<ast::Instruction>,
        blk_item: parser::ast::BlockItem<'a>,
    ) {
        match blk_item {
            parser::ast::BlockItem::Statement(stmt) => self.statement_to_tacky(ins, stmt),
            parser::ast::BlockItem::Declaration(_) => todo!(),
        }
    }

    pub fn statement_to_tacky(
        &mut self,
        ins: &mut Vec<ast::Instruction>,
        smt: parser::ast::Statement,
    ) {
        match smt {
            parser::ast::Statement::Return(expr) => {
                let v = ast::Instruction::Return(self.expression_to_tacky(ins, expr));
                ins.push(v);
            }
            parser::ast::Statement::Expression(expr) => {
                self.expression_to_tacky(ins, expr);
            }
            parser::ast::Statement::Empty => {}
        }
    }

    pub fn expression_to_tacky(
        &mut self,
        ins: &mut Vec<ast::Instruction>,
        expr: parser::ast::Expr,
    ) -> ast::Val {
        match expr {
            parser::ast::Expr::Constant(val) => ast::Val::Const(match val {
                parser::ast::Literal::Number(num) => num.get_num().parse().unwrap(),
                parser::ast::Literal::String(_) => todo!(),
                parser::ast::Literal::Char(char) => char as i32,
                parser::ast::Literal::Bool(bool) => {
                    if bool {
                        1
                    } else {
                        0
                    }
                }
            }),
            parser::ast::Expr::Unary(op, expr) => {
                let src = self.expression_to_tacky(ins, *expr);
                let dest = self.next_tmp_var();
                ins.push(ast::Instruction::Unary {
                    op: match op {
                        parser::ast::UnaryOp::Neg => ast::UnaryOp::Minus,
                        parser::ast::UnaryOp::BitNot => ast::UnaryOp::BitNot,
                        parser::ast::UnaryOp::LogNot => ast::UnaryOp::LogNot,
                        parser::ast::UnaryOp::PreInc => ast::UnaryOp::PreInc,
                        parser::ast::UnaryOp::PreDec => ast::UnaryOp::PreDec,
                        parser::ast::UnaryOp::PostInc => ast::UnaryOp::PostInc,
                        parser::ast::UnaryOp::PostDec => ast::UnaryOp::PostDec,
                    },
                    dest,
                    src,
                });
                dest
            }
            parser::ast::Expr::Binary { op, lhs, rhs } => {
                let op = match op {
                    parser::ast::BinaryOp::Addition => ast::BinaryOp::Addition,
                    parser::ast::BinaryOp::Subtract => ast::BinaryOp::Subtract,
                    parser::ast::BinaryOp::Multiply => ast::BinaryOp::Multiply,
                    parser::ast::BinaryOp::Divide => ast::BinaryOp::Divide,
                    parser::ast::BinaryOp::Remainder => ast::BinaryOp::Remainder,

                    parser::ast::BinaryOp::BitAnd => ast::BinaryOp::BitAnd,
                    parser::ast::BinaryOp::BitOr => ast::BinaryOp::BitOr,
                    parser::ast::BinaryOp::BitXor => ast::BinaryOp::BitXor,

                    parser::ast::BinaryOp::ShiftLeft => ast::BinaryOp::ShiftLeft,
                    parser::ast::BinaryOp::ShiftRight => ast::BinaryOp::ShiftRight,

                    parser::ast::BinaryOp::LogAnd => {
                        let false_branch = self.next_tmp_label();
                        let end_branch = self.next_tmp_label();
                        let res = self.next_tmp_var();

                        let lhs = self.expression_to_tacky(ins, *lhs);
                        ins.push(ast::Instruction::JumpIfZero {
                            cond: lhs,
                            target: false_branch,
                        });

                        let rhs = self.expression_to_tacky(ins, *rhs);
                        ins.push(ast::Instruction::JumpIfZero {
                            cond: rhs,
                            target: false_branch,
                        });

                        ins.push(ast::Instruction::Copy {
                            src: ast::Val::Const(1),
                            dest: res,
                        });
                        ins.push(ast::Instruction::Jump { target: end_branch });
                        ins.push(ast::Instruction::LocalLabel(false_branch));
                        ins.push(ast::Instruction::Copy {
                            src: ast::Val::Const(0),
                            dest: res,
                        });
                        ins.push(ast::Instruction::LocalLabel(end_branch));

                        return res;
                    }
                    parser::ast::BinaryOp::LogOr => {
                        let true_branch = self.next_tmp_label();
                        let end_branch = self.next_tmp_label();
                        let res = self.next_tmp_var();

                        let lhs = self.expression_to_tacky(ins, *lhs);
                        ins.push(ast::Instruction::JumpIfNotZero {
                            cond: lhs,
                            target: true_branch,
                        });

                        let rhs = self.expression_to_tacky(ins, *rhs);
                        ins.push(ast::Instruction::JumpIfNotZero {
                            cond: rhs,
                            target: true_branch,
                        });

                        ins.push(ast::Instruction::Copy {
                            src: ast::Val::Const(0),
                            dest: res,
                        });
                        ins.push(ast::Instruction::Jump { target: end_branch });
                        ins.push(ast::Instruction::LocalLabel(true_branch));
                        ins.push(ast::Instruction::Copy {
                            src: ast::Val::Const(1),
                            dest: res,
                        });
                        ins.push(ast::Instruction::LocalLabel(end_branch));

                        return res;
                    }

                    parser::ast::BinaryOp::Eq => ast::BinaryOp::Eq,
                    parser::ast::BinaryOp::Ne => ast::BinaryOp::Ne,

                    parser::ast::BinaryOp::Gt => ast::BinaryOp::Gt,
                    parser::ast::BinaryOp::Gte => ast::BinaryOp::Gte,
                    parser::ast::BinaryOp::Lt => ast::BinaryOp::Lt,
                    parser::ast::BinaryOp::Lte => ast::BinaryOp::Lte,

                    parser::ast::BinaryOp::Assignment => todo!(),
                    parser::ast::BinaryOp::PlusEq => todo!(),
                    parser::ast::BinaryOp::MinusEq => todo!(),
                    parser::ast::BinaryOp::TimesEq => todo!(),
                    parser::ast::BinaryOp::DivideEq => todo!(),
                    parser::ast::BinaryOp::ModuloEq => todo!(),
                    parser::ast::BinaryOp::ShiftLeftEq => todo!(),
                    parser::ast::BinaryOp::ShiftRightEq => todo!(),
                    parser::ast::BinaryOp::AndEq => todo!(),
                    parser::ast::BinaryOp::OrEq => todo!(),
                    parser::ast::BinaryOp::XorEq => todo!(),
                };

                let lhs = self.expression_to_tacky(ins, *lhs);
                let rhs = self.expression_to_tacky(ins, *rhs);
                let dest = self.next_tmp_var();
                ins.push(ast::Instruction::Binary { op, lhs, rhs, dest });
                dest
            }
            parser::ast::Expr::Assignment { kind, dest, src } => {
                if let parser::ast::Expr::Ident(ident) = *dest {
                } else {
                    todo!("unsupported non identifier assignment");
                }
                match kind {
                    parser::ast::AssignmentKind::Assign => todo!(),
                    parser::ast::AssignmentKind::PlusEq => todo!(),
                    parser::ast::AssignmentKind::MinusEq => todo!(),
                    parser::ast::AssignmentKind::TimesEq => todo!(),
                    parser::ast::AssignmentKind::DivEq => todo!(),
                    parser::ast::AssignmentKind::ModEq => todo!(),
                    parser::ast::AssignmentKind::AndEq => todo!(),
                    parser::ast::AssignmentKind::OrEq => todo!(),
                    parser::ast::AssignmentKind::XorEq => todo!(),
                    parser::ast::AssignmentKind::ShlEq => todo!(),
                    parser::ast::AssignmentKind::ShrEq => todo!(),
                }
            }
            parser::ast::Expr::Ident(ident) => todo!(),
        }
    }
}
