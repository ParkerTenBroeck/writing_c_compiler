pub mod ast;

use crate::{parser, util};

pub struct TackyGen<'a, 'b> {
    info: &'b mut util::info::CompilerInfo<'a>,
}

impl<'a, 'b> TackyGen<'a, 'b> {
    pub fn new(info: &'b mut util::info::CompilerInfo<'a>) -> Self {
        Self { info }
    }

    pub fn ast_to_tacky(&mut self, input: parser::ast::Program<'a>) -> ast::Program<'a> {
        let mut prog = ast::Program(Vec::new());
        for top in input.0 {
            prog.0.push(self.top_level_to_tacky(top));
        }
        prog
    }

    pub fn top_level_to_tacky(&mut self, input: parser::ast::TopLevel<'a>) -> ast::TopLevel<'a> {
        match input {
            parser::ast::TopLevel::FunctionDef(func) => {
                let mut ins = Vec::new();

                for item in func.body {
                    self.block_item_to_tacky(&mut ins, item);
                }
                ast::TopLevel::FunctionDef(ast::FunctionDef {
                    name: func.name,
                    instructions: ins,
                })
            }
        }
    }

    pub fn block_item_to_tacky(
        &mut self,
        ins: &mut Vec<ast::Instruction>,
        blk_item: parser::ast::BlockItem<'a>,
    ) {
        match blk_item {
            parser::ast::BlockItem::Statement(stmt) => self.statement_to_tacky(ins, stmt),
            parser::ast::BlockItem::Declaration(decl) => self.declaration_to_tacky(ins, decl),
        }
    }

    fn declaration_to_tacky(
        &mut self,
        ins: &mut Vec<ast::Instruction>,
        decl: parser::ast::Declaration,
    ) {
        let var = ast::Val::Var(decl.name.resolve.unwrap());
        if let Some(expr) = decl.expr {
            let src = self.expression_to_tacky(ins, expr);
            ins.push(ast::Instruction::Copy { src, dest: var })
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

                    parser::ast::BinaryOp::Assignment
                    | parser::ast::BinaryOp::PlusEq
                    | parser::ast::BinaryOp::MinusEq
                    | parser::ast::BinaryOp::TimesEq
                    | parser::ast::BinaryOp::DivideEq
                    | parser::ast::BinaryOp::ModuloEq
                    | parser::ast::BinaryOp::ShiftLeftEq
                    | parser::ast::BinaryOp::ShiftRightEq
                    | parser::ast::BinaryOp::AndEq
                    | parser::ast::BinaryOp::OrEq
                    | parser::ast::BinaryOp::XorEq => {
                        if let parser::ast::Expr::Ident(_) = &*lhs {
                        } else {
                            todo!("unsupported non identifier assignment");
                        }
                        let lhs = self.expression_to_tacky(ins, *lhs);
                        let rhs = self.expression_to_tacky(ins, *rhs);
                        match op {
                            parser::ast::BinaryOp::Assignment => ins.push(ast::Instruction::Copy {
                                src: rhs,
                                dest: lhs,
                            }),
                            parser::ast::BinaryOp::PlusEq => ins.push(ast::Instruction::Binary {
                                op: ast::BinaryOp::Addition,
                                lhs,
                                rhs,
                                dest: lhs,
                            }),
                            parser::ast::BinaryOp::MinusEq => ins.push(ast::Instruction::Binary {
                                op: ast::BinaryOp::Subtract,
                                lhs,
                                rhs,
                                dest: lhs,
                            }),
                            parser::ast::BinaryOp::TimesEq => ins.push(ast::Instruction::Binary {
                                op: ast::BinaryOp::Multiply,
                                lhs,
                                rhs,
                                dest: lhs,
                            }),
                            parser::ast::BinaryOp::DivideEq => ins.push(ast::Instruction::Binary {
                                op: ast::BinaryOp::Divide,
                                lhs,
                                rhs,
                                dest: lhs,
                            }),
                            parser::ast::BinaryOp::ModuloEq => ins.push(ast::Instruction::Binary {
                                op: ast::BinaryOp::Remainder,
                                lhs,
                                rhs,
                                dest: lhs,
                            }),
                            parser::ast::BinaryOp::ShiftLeftEq => {
                                ins.push(ast::Instruction::Binary {
                                    op: ast::BinaryOp::ShiftLeft,
                                    lhs,
                                    rhs,
                                    dest: lhs,
                                })
                            }
                            parser::ast::BinaryOp::ShiftRightEq => {
                                ins.push(ast::Instruction::Binary {
                                    op: ast::BinaryOp::ShiftRight,
                                    lhs,
                                    rhs,
                                    dest: lhs,
                                })
                            }
                            parser::ast::BinaryOp::AndEq => ins.push(ast::Instruction::Binary {
                                op: ast::BinaryOp::BitAnd,
                                lhs,
                                rhs,
                                dest: lhs,
                            }),
                            parser::ast::BinaryOp::OrEq => ins.push(ast::Instruction::Binary {
                                op: ast::BinaryOp::BitOr,
                                lhs,
                                rhs,
                                dest: lhs,
                            }),
                            parser::ast::BinaryOp::XorEq => ins.push(ast::Instruction::Binary {
                                op: ast::BinaryOp::BitXor,
                                lhs,
                                rhs,
                                dest: lhs,
                            }),
                            _ => unreachable!(),
                        }

                        return lhs;
                    }
                };

                let lhs = self.expression_to_tacky(ins, *lhs);
                let rhs = self.expression_to_tacky(ins, *rhs);
                let dest = self.next_tmp_var();
                ins.push(ast::Instruction::Binary { op, lhs, rhs, dest });
                dest
            }
            parser::ast::Expr::Ident(ident) => ast::Val::Var(ident.resolve.unwrap()),
        }
    }

    fn next_tmp_var(&mut self) -> ast::Val {
        ast::Val::Var(self.info.next_tmp_var())
    }

    fn next_tmp_label(&mut self) -> util::info::LabelId {
        self.info.next_tmp_label()
    }
}
