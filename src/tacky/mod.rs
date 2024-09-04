pub mod ast;

use crate::parser;

pub struct TackyGen{
    tmp: usize,
}

impl TackyGen{
    pub fn new() -> Self{
        Self { 
            tmp: 0
        }
    }

    fn next_tmp(&mut self) -> ast::Val{
        self.tmp += 1;
        ast::Val::Var(self.tmp - 1)
    }

    pub fn ast_to_tacky<'a>(&mut self, input: parser::ast::Program<'a>) -> ast::Program<'a>{
        match input{
            parser::ast::Program::FunctionDef(func) => {
                let mut ins = Vec::new();
                self.statement_to_tacky(&mut ins, func.body);
                ast::Program::FunctionDef(ast::FunctionDef{
                    name: func.name,
                    temps: self.tmp,
                    instructions: ins
                })
            },
        }
    }

    pub fn statement_to_tacky(&mut self, ins: &mut Vec<ast::Instruction>, smt: parser::ast::Statement){
        match smt{
            parser::ast::Statement::Return(expr) => {
                let v = ast::Instruction::Return(self.expression_to_tacky(ins, expr));
                ins.push(v);
            },
        }
    }

    pub fn expression_to_tacky(&mut self, ins: &mut Vec<ast::Instruction>, expr: parser::ast::Expr) -> ast::Val{
        match expr{
            parser::ast::Expr::Constant(val) => ast::Val::Const(match val{
                parser::ast::Literal::Number(num) => num.get_num().parse().unwrap(),
                parser::ast::Literal::String(_) => todo!(),
                parser::ast::Literal::Char(char) => char as i32,
                parser::ast::Literal::Bool(bool) => if bool {1} else {0},
            }),
            parser::ast::Expr::Unary(op, expr) => {
                let src = self.expression_to_tacky(ins, *expr);
                let dest = self.next_tmp();
                ins.push(ast::Instruction::Unary { op: 
                    match op{
                        parser::ast::UnaryOp::Neg => ast::UnaryOp::Minus,
                        parser::ast::UnaryOp::Not => ast::UnaryOp::Not,
                        parser::ast::UnaryOp::PreInc => ast::UnaryOp::PreInc,
                        parser::ast::UnaryOp::PreDec => ast::UnaryOp::PreDec,
                        parser::ast::UnaryOp::PostInc => ast::UnaryOp::PostInc,
                        parser::ast::UnaryOp::PostDec => ast::UnaryOp::PostDec,
                    }, dest, src });
                    dest
            },
            parser::ast::Expr::Binary { op, lhs, rhs } => {
                let op =  match op{
                    parser::ast::BinaryOp::Addition => ast::BinaryOp::Addition,
                    parser::ast::BinaryOp::Subtract => ast::BinaryOp::Subtract,
                    parser::ast::BinaryOp::Multiply => ast::BinaryOp::Multiply,
                    parser::ast::BinaryOp::Divide => ast::BinaryOp::Divide,
                    parser::ast::BinaryOp::Remainder => ast::BinaryOp::Remainder,
                };

                let lhs = self.expression_to_tacky(ins, *lhs);
                let rhs = self.expression_to_tacky(ins, *rhs);
                let dest = self.next_tmp();
                ins.push(ast::Instruction::Binary { op, lhs, rhs, dest });
                dest
            },
        }
    }
}