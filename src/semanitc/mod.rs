use std::collections::HashMap;

use crate::{
    parser::ast::{self, Path, Type},
    util::{
        self,
        info::{Node, NodeId, VarId},
    },
};

#[derive(Debug)]
pub enum SemanitcError<'a> {
    VariableNotDefined(&'a str),
    VariableAlreadyDefined(&'a str),
}

#[derive(Default)]
struct ScopeTracker<'a> {
    map: Vec<HashMap<Path<'a>, (VarId, NodeId)>>,
}

impl<'a> ScopeTracker<'a>{
    pub fn enter_scope(&mut self){
        self.map.push(HashMap::new())
    }

    pub fn exit_scope(&mut self){
        self.map.pop();
    }
}

pub struct SemanticAnalysis<'a, 'b> {
    map: ScopeTracker<'a>,
    info: &'b mut util::info::CompilerInfo<'a>,
}

impl<'a, 'b> SemanticAnalysis<'a, 'b> {
    pub fn new(info: &'b mut util::info::CompilerInfo<'a>) -> Self {
        Self {
            map: ScopeTracker::default(),
            info,
        }
    }

    pub fn resolve_pass(&mut self, ast: &mut ast::Program<'a>) {
        self.map.enter_scope();
        for item in &mut ast.0 {
            self.resolve_top_level(&mut item.0);
        }
        self.map.exit_scope();
    }

    fn resolve_top_level(&mut self, top_lvl: &mut ast::TopLevel<'a>) {
        match top_lvl {
            ast::TopLevel::FunctionDef(func) => self.resolve_function_def(func),
        }
    }

    fn resolve_function_def(&mut self, func_def: &mut ast::FunctionDef<'a>) {
        self.resolve_block(&mut func_def.body);
    }

    fn resolve_block(&mut self, block: &mut Node<ast::Block<'a>>) {
        for item in &mut block.0.body {
            self.resolve_block_item(&mut item.0)
        }
    }

    fn resolve_block_item(&mut self, item: &mut ast::BlockItem<'a>) {
        match item {
            ast::BlockItem::Statement(stmt) => self.resolve_statement(stmt),
            ast::BlockItem::Declaration(decl) => self.resolve_declaration(decl),
        }
    }

    fn resolve_statement(&mut self, stmt: &mut ast::Statement<'a>) {
        match stmt {
            ast::Statement::Return(expr) => self.resolve_expr(expr),
            ast::Statement::Expression(expr) => self.resolve_expr(expr),
            ast::Statement::Continue { label} => {
                
            }
            ast::Statement::Break { label, expr } => {
                if let Some(expr) = expr {
                    self.resolve_expr(expr)
                }
            }
            ast::Statement::Empty => {}
        }
    }

    fn resolve_declaration(&mut self, decl: &mut ast::Declaration<'a>) {
        self.resolve_decl(decl);
        if let Some(expr) = &mut decl.expr {
            self.resolve_expr(expr)
        }
    }

    fn resolve_expr(&mut self, expr: &mut Node<ast::Expr<'a>>) {
        match &mut expr.0 {
            ast::Expr::Constant(_) => {}
            ast::Expr::Unary(_, expr) => self.resolve_expr(expr),
            ast::Expr::Binary { lhs, rhs, .. } => {
                self.resolve_expr(lhs);
                self.resolve_expr(rhs);
            }
            ast::Expr::Ident(ident) => self.resolve_ident(ident),

            ast::Expr::If {
                label: _,
                cond,
                met,
                not_met,
            } => {
                self.resolve_expr(cond);

                self.map.enter_scope();
                self.resolve_block(met);
                self.map.exit_scope();
                if let Some(not_met) = not_met {
                    self.map.enter_scope();
                    self.resolve_block(not_met);
                    self.map.exit_scope();
                }
            }
            ast::Expr::While {
                label: _,
                cond,
                body,
            } => {
                match &mut **cond {
                    ast::LoopCond::Infinite => {}
                    ast::LoopCond::While(expr) => {
                        self.resolve_expr(expr);
                    }
                    ast::LoopCond::DoWhile(expr) => {
                        self.resolve_expr(expr);
                    }
                }
                self.map.enter_scope();
                self.resolve_block(body);
                self.map.exit_scope();
            }
            ast::Expr::Block { label: _, inner } => {
                self.map.enter_scope();
                self.resolve_block(inner);
                self.map.exit_scope();
            },
            ast::Expr::Cast { .. } => todo!(),
        }
    }

    fn resolve_decl(&mut self, decl: &mut ast::Declaration<'a>) {
        let var = self.info.next_tmp_var();

        let Some(node_id) = decl.name.name.1 else {
            let err = decl.name.name.error(
                self.info,
                format!(
                    "variable declaration {:?} identifier does not have a node id",
                    decl.name.name.0.ident
                ),
            );
            self.info.report_error(err);
            return;
        };

        let mut iter = self.map.map.iter_mut().rev();
        let Some(last) = iter.next() else {
            self.info.report_error(decl.name.name.error(
                self.info,
                "somehow the resolver map has zero items in it... weird".into(),
            ));
            return;
        };
        if let Some(first) = last.insert(decl.name.name.0.clone(), (var, node_id)) {
            self.info.report_error(decl.name.name.error(
                self.info,
                format!("variable {:?} is already defined", decl.name.name.0.ident),
            ));
            self.info
                .report_error(first.1.error(self.info, "here".into()));
        }
        for previous in iter {
            if let Some(first) = previous.insert(decl.name.name.0.clone(), (var, node_id)) {
                self.info.report_error(decl.name.name.warn(
                    self.info,
                    format!("variable {:?} shadows", decl.name.name.0.ident),
                ));
                self.info
                    .report_error(first.1.warn(self.info, "this definition".into()));
            }
        }
        decl.name.resolve = Some(var);
    }

    fn resolve_ident(&mut self, decl: &mut ast::Ident<'a>) {
        let mut iter = self.map.map.iter_mut().rev();

        for level in &mut iter {
            if let Some(id) = level.get(&decl.name.0) {
                decl.resolve = Some(id.0);
                return;
            }
        }
        self.info.report_error(decl.name.error(
            self.info,
            format!("variable {:?} is not defined", decl.name.0.ident),
        ));
    }
}
