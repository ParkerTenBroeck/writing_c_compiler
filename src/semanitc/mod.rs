use std::collections::HashMap;

use crate::{
    parser::ast,
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
    map: HashMap<&'a str, (VarId, NodeId)>,
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
        for item in &mut ast.0 {
            self.resolve_top_level(&mut item.0);
        }
    }

    fn resolve_top_level(&mut self, top_lvl: &mut ast::TopLevel<'a>) {
        match top_lvl {
            ast::TopLevel::FunctionDef(func) => self.resolve_function_def(func),
        }
    }

    fn resolve_function_def(&mut self, func_def: &mut ast::FunctionDef<'a>) {
        for item in &mut func_def.body.0 {
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
            ast::Statement::Empty => {}
        }
    }

    fn resolve_declaration(&mut self, decl: &mut ast::Declaration<'a>) {
        self.resolve_decl(&mut decl.name);
        if let Some(expr) = &mut decl.expr {
            self.resolve_expr(expr)
        }
    }

    fn resolve_expr(&mut self, Node(expr, span): &mut Node<ast::Expr<'a>>) {
        match expr {
            ast::Expr::Constant(_) => {}
            ast::Expr::Unary(_, expr) => self.resolve_expr(expr),
            ast::Expr::Binary { lhs, rhs, .. } => {
                self.resolve_expr(lhs);
                self.resolve_expr(rhs);
            }
            ast::Expr::Ident(ident) => self.resolve_ident(ident),
        }
    }

    fn resolve_decl(&mut self, ident: &mut ast::Ident<'a>) {
        let var = self.info.next_tmp_var();

        let Some(node_id) = ident.name.1 else {
            let err = ident.name.error(
                self.info,
                format!(
                    "variable declaration {:?} identifier does not have a node id",
                    ident.name.0.ident
                ),
            );
            self.info.report_error(err);
            return;
        };

        if let Some(first) = self.map.map.insert(ident.name.0.ident, (var, node_id)) {
            self.info.report_error(ident.name.error(
                self.info,
                format!("variable {:?} is already defined", ident.name.0.ident),
            ));
            self.info
                .report_error(first.1.error(self.info, "here".into()));
        }
        ident.resolve = Some(var);
    }

    fn resolve_ident(&mut self, ident: &mut ast::Ident<'a>) {
        if let Some(id) = self.map.map.get(ident.name.0.ident) {
            ident.resolve = Some(id.0);
        } else {
            self.info.report_error(ident.name.error(
                self.info,
                format!("variable {:?} is not defined", ident.name.0.ident),
            ));
        }
    }
}
