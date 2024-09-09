use std::collections::HashMap;

use crate::{parser::ast, util::{self, info::VarId}};

#[derive(Debug)]
pub enum SemanitcError<'a> {
    VariableNotDefined(&'a str),
    VariableAlreadyDefined(&'a str),
}

#[derive(Default)]
struct ScopeTracker<'a> {
    map: HashMap<&'a str, VarId>,
}

pub struct SemanticAnalysis<'a, 'b> {
    errors: Vec<SemanitcError<'a>>,
    map: ScopeTracker<'a>,
    info: &'b mut util::info::CompilerInfo<'a>,
}

impl<'a, 'b> SemanticAnalysis<'a, 'b> {
    pub fn new(info: &'b mut util::info::CompilerInfo<'a>) -> Self {
        Self {
            errors: Vec::new(),
            map: ScopeTracker::default(),
            info
        }
    }

    pub fn resolve_pass(&mut self, ast: &mut ast::Program<'a>) -> Result<(), Vec<SemanitcError>> {
        for item in &mut ast.0 {
            self.resolve_top_level(item);
        }
        Ok(())
    }

    fn resolve_top_level(&mut self, top_lvl: &mut ast::TopLevel<'a>) {
        match top_lvl {
            ast::TopLevel::FunctionDef(func) => self.resolve_function_def(func),
        }
    }

    fn resolve_function_def(&mut self, func_def: &mut ast::FunctionDef<'a>) {
        for item in &mut func_def.body {
            self.resolve_block_item(item)
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

    fn resolve_expr(&mut self, expr: &mut ast::Expr<'a>) {
        match expr {
            ast::Expr::Constant(_) => {},
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

        if self.map.map.insert(ident.name, var).is_some() {
            self.errors
                .push(SemanitcError::VariableAlreadyDefined(ident.name));
        }
        ident.resolve = Some(var);
    }

    fn resolve_ident(&mut self, ident: &mut ast::Ident<'a>) {
        if let Some(id) = self.map.map.get(ident.name) {
            ident.resolve = Some(*id);
        } else {
            self.errors
                .push(SemanitcError::VariableNotDefined(ident.name));
        }
    }
}
