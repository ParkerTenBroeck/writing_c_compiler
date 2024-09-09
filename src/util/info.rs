
use crate::lex::{Span, Spanned};

use super::error::ErrorNode;



#[derive(Debug)]
pub enum Var<'a>{
    Local(usize),
    Global(&'a str),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LabelId(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct VarId(pub usize);


#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Node<T>(pub T, pub NodeId);


#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct NodeId(pub(super) usize);

#[derive(Clone, Copy)]
pub struct Source<'a>{
    pub path: &'a str,
    pub contents: &'a str,
}

impl<'a> std::fmt::Debug for Source<'a>{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Source").field("path", &self.path).finish()
    }
}


#[derive(Default)]
pub struct CompilerInfo<'a>{
    pub func: Functioninfo,
    
    var_map: Vec<Var<'a>>,

    tmp_labels: usize,
    tmp_vals: usize,

    nodes: Vec<(Span, Source<'a>)>,
    errors: Vec<ErrorNode<'a>>,
}

#[derive(Debug, Default)]
pub struct Functioninfo{
    pub frame_size: usize,
}

impl<'a> CompilerInfo<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next_tmp_label(&mut self) -> LabelId {
        self.tmp_labels += 1;
        LabelId(self.tmp_labels - 1)
    }
    
    pub fn next_tmp_var(&mut self) -> VarId {
        let tmp = Var::Local(self.tmp_vals);
        self.tmp_vals += 1;
        self.var_map.push(tmp);
        VarId(self.var_map.len() - 1)
    }
    
    pub fn get_var(&self, var: VarId) -> &Var<'a> {
        self.var_map.get(var.0).unwrap()
    }

    pub fn create_node<T>(&mut self, source: Source<'a>, spanned: Spanned<T>) -> Node<T>{
        let node = NodeId(self.nodes.len());
        self.nodes.push((spanned.span, source));
        Node(spanned.val, node)
    }

    pub fn report_error(&mut self, error: ErrorNode<'a>){
        self.errors.push(error);
    }
    
    pub fn get_node_source(&self, arg: NodeId) -> (Span, Source<'a>) {
        self.nodes[arg.0]
    }
    
    pub fn print_errors(&self) {
       for error in &self.errors{
            println!("{error}")
       } 
    }
}

#[macro_export]
macro_rules! error {
    ($into:expr, $fmt:literal, $args:tt) => {{
        let args = $crate::util::error::FormatableError{
            _args: format_args!($fmt, $args),
        };
        $into.report_error(args);
    }};
}