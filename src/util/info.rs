use std::collections::HashMap;



#[derive(Debug, Default)]
pub struct CompilerInfo<'a>{
    pub func: Functioninfo<'a>,

    tmp_variable: usize,
    tmp_label: usize,
}

#[derive(Debug, Default)]
pub struct Functioninfo<'a>{
    pub stack_size: usize,

    var_map: HashMap<&'a str, Var>
}

impl<'a> CompilerInfo<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next_tmp_var(&mut self) -> Var {
        self.tmp_variable += 1;
        Var(self.tmp_variable - 1)
    }

    pub fn next_tmp_label(&mut self) -> Label {
        self.tmp_label += 1;
        Label(self.tmp_label - 1)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Label(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Var(pub usize);