#[derive(Debug)]
pub enum Var<'a>{
    Local(usize),
    Global(&'a str),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LabelId(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct VarId(pub usize);

#[derive(Debug, Default)]
pub struct CompilerInfo<'a>{
    pub func: Functioninfo,
    
    var_map: Vec<Var<'a>>,

    tmp_labels: usize,
}

#[derive(Debug, Default)]
pub struct Functioninfo{
    temporaries: usize,
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
        self.var_map.push(self.func.next_tmp_var());
        VarId(self.var_map.len() - 1)
    }
    
    pub fn get_var(&self, var: VarId) -> &Var<'a> {
        self.var_map.get(var.0).unwrap()
    }
}

impl Functioninfo{
    fn next_tmp_var<'a>(&mut self) -> Var<'a> {
        self.temporaries += 1;
        Var::Local(self.temporaries - 1)
    }

    pub fn get_temporaries(&self) -> usize{
        self.temporaries
    }
}