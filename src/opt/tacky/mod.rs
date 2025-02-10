use crate::tacky;

pub mod constant_folding;

pub fn run_opts(program: &mut tacky::ast::Program<'_>) {
    let mut run = true;
    while run {
        run = false;
        run |= constant_folding::ConstantFolding::new()
            .run(program)
            .changed()
    }
}
