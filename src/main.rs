use std::path::PathBuf;

use clap::{arg, command, Parser};
use parser::ast;
use util::info;

pub mod util;
// pub mod code_emit;
// pub mod code_gen;
pub mod lex;
pub mod parser;
// pub mod semanitc;
// pub mod tacky;
// pub mod tacky_opt;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input file to compile
    #[arg(short, long)]
    input: PathBuf,

    #[arg(short, long)]
    mode: Option<Mode>,

    #[arg(short, long)]
    ops: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum)]
enum Mode {
    /// run the lexer, but stop before parsing
    Lex,
    /// run the lexer and parser, but stop before semantic analysis
    Parse,
    /// run the lexer, parser, and semantic analysis, but stop before tacky generation]
    Verify,
    /// run the lexer, parser, semantic analysis, and tacky, but stop before code generation
    Tacky,
    /// run the lexer, parser, semantic analysis, tacky, and codegen, but stop before assembly emission
    Codegen,
}

fn main() -> Result<(), ()> {
    // let cli = Cli::parse();
    let cli = Cli{
        input: "./tests/test.rc".into(),
        ops: false,
        mode: None,
    };

    let input_path = cli.input;
    let mut output = input_path.clone();
    output.set_extension(".s");

    let pre_processed = std::fs::read_to_string(&input_path).unwrap();
    let input = pre_processed;

    let mut info = util::info::CompilerInfo::new();
    let source = util::info::Source {
        path: input_path.to_str().unwrap_or(""),
        contents: &input,
    };

    // lexing stage
    let lexer = lex::Lexer::new(&input);
    if cli.mode == Some(Mode::Lex) {
        let mut error = false;
        for tok in lexer {
            error |= tok.is_err();
            match tok {
                Ok(_) => {}
                Err(err) => {
                    let node = info.create_node(source, *err);
                    info.report_error(node.error(&info, format!("{:?}", node.0)));
                }
            }
        }
        if error {
            info.print_errors();
            return Err(());
        } else {
            return Ok(());
        }
    };

    // parsing stage
    let ast = parser::Parser::new(source, &mut info).parse();
    let mut ast = match ast {
        Ok(program) => program,
        Err(_) => {
            info.print_errors();
            return Err(());
        }
    };
    Test{info: &mut info}.recurse(ast);
    info.print_errors();
    struct Test<'a, 'b>{
        info: &'b mut info::CompilerInfo<'a>
    }
    impl<'a, 'b> Test<'a, 'b>{
        pub fn recurse(&mut self, program: ast::Program<'a>) {
            for item in program.0{
                self.info.report_error(item.error(self.info, "top".into()));
                
                let ast::TopLevel::FunctionDef(func) = item.0;
                self.info.report_error(func.body.error(self.info, "func".into()));
                for item in func.body.0{
                    self.info.report_error(item.error(self.info, "block".into()));
                    match item.0{
                        ast::BlockItem::Statement(stmt) => {
                            match stmt{
                                ast::Statement::Return(expr) => self.expr(expr),
                                ast::Statement::Expression(expr) => self.expr(expr),
                                ast::Statement::Empty => {},
                            }
                        },
                        ast::BlockItem::Declaration(decl) => {
                            self.path(decl.name);
                            if let Some(expr) = decl.expr{
                                self.expr(expr)
                            }
                        },
                    }
                }
                // func.body
            }
        }
        fn expr(&mut self, expr: info::Node<ast::Expr<'a>>){
            self.info.report_error(expr.error(self.info, "expr".into()));
            match expr.0{
                ast::Expr::Constant(_) => {},
                ast::Expr::Unary(op, expr) => {
                    self.info.report_error(op.error(self.info, "op".into()));
                    self.expr(*expr);
                },
                ast::Expr::Binary { op, lhs, rhs } => {
                    self.info.report_error(op.error(self.info, "op".into()));
                    self.expr(*lhs);
                    self.expr(*rhs);
                },
                ast::Expr::Ident(_) => {},
            }
        }
        fn path(&mut self, path: info::Node<ast::Path<'a>>){
            self.info.report_error(path.error(self.info, "node".into()));
        }
    }

    if cli.mode == Some(Mode::Parse) {
        return Ok(());
    }
    Ok(())

    // semantic analysis & variable resolution
    // if let Err(errors) = semanitc::SemanticAnalysis::new(&mut info).resolve_pass(&mut ast) {
    //     return Err(println!("{errors:#?}"));
    // }
    // if cli.mode == Some(Mode::Verify) {
    //     return Ok(());
    // }

    // // intermediate representation generation
    // let mut tacky = tacky::TackyGen::new(&mut info).ast_to_tacky(ast);
    // if cli.mode == Some(Mode::Tacky) {
    //     return Ok(());
    // }

    // if cli.ops {
    //     tacky_opt::run_opts(&mut tacky);
    // }

    // // code generation
    // let code = code_gen::code_gen(&mut info, tacky);
    // if cli.mode == Some(Mode::Codegen) {
    //     return Ok(());
    // }

    // // code emission
    // let mut out = String::new();
    // code_emit::AsmEmission::new(&mut out, &mut info)
    //     .emit_asm(code)
    //     .unwrap();
    // std::fs::write(&output, out).unwrap();
    // std::process::Command::new("gcc")
    //     .arg(output)
    //     .status()
    //     .unwrap();
    // Ok(())
}


