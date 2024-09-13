use std::path::PathBuf;

use clap::{arg, command, Parser};

pub mod code_emit;
pub mod code_gen;
pub mod lex;
pub mod parser;
pub mod semanitc;
pub mod tacky;
pub mod tacky_opt;
pub mod util;

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
    let cli = Cli::parse();

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
    let mut ast = parser::Parser::new(source, &mut info).parse();
    if info.has_errors() {
        info.print_errors();
        return Err(());
    }

    if cli.mode == Some(Mode::Parse) {
        return Ok(());
    }

    semanitc::SemanticAnalysis::new(&mut info).resolve_pass(&mut ast);
    if info.has_errors() {
        info.print_errors();
        return Err(());
    }
    if cli.mode == Some(Mode::Verify) {
        return Ok(());
    }

    // intermediate representation generation
    let mut tacky = tacky::TackyGen::new(&mut info).ast_to_tacky(ast);
    if cli.mode == Some(Mode::Tacky) {
        return Ok(());
    }

    if cli.ops {
        tacky_opt::run_opts(&mut tacky);
    }

    // code generation
    let code = code_gen::code_gen(&mut info, tacky);
    if cli.mode == Some(Mode::Codegen) {
        return Ok(());
    }

    // code emission
    let mut out = String::new();
    code_emit::AsmEmission::new(&mut out, &mut info)
        .emit_asm(code)
        .unwrap();
    std::fs::write(&output, out).unwrap();
    std::process::Command::new("gcc")
        .arg(output)
        .status()
        .unwrap();
    Ok(())
}
