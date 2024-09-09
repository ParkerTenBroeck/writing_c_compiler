use std::path::PathBuf;

use clap::{arg, command, Parser};

pub mod util;
pub mod lex;
pub mod parser;
pub mod semanitc;
pub mod tacky;
pub mod tacky_opt;
pub mod code_gen;
pub mod code_emit;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input file to compile
    #[arg(short, long)]
    input: PathBuf,

    #[clap(subcommand)]
    mode: Option<Mode>,

    #[arg(short, long)]
    ops: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, clap::Subcommand)]
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
    // let cli = Cli{
    //     input: "./tests/test.c".into(),
    //     mode: None,
    // };

    let input = cli.input;
    let mut output = input.clone();
    output.set_extension(".s");

    let pre_processed = std::process::Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(input)
        .arg("-o-")
        .output()
        .unwrap()
        .stdout;
    let pre_processed = String::from_utf8(pre_processed).unwrap();
    let input = pre_processed;

    let mut info = util::info::CompilerInfo::new();

    // lexing stage
    let lexer = lex::Lexer::new(&input);
    if cli.mode == Some(Mode::Lex) {
        let mut error = false;
        for tok in lexer {
            error |= tok.is_err();
            match tok {
                Ok(_) => todo!(),
                Err(err) => println!("{err:#?}"),
            }
        }
        if error {
            return Err(());
        } else {
            return Ok(());
        }
    };

    // parsing stage
    let ast = parser::Parser::new(lexer).parse();
    let mut ast = match ast {
        Ok(program) => program,
        Err(errors) => return Err(println!("{errors:#?}")),
    };
    if cli.mode == Some(Mode::Parse) {
        return Ok(());
    }

    // semantic analysis & variable resolution
    if let Err(errors) = semanitc::SemanticAnalysis::new(&mut info).resolve_pass(&mut ast) {
        return Err(println!("{errors:#?}"));
    }
    if cli.mode == Some(Mode::Verify) {
        return Ok(());
    }

    // intermediate representation generation
    let mut tacky = tacky::TackyGen::new(&mut info).ast_to_tacky(ast);
    if cli.mode == Some(Mode::Tacky) {
        return Ok(());
    }

    if cli.ops{
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
