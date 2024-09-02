use std::path::PathBuf;

use clap::{arg, command, Parser, ValueEnum};

pub mod code_gen;
pub mod lex;
pub mod parser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input file to compile
    #[arg(short, long)]
    input: PathBuf,

    #[arg(short, long)]
    mode: Option<Mode>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Mode {
    /// run the lexer, but stop before parsing
    Lex,
    /// run the lexer and parser, but stop before assembly generation
    Parse,
    /// run the lexer and parser, but stop before assembly generation
    Codegen,
}

fn main() -> Result<(), ()> {
    let cli = Cli::parse();

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

    match cli.mode {
        Some(Mode::Lex) => {
            for tok in lex::Lexer::new(&input) {
                match tok {
                    Ok(_) => todo!(),
                    Err(err) => println!("{err:#?}"),
                }
            }
            return Ok(());
        }
        Some(Mode::Parse) => {
            match parser::Parser::new(&input).parse() {
                Ok(program) => println!("{program:#?}"),
                Err(errors) => return Err(println!("{errors:#?}")),
            }
            return Ok(());
        }
        Some(Mode::Codegen) => {
            match parser::Parser::new(&input).parse() {
                Ok(program) => println!("{:#?}", code_gen::gen::gen(program)),
                Err(errors) => return Err(println!("{errors:#?}")),
            }
            return Ok(());
        }
        None => {}
    }

    let res = parser::Parser::new(&input).parse();
    let program = match res {
        Ok(program) => program,
        Err(errors) => return Err(println!("{errors:#?}")),
    };
    let mut out = String::new();
    code_gen::asm_gen(&mut out, program).unwrap();
    std::fs::write(&output, out).unwrap();
    std::process::Command::new("gcc")
    .arg(output)
    .status().unwrap();
    Ok(())
}
