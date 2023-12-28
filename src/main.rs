use anyhow::Result;
use clap::{Parser, Subcommand, ValueEnum};
use gen::CodeGen;

use crate::parser::Parser as CParser;
use crate::{lexer::Lexer, token::TokenKind};

pub mod ast;
pub mod gen;
pub mod lexer;
pub mod parser;
pub mod token;

fn main() -> Result<()> {
    let Args {
        command,
        global_opts,
    } = Args::parse();

    match command {
        Command::Run { file, output } => {
            let asm = assemble(&file, &global_opts)?;

            std::fs::write(&output, asm)?;

            build_executable(&output);
            run_executable();

            Ok(())
        }
    }
}

fn run_executable() {
    println!("\nRunning executable...\n\n");

    let output = std::process::Command::new("./out").output().unwrap();
    println!("{}", String::from_utf8_lossy(&output.stdout));
}

fn build_executable(output: &str) {
    std::process::Command::new("cc")
        .args(["-o", "out", output])
        .output()
        .unwrap();
}

fn assemble(file: &str, global_opts: &GlobalOpts) -> Result<String> {
    let path = file;

    let Ok(content) = std::fs::read_to_string(path) else {
        eprintln!("Could not open file {}.", path);
        std::process::exit(74);
    };

    if global_opts.has_emit(Emit::Src) {
        println!("==Source==\n\n{}\n", &content);
    }

    if global_opts.has_emit(Emit::Tokens) {
        emit_tokens(&content);
    }

    let mut parser = CParser::new(&content);

    let ast = parser.parse()?;

    if global_opts.has_emit(Emit::Ast) {
        println!("== Ast ==\n\n{:#?}\n", ast);
    }

    let out = CodeGen::new().gen(&ast);

    if global_opts.has_emit(Emit::Asm) {
        println!("== Assembly ==\n\n{}\n", out);
    }

    Ok(out)
}

fn emit_tokens(source: &str) {
    println!("== Tokens ==\n");

    let mut lexer = Lexer::new(source);

    loop {
        let token = lexer.next_token();
        println!("{:?}", token);

        if token.is_kind(TokenKind::Eof) {
            break;
        }
    }

    println!();
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Command to execute
    #[command(subcommand)]
    command: Command,

    #[command(flatten)]
    global_opts: GlobalOpts,
}

#[derive(clap::Args, Debug, Clone)]
struct GlobalOpts {
    /// Debug mode
    #[arg(short, long, global = true)]
    debug: bool,
    /// Emit intermediaries to stdout
    #[arg(short, long, value_delimiter = ',', global = true)]
    emit: Option<Vec<Emit>>,
}

impl GlobalOpts {
    pub fn has_emit(&self, emit: Emit) -> bool {
        if let Some(emits) = &self.emit {
            return emits.contains(&emit);
        }
        false
    }
}

#[derive(Debug, Subcommand)]
enum Command {
    Run {
        /// Input file
        file: String,
        /// Output file
        #[arg(short, long)]
        output: String,
    },
}

#[derive(Debug, Clone, ValueEnum, PartialEq, PartialOrd)]
enum Emit {
    Src,
    Tokens,
    Ast,
    Asm,
}
