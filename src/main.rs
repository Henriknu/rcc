use anyhow::Result;
use clap::{Parser, Subcommand, ValueEnum};

use crate::{lexer::Lexer, token::TokenKind};

pub mod lexer;
pub mod token;

fn main() -> Result<()> {
    let Args {
        command,
        global_opts,
    } = Args::parse();

    match command {
        Command::Run {
            file,
            output: _output,
        } => {
            assemble(&file, &global_opts)?;
        }
    }

    Ok(())
}

fn assemble(file: &str, global_opts: &GlobalOpts) -> Result<()> {
    let path = file;

    let Ok(content) = std::fs::read_to_string(path) else {
        eprintln!("Could not open file {}.", path);
        std::process::exit(74);
    };

    if global_opts.has_emit(Emit::Tokens) {
        emit_tokens(&content);
    }

    Ok(())
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
    Tokens,
    Ast,
    Asm,
}
