use ariadne::{Color, Fmt};
use clap::Parser;
use foxglove::{run, Config};
use std::path::PathBuf;

fn main() {
    let args = Args::parse();

    let config = args.into();

    match run(config) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("{}: {}", "Error".fg(Color::Red), e);
            std::process::exit(1);
        }
    }
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[command(arg_required_else_help(true))]
/// Compiler for the foxglove programming language
struct Args {
    filename: PathBuf,

    #[clap(short, long)]
    tokens: bool,

    #[clap(short, long)]
    ast: bool,

    #[clap(short = 'H', long)]
    hir: bool,
}

impl From<Args> for Config {
    fn from(args: Args) -> Self {
        Self::new(args.filename, args.tokens, args.ast, args.hir)
    }
}
