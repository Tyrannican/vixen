mod lex;
mod parse;

use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
struct Vixen {
    #[clap(subcommand)]
    command: VixenCommand,
}

#[derive(Subcommand, Debug)]
enum VixenCommand {
    Lex { filename: String },
    Parse { filename: String },
}

fn main() -> Result<(), miette::Error> {
    let args = Vixen::parse();

    match args.command {
        VixenCommand::Lex { filename } => {
            let contents = std::fs::read_to_string(filename).expect("ahh");
            let mut lexer = lex::Lexer::new(&contents);

            while let Some(tkn) = lexer.next() {
                let tkn = tkn?;
                println!("{tkn}");
            }
        }
        VixenCommand::Parse { filename } => {
            let contents = std::fs::read_to_string(filename).expect("ahh");
            let mut parser = parse::Parser::new(&contents);
        }
    }

    Ok(())
}
