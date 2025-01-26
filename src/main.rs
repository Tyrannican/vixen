mod lex;

fn main() -> Result<(), miette::Error> {
    let contents = std::fs::read_to_string("./test.vx").expect("ahh");
    let mut lexer = lex::Lexer::new(&contents);

    while let Some(tkn) = lexer.next() {
        let tkn = tkn?;
        println!("{tkn}");
    }

    Ok(())
}
