use std::io::Write;

use lox::{Lexer, Parser};

fn run(src: String) -> Result<(), String> {
    let mut lexer = Lexer::new(&src);
    let (tokens, errors) = lexer.scan_tokens();

    if errors.len() > 0 {
        // TODO properly handle errors:
        // 1. create a global "Error" type with line information and string
        // 2. print errors as soon as lexing is done
        // 3. call parser after lexing, overwriting errors
        // 4. print further errors after parsing is done
        return Err(errors
            .iter()
            .map(|(line, text)| format!("Line {} | {}", line, text))
            .reduce(|acc, next| format!("{}\n{}", acc, next))
            .unwrap());
    }

    for t in &tokens {
        println!("{:?}", t);
    }

    let mut parser = Parser::new(&tokens);
    let expr = parser.parse()?;

    println!("{}", expr);

    Ok(())
}

fn error(line: u32, loc: &str, msg: &str) {
    eprintln!("[line {}] Error {}: {}", line, loc, msg);
}

fn repl() {
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        if &line == ":q\n" {
            return;
        }

        match run(line) {
            Ok(()) => println!("Ok"),
            Err(ref e) => println!("Syntax error: {}", e),
        }
    }
}

fn main() {
    let mut args = std::env::args();
    if args.len() > 2 {
        println!("Usage: Usage: lox [script]");
        std::process::exit(64);
    }

    if let Some(file) = args.nth(1) {
        let src = std::fs::read_to_string(file).expect("failed to read source file");

        if let Err(ref e) = run(src) {
            println!("Syntax error: {}", e);
            std::process::exit(65);
        }
    } else {
        repl();
    }
}
