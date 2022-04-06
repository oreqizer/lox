use std::io::Write;

use lox::{Lexer, Parser, Interpreter};

// TODO make proper error handling here
fn run(src: String) -> Option<()> {
    let mut lexer = Lexer::new(&src);
    let (tokens, errors) = lexer.scan_tokens();

    for e in &errors {
        println!("{}", e.format(&src));
    }

    let mut parser = Parser::new(&tokens);
    match parser.parse() {
        Ok(e) => match Interpreter::interpret(&e) {
            Ok(res) => { println!("{}", res); Some(()) },
            Err(e) => { println!("{}", e.format(&src)); None },
        },
        Err(errs) => {
            for e in &errs {
                println!("{}", e.format(&src));
            }

            None
        }
    }
}

fn repl() {
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        line.pop(); // pops \n

        if &line == ":q" {
            return;
        }

        run(line);
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

        if let None = run(src) {
            std::process::exit(65);
        }
    } else {
        repl();
    }
}
