use std::io::Write;

use lox::{Error, Lexer};

// TODO replace () with eval value
fn run(src: String) -> Result<(), Vec<Error>> {
    let mut lexer = Lexer::new();
    let (tokens, errors) = lexer.scan_tokens(src);

    for token in tokens {
        println!("{:?}", token);
    }

    if errors.len() > 0 {
        Err(errors)
    } else {
        Ok(())
    }
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
            Err(e) => {
                for (line, err) in e {
                    error(line, "", &err);
                }
            }
        }
    }
}

fn main() {
    let mut args = std::env::args();
    if args.len() > 2 {
        println!("Usage: Usage: rlox [script]");
        std::process::exit(64);
    }

    if let Some(file) = args.nth(1) {
        let src = std::fs::read_to_string(file).expect("failed to read source file");

        if let Err(e) = run(src) {
            for (line, err) in e {
                error(line, "", &err);
            }
            std::process::exit(65);
        }
    } else {
        repl();
    }
}
