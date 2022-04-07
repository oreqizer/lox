use std::io::Write;

use lox::{Lexer, Parser, Interpreter, Stmt, Value};

fn run(src: &str) -> Vec<Stmt> {
    let mut lexer = Lexer::new(src);
    let (tokens, errors) = lexer.scan_tokens();

    for e in &errors {
        println!("{}", e.format(src));
    }

    let mut parser = Parser::new(&tokens);
    let (stmts, errors) = parser.parse();

    for e in &errors {
        println!("{}", e.format(src));
    }
    
    stmts
}

fn repl() {
    let interpreter = Interpreter::new();

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        line.pop(); // pops \n

        if &line == ":q" {
            return;
        }

        let stmts = run(&line);
        for stmt in &stmts {
            match interpreter.execute(stmt) {
                Ok(Some(v)) => println!("{}", v),
                Ok(None) => continue,
                Err(e) => println!("{}", e.format(&line)),
            }
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

        let interpreter = Interpreter::new();
        let stmts = run(&src);

        if let Err(e) = interpreter.interpret(&stmts) {
            println!("{}", e.format(&src));
            std::process::exit(65); // TODO also runtime errors code 70
        }
    } else {
        repl();
    }
}
