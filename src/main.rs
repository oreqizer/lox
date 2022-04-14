use std::io::Write;

use lox::{lexer::Token, parser::Expr, parser::Stmt, Interpreter, Lexer, Parser};

fn run_lexer(src: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(src);
    let (tokens, errors) = lexer.scan_tokens();

    for e in &errors {
        println!("{}", e.format(src));
    }

    tokens
}

fn run_parse_expr(tokens: &[Token]) -> Option<Expr> {
    let mut parser = Parser::new(&tokens);
    match parser.parse_expr() {
        Ok(e) => Some(e),
        _ => None,
    }
}

fn run_parse(src: &str, tokens: &[Token]) -> Vec<Stmt> {
    let mut parser = Parser::new(&tokens);
    let (stmts, errors) = parser.parse();

    for e in &errors {
        println!("{}", e.format(src));
    }

    stmts
}

fn repl() {
    let mut interpreter = Interpreter::new();

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        line.pop(); // pops \n

        if &line == ":q" {
            return;
        }

        let tokens = run_lexer(&line);
        if let Some(e) = run_parse_expr(&tokens) {
            match interpreter.eval(&e) {
                Ok(v) => println!("{}", v),
                Err(e) => println!("{}", e.format(&line)),
            }
            continue;
        }

        let stmts = run_parse(&line, &tokens);
        match interpreter.interpret(&stmts) {
            Ok(_) => continue,
            Err(e) => println!("{}", e.format(&line)),
        }
    }
}

fn main() {
    let mut args = std::env::args();
    if args.len() > 2 {
        println!("Usage: lox [script]");
        std::process::exit(64);
    }

    if let Some(file) = args.nth(1) {
        let src = std::fs::read_to_string(file).expect("failed to read source file");

        let mut interpreter = Interpreter::new();
        let tokens = run_lexer(&src);
        let stmts = run_parse(&src, &tokens);

        if let Err(e) = interpreter.interpret(&stmts) {
            println!("{}", e.format(&src));
            std::process::exit(65); // TODO also runtime errors code 70
        }
    } else {
        repl();
    }
}
