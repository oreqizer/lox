use std::{cell::RefCell, io::Write, rc::Rc};

use lox_walker::{
    interpreter::Resolver, lexer::Token, parser::Expr, parser::Stmt, Interpreter, Lexer, Parser,
};

fn run_lexer(src: &str) -> Option<Vec<Token>> {
    let mut lexer = Lexer::new(src);
    let (tokens, errors) = lexer.scan_tokens();

    for e in &errors {
        println!("{}", e.format(src));
    }

    if errors.len() == 0 {
        Some(tokens)
    } else {
        None
    }
}

fn run_parse_expr(tokens: &[Token]) -> Option<Expr> {
    let mut parser = Parser::new(&tokens);
    match parser.parse_expr() {
        Ok(e) => Some(e),
        _ => None,
    }
}

fn run_parse(src: &str, tokens: &[Token]) -> Option<Vec<Stmt>> {
    let mut parser = Parser::new(&tokens);
    let (stmts, errors) = parser.parse();

    for e in &errors {
        println!("{}", e.format(src));
    }

    if errors.len() == 0 {
        Some(stmts)
    } else {
        None
    }
}

fn repl() {
    let interpreter = &Rc::new(RefCell::new(Interpreter::new()));
    let mut resolver = Resolver::new(&interpreter);

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        line.pop(); // pops \n

        if &line == ":q" {
            return;
        }

        let tokens = match run_lexer(&line) {
            Some(v) => v,
            None => continue,
        };

        if let Some(e) = run_parse_expr(&tokens) {
            match interpreter.borrow_mut().eval(&e) {
                Ok(v) => println!("{}", v),
                Err(e) => println!("{}", e.format(&line)),
            }
            continue;
        }

        let stmts = match run_parse(&line, &tokens) {
            Some(v) => v,
            None => continue,
        };

        if let Err(e) = resolver.resolve(&stmts) {
            println!("{}", e.format(&line));
            continue;
        }

        if let Err(e) = interpreter.borrow_mut().interpret(&stmts) {
            println!("{}", e.format(&line));
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

        let tokens = run_lexer(&src).unwrap_or_else(|| {
            std::process::exit(65);
        });

        let stmts = run_parse(&src, &tokens).unwrap_or_else(|| {
            std::process::exit(65);
        });

        let interpreter = Rc::new(RefCell::new(Interpreter::new()));
        let mut resolver = Resolver::new(&interpreter);

        if let Err(e) = resolver.resolve(&stmts) {
            println!("{}", e.format(&src));
            std::process::exit(65);
        }

        let mut it = interpreter.as_ref().borrow_mut();
        if let Err(e) = it.interpret(&stmts) {
            println!("{}", e.format(&src));
            std::process::exit(70);
        }
    } else {
        repl();
    }
}
