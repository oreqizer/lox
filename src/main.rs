fn main() {
    let args = std::env::args();
    if args.len() > 1 {
        println!("Usage: Usage: rlox [script]");
        std::process::exit(64);
    }

    if args.len() == 1 {
        // run from file
    }

    // run REPL
}
