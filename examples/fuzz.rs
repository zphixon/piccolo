extern crate piccolo;

fn main() {
    #[cfg(feature = "fuzzer")]
    {
        let args: Vec<String> = std::env::args().collect();
        let n = if args.len() == 1 {
            50
        } else {
            args[1].parse::<usize>().unwrap_or_else(|_| {
                println!("expected number, got {}", args[1]);
                std::process::exit(1);
            })
        };
        piccolo::fuzzer::fuzz(n);
    }
    #[cfg(not(feature = "fuzzer"))]
    {
        compile_error!("fuzzer requires fuzzer feature");
    }
}
