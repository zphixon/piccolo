extern crate clap;
extern crate piccolo;

use clap::{App, Arg, SubCommand};

fn main() {
    let _ = run();
}

fn run() -> Result<(), ()> {
    #[cfg(feature = "fuzzer")]
    {
        let matches = App::new("Piccolo fuzzer")
            .arg(
                Arg::with_name("max")
                    .help("exclusive max number of tokens (default 50)")
                    .short("m")
                    .long("max")
                    .takes_value(true),
            )
            .arg(
                Arg::with_name("min")
                    .help("inclusive min number of tokens (default 5)")
                    .short("n")
                    .long("min")
                    .takes_value(true),
            )
            .arg(
                Arg::with_name("runs")
                    .help("number of runs (default 100)")
                    .index(1),
            )
            .get_matches();

        let runs = if matches.is_present("runs") {
            matches
                .value_of("runs")
                .unwrap()
                .parse::<usize>()
                .map_err(|_| {
                    println!("Expected number");
                })?
        } else {
            100
        };

        let min = if matches.is_present("min") {
            matches
                .value_of("min")
                .unwrap()
                .parse::<usize>()
                .map_err(|_| {
                    println!("Expected number for min");
                })?
        } else {
            5
        };

        let max = if matches.is_present("max") {
            matches
                .value_of("max")
                .unwrap()
                .parse::<usize>()
                .map_err(|_| {
                    println!("Expected number for max");
                })?
        } else {
            50
        };

        if min < max {
            piccolo::fuzzer::fuzz(runs, min, max);
            Ok(())
        } else {
            println!("Expected min < max");
            Err(())
        }
    }
    #[cfg(not(feature = "fuzzer"))]
    {
        println!("Fuzzer requires piccolo to be built with --features 'fuzzer'");
        Err(())
    }
}
