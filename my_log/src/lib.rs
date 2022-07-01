use std::{
    str::FromStr,
    sync::atomic::{AtomicBool, Ordering},
    sync::RwLock,
};

use fnv::FnvHashMap;
use log::{Level, LevelFilter, Log, Metadata, Record};
use once_cell::sync::Lazy;
use regex::Regex;

#[derive(Clone, Debug)]
struct LogConfig {
    all: Option<Level>,
    specific: FnvHashMap<String, Level>,
}

impl Default for LogConfig {
    fn default() -> Self {
        LogConfig {
            all: None,
            specific: FnvHashMap::with_capacity_and_hasher(0, Default::default()),
        }
    }
}

struct SimpleLogger {
    inner: RwLock<LogConfig>,
    enabled: AtomicBool,
}

impl SimpleLogger {
    fn new() -> Self {
        Self {
            inner: RwLock::new(Default::default()),
            enabled: false.into(),
        }
    }

    fn set_config(&self, config: LogConfig) {
        let mut inner = self.inner.write().expect("could not acquire write lock");
        *inner = config;
    }

    fn get(&self, crate_: &str) -> Option<Level> {
        let inner = self.inner.read().expect("could not acquire read lock");
        inner.specific.get(crate_).cloned()
    }
}

fn color_with_level(s: &str, level: Level) -> String {
    use owo_colors::OwoColorize;
    match level {
        Level::Error => s.black().on_bright_red().to_string(),
        Level::Warn => s.bright_yellow().to_string(),
        Level::Info => s.cyan().to_string(),
        Level::Debug => s.blue().to_string(),
        Level::Trace => s.green().dimmed().to_string(),
    }
}

const CRATE: &'static str = "([a-z_]+)(?:::.*)?";
static CRATE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(CRATE).unwrap());

impl Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        if !self.enabled.load(Ordering::Relaxed) {
            return false;
        }

        let inner = self.inner.read().expect("could not acquire read lock");

        if let Some(all) = inner.all {
            all >= metadata.level()
        } else {
            CRATE_REGEX
                .captures(metadata.target())
                .and_then(|capture| capture.get(1))
                .and_then(|crate_| self.get(crate_.as_str()))
                .map(|level| level >= metadata.level())
                .unwrap_or(false)
        }
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            let mut level = format!("{:5}", record.level());

            let mut module = record
                .module_path()
                .map(|module| format!("[{module}]"))
                .unwrap_or("(unknown)".to_string());

            if atty::is(atty::Stream::Stdout) {
                level = color_with_level(&level, record.level());
                module = color_with_level(&module, record.level());
            }

            println!("{level} {module} {}", record.args());
        }
    }

    fn flush(&self) {}
}

const ENV_TERM: &'static str = "([a-z_\\-]+)";

fn make_multi_regex() -> Regex {
    let regex = format!("{ENV_TERM}(?:={ENV_TERM})?");
    Regex::new(&regex).unwrap()
}

fn make_single_regex() -> Regex {
    let regex = format!("^{ENV_TERM}$");
    Regex::new(&regex).unwrap()
}

fn make_config(env: &str) -> LogConfig {
    if make_single_regex().is_match(env) {
        LogConfig {
            all: Some(Level::from_str(env).expect(&format!("invalid log level {env}"))),
            specific: Default::default(),
        }
    } else {
        let mut specific = FnvHashMap::default();
        make_multi_regex().captures_iter(env).for_each(|captures| {
            match (captures.get(1), captures.get(2)) {
                (Some(crate_capture), Some(level_capture)) => {
                    specific.insert(
                        crate_capture.as_str().to_owned(),
                        Level::from_str(level_capture.as_str())
                            .expect(&format!("unknown level {}", level_capture.as_str())),
                    );
                }
                _ => {}
            }
        });

        LogConfig {
            all: None,
            specific,
        }
    }
}

static LOGGER: Lazy<SimpleLogger> = Lazy::new(|| SimpleLogger::new());

pub fn init() {
    if let Ok(log) = std::env::var("RUST_LOG") {
        LOGGER.set_config(make_config(&log));
        LOGGER.enabled.store(true, Ordering::SeqCst);
    }

    log::set_logger(&*LOGGER)
        .map(|()| log::set_max_level(LevelFilter::Trace))
        .expect("log::set_logger failed");
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_matches() {
        dbg!(make_config(""));
        dbg!(make_config("trace"));
        dbg!(make_config("piccolo=debug"));
        dbg!(make_config("piccolo=debug,"));
        dbg!(make_config(",rustyline=debug"));
        dbg!(make_config("piccolo=,rustyline=debug"));
        dbg!(make_config("piccolo=trace,rustyline=warn,main=info"));
        dbg!(make_config("piccolo=debug,rustylin===="));
    }
}
