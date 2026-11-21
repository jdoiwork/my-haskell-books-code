// The chapter on learning from other lands (MiniRegex): ask Rust's
// regex-automata which strategy its meta engine chose for each of the
// guild's jobs. With the "logging" feature it says so at the debug
// level; a small logger keeps only those lines.
// よその国に学ぶ章（MiniRegex）：Rust の regex-automata の meta エンジンが、
// ギルドの仕事ごとに、どの手を選んだかを聞く。"logging" の機能を入れると、
// debug の段で教えてくれる。小さなロガーで、その行だけを残す

// ANCHOR: strategy
use log::{Level, LevelFilter, Log, Metadata, Record};
use regex_automata::meta::Regex;

// 📣 Print only the lines that say which strategy was chosen.
// どの手を選んだかの行だけを出す
struct Strategies;

impl Log for Strategies {
    fn enabled(&self, m: &Metadata) -> bool {
        m.level() <= Level::Debug
    }
    fn log(&self, r: &Record) {
        let line = r.args().to_string();
        if line.starts_with("using") && line.ends_with("strategy") {
            println!("  {line}");
        }
    }
    fn flush(&self) {}
}

static LOGGER: Strategies = Strategies;

fn main() {
    log::set_logger(&LOGGER).unwrap();
    log::set_max_level(LevelFilter::Debug);
    let jobs = [
        ("dates", r"\d{4}-\d{2}-\d{2}"),
        ("mail", r"[\w.+-]+@[\w-]+\.[\w.-]+"),
        ("names", "Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go"),
        ("caseless", "(?i)perl"),
        ("sales", r"(\w+) sold (\d+) (\w+)"),
    ];
    for (name, pattern) in jobs {
        println!("{name}");
        let re = Regex::new(pattern).unwrap();
        // the strategy is chosen when the regex is built
        // 手は、正規表現を作るときに決まる
        assert!(re.is_match("2026-11-21 Mira sold 3 herbs <mira@guild.example> perl"));
    }
}
// ANCHOR_END: strategy
