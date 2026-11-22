// The clap code of the chapter "Making a CLI the easy way" (Lam): a type
// of its own, read with FromStr (as Lam reads it with FromArg).
// 「CLI を手軽に作る」の章（Lam）の clap のコード：自分の型を FromStr で
// 読む（Lam が FromArg で読むのと同じ）
// ANCHOR: level
// Rust (clap, with FromStr)
// Rust（clap と FromStr）
use clap::Parser;
use std::str::FromStr;

#[derive(Clone, Debug, PartialEq)]
enum Level {
    Quiet,
    Normal,
    Loud,
}

// quiet, normal or loud (Loud and LOUD too) on the command line.
// コマンドラインでは quiet・normal・loud（Loud や LOUD も）
impl FromStr for Level {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "quiet" => Ok(Level::Quiet),
            "normal" => Ok(Level::Normal),
            "loud" => Ok(Level::Loud),
            _ => Err("quiet, normal or loud".to_string()),
        }
    }
}

/// Say what to remember.
#[derive(Parser)]
struct Remind {
    /// What to remember
    what: String,
    /// How loud: quiet, normal or loud
    #[arg(short, long, default_value = "normal")]
    level: Level,
}
// ANCHOR_END: level

fn main() {
    let r = Remind::parse();
    match r.level {
        Level::Quiet => println!("({})", r.what),
        Level::Normal => println!("{}", r.what),
        Level::Loud => println!("{}!", r.what.to_uppercase()),
    }
}
