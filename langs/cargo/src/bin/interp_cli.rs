// The clap code of the chapter "Making a CLI the easy way" (Lam).
// 「CLI を手軽に作る」の章（Lam）の clap のコード
// ANCHOR: clap
// Rust (clap, derive)
// Rust（clap の derive）
use clap::Parser;

/// Greet someone, as many times as you like.
#[derive(Parser)]
struct Options {
    /// Who to greet
    name: String,
    /// How many times
    #[arg(short = 'n', long, default_value_t = 1)]
    count: u32,
    /// Shout it
    #[arg(long)]
    loud: bool,
}

fn main() {
    let opts = Options::parse();
    let line = format!("Hello, {}!", opts.name);
    for _ in 0..opts.count {
        if opts.loud {
            println!("{}", line.to_uppercase());
        } else {
            println!("{line}");
        }
    }
}
// ANCHOR_END: clap
