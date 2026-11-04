// The clap code in the CLI subcommands chapter.
// サブコマンドのある CLI ツールの章の clap のコード
#[derive(Clone, Debug, clap::ValueEnum)]
enum Element { Fire, Water, Grass }
// ANCHOR: clap_derive
// Rust (clap, derive)
// Rust（clap の derive）
use clap::{Parser, Subcommand};

#[derive(Parser)]
struct Cli {
    #[arg(short, long, default_value = "monsters.json")]
    file: String,
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    List {
        #[arg(short, long)]
        element: Option<Element>,
        #[arg(long)]
        json: bool,
    },
    // Add { ... }, Remove { ... }
}
// ANCHOR_END: clap_derive
fn main() {
let cli = Cli::parse();
let Command::List { element, json } = cli.command;
println!("{} {:?} {}", cli.file, element, json);
}
