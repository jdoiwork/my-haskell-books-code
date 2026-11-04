// The clap code in the CLI tool chapter.
// CLI ツールを作る章の clap のコード
#[derive(Clone, Debug, clap::ValueEnum)]
enum Element { Fire, Water, Grass }
fn main() {
    // ANCHOR: clap_derive
    // Rust (clap, derive)
    // Rust（clap の derive）
    use clap::Parser;

    #[derive(Parser)]
    struct Cli {
        #[arg(short, long, default_value = "monsters.json")]
        file: String,
        #[arg(short, long)]
        element: Option<Element>,
        #[arg(long, default_value = "name")]
        sort: String,
        #[arg(long)]
        json: bool,
    }

    let cli = Cli::parse();
    // ANCHOR_END: clap_derive
println!("{} {:?} {} {}", cli.file, cli.element, cli.sort, cli.json);
}
