// The chapter on the lazy DFA (MiniRegex): Rust's lazy DFA by itself
// (regex-automata's hybrid::regex::Regex: a forward DFA for the end, a
// reverse one for the start, as in the chapter), on the guild's book of
// 2000 lines. Prints the count of each job, and how big the two caches
// grew.
// lazy DFA の章（MiniRegex）：Rust の lazy DFA だけを使う
// （regex-automata の hybrid::regex::Regex。章と同じく、前向きの DFA で
// 終わりを、逆向きの DFA で始まりを）。ギルドの帳面 2000 行で、仕事ごとの
// 件数と、2つのキャッシュがどれだけ大きくなったかを出す

const NAMES: [&str; 8] = ["Mira", "Ruby", "Lam", "GHC", "Perl", "Rust", "Python", "Go"];
const ITEMS: [&str; 5] = ["herbs", "potions", "swords", "maps", "scrolls"];

// 📒 The guild's book of sales: n lines, the same in every language.
// ギルドの売り上げの帳面：n 行。どの言語でも同じもの
fn guild_book(n: usize) -> String {
    let mut text = String::new();
    for i in 0..n {
        let seller = NAMES[i % 8];
        text.push_str(&format!(
            "2026-11-{:02} {:02}:{:02} {} sold {} {} to {} for {} gold",
            i % 28 + 1,
            i % 24,
            i * 7 % 60,
            seller,
            i % 9 + 1,
            ITEMS[i % 5],
            NAMES[(3 * i + 1) % 8],
            i * 37 % 1000
        ));
        if i % 7 == 0 {
            text.push_str(&format!(" <{}@guild.example>", seller.to_lowercase()));
        }
        text.push('\n');
    }
    text
}

// ANCHOR: hybrid
use regex_automata::hybrid::regex::Regex;

fn main() {
    let book = guild_book(2000);
    let jobs = [
        ("dates", r"\d{4}-\d{2}-\d{2}"),
        ("mail", r"[\w.+-]+@[\w-]+\.[\w.-]+"),
        ("names", "Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go"),
        ("caseless", "(?i)perl"),
        ("sales", r"(\w+) sold (\d+) (\w+)"),
    ];
    for (name, pattern) in jobs {
        let re = Regex::new(pattern).unwrap();
        // the caches of the two DFAs (forward, reverse)
        // 2つの DFA のキャッシュ（前向き、逆向き）
        let mut cache = re.create_cache();
        let count = re.find_iter(&mut cache, &book).count();
        let (fwd, rev) = cache.as_parts();
        println!(
            "{name:<8} {count:>5} matches, caches {:>6} + {:>6} bytes",
            fwd.memory_usage(),
            rev.memory_usage()
        );
    }
}
// ANCHOR_END: hybrid
