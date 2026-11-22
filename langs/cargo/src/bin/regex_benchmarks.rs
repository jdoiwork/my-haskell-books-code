// The chapter on measuring (MiniRegex): the same jobs as MiniRegex's
// benchmark, with Rust's regex crate. Run as:
//   cargo run --release --bin regex_benchmarks -- LINES ROUNDS
// Prints one line for each job: the job, the count, the checksum, and
// the middle time of the rounds in microseconds.
// 測って比べる章（MiniRegex）：MiniRegex のベンチマークと同じ仕事を、
// Rust の regex クレートで。仕事ごとに1行：仕事、件数、確かめの数、
// 回のまん中の時間（マイクロ秒）
use regex::Regex;
use std::time::Instant;

const NAMES: [&str; 8] = ["Mira", "Ruby", "Lam", "GHC", "Perl", "Rust", "Python", "Go"];
const ITEMS: [&str; 5] = ["herbs", "potions", "swords", "maps", "scrolls"];

// 📒 The guild's book of sales: n lines, the same in every language.
// ギルドの売り上げのログ：n 行。どの言語でも同じもの
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

const JOBS: [(&str, &str); 5] = [
    ("dates", r"\d{4}-\d{2}-\d{2}"),
    ("mail", r"[\w.+-]+@[\w-]+\.[\w.-]+"),
    ("names", r"Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go"),
    ("caseless", r"(?i)perl"),
    ("sales", r"(\w+) sold (\d+) (\w+)"),
];

// ANCHOR: count
// 🔢 Count the matches; the checksum adds up group 2 where there is one
//    (then the groups are asked for, with captures_iter).
// 一致を数える。確かめの数は、2番のグループがあれば、その数の合計
// （そのときは captures_iter で、グループの場所も求める）
fn count(re: &Regex, text: &str) -> (usize, u64) {
    if re.captures_len() > 2 {
        let mut n = 0;
        let mut total = 0;
        for caps in re.captures_iter(text) {
            n += 1;
            total += caps[2].parse::<u64>().unwrap();
        }
        (n, total)
    } else {
        (re.find_iter(text).count(), 0)
    }
}
// ANCHOR_END: count

// 🕰️ The middle time of the rounds, in microseconds.
// 回の、まん中の時間（マイクロ秒）
fn clock<T>(rounds: usize, mut job: impl FnMut() -> T) -> (T, f64) {
    let mut times = Vec::new();
    let mut answer = None;
    for _ in 0..rounds {
        let start = Instant::now();
        answer = Some(job());
        times.push(start.elapsed().as_secs_f64() * 1_000_000.0);
    }
    times.sort_by(|a, b| a.partial_cmp(b).unwrap());
    (answer.unwrap(), times[times.len() / 2])
}

fn main() {
    let args: Vec<usize> = std::env::args()
        .skip(1)
        .map(|a| a.parse().unwrap())
        .collect();
    let (lines, rounds) = (args[0], args[1]);
    let text = guild_book(lines);
    for (name, pattern) in JOBS {
        let re = Regex::new(pattern).unwrap();
        let ((n, total), us) = clock(rounds, || count(&re, &text));
        println!("{name} {n} {total} {us:.2}");
    }
    // the name field: one search, eight names and a ! by mistake
    // 名前の欄：探すのは1回。名前が8個と、まちがえて入った !
    let field = format!("{}!", [NAMES[..5].to_vec(), NAMES[..3].to_vec()].concat().join(" "));
    let re = Regex::new(r"^(\w+\s?)+$").unwrap();
    let (found, us) = clock(rounds, || re.find(&field).is_some());
    println!("field {} 0 {us:.2}", found as u8);
}
