// The Rust code in the chapter on f"...": format! with the names in
// the braces. Rust has no format for a comma every 3 digits, so a small
// function of our own makes it.
// f"..." の章の Rust のコード：波かっこに名前を書く format!。
// Rust には3桁ごとのカンマの書式がないので、小さな関数を自分で書く
use std::collections::HashMap;

// ANCHOR: format
// Rust (format!): {item:<10} and {n:>3}, checked when compiling
// Rust（format!）：{item:<10} や {n:>3}。コンパイルのときに確かめる
fn yen(n: u64) -> String {
    let digits = n.to_string();
    let mut out = String::new();
    for (i, c) in digits.chars().enumerate() {
        if i > 0 && (digits.len() - i) % 3 == 0 {
            out.push(',');
        }
        out.push(c);
    }
    out
}

fn line(prices: &HashMap<&str, u64>, item: &str, n: u64) -> String {
    let price = prices[item];
    let total = n * price;
    format!("{item:<10}{n:>3} x {:>6} = {:>9}", yen(price), yen(total))
}
// ANCHOR_END: format

fn main() {
    let prices = HashMap::from([("sticker", 300), ("mug", 1200), ("tshirt", 2500)]);
    let lines: Vec<String> = [("sticker", 4), ("mug", 1), ("tshirt", 3)]
        .iter()
        .map(|&(item, n)| line(&prices, item, n))
        .collect();
    assert_eq!(
        lines,
        vec![
            "sticker     4 x    300 =     1,200",
            "mug         1 x  1,200 =     1,200",
            "tshirt      3 x  2,500 =     7,500",
        ]
    );
    println!("{}", lines.join("\n"));
}
