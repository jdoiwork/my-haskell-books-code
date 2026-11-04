// The Rust code in the branching chapter.
// 場合分けと Maybe の章の Rust のコード
// ANCHOR: functional
// Rust (functional style)
// Rust（関数型スタイル）
fn category(item: &str) -> &'static str {
    match item {
        "potion" | "elixir" => "medicine",
        "sword" => "weapon",
        "shield" => "armor",
        _ => "unknown",
    }
}
// ANCHOR_END: functional
// ANCHOR: functional_2
// Rust (functional style)
// Rust（関数型スタイル）
fn price(item: &str) -> Option<i64> {
    match item {
        "potion" => Some(50),
        "elixir" => Some(150),
        "sword" => Some(300),
        _ => None,
    }
}

fn price_tag(item: &str) -> String {
    match price(item) {
        Some(p) => format!("{item}: {p} G"),
        None => format!("{item}: not for sale"),
    }
}
// ANCHOR_END: functional_2
fn main() {
assert_eq!(category("elixir"), "medicine");
assert_eq!(category("bow"), "unknown");
assert_eq!(price_tag("sword"), "sword: 300 G");
assert_eq!(price_tag("bow"), "bow: not for sale");
}
