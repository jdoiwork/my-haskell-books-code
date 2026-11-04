// traverse in Rust: collect into Option<Vec<_>>, for the chapter
// "Foldable and Traversable".
// Rust の traverse：Option<Vec<_>> に collect する。
// 「Foldable・Traversable」の章

fn price_of(item: &str) -> Option<u32> {
    match item {
        "potion" => Some(50),
        "herb" => Some(10),
        "sword" => Some(300),
        _ => None,
    }
}

// ANCHOR: procedural
// Rust (procedural style: a loop that gives up early)
// Rust（手続き型スタイル：途中であきらめるループ）
fn prices_loop(items: &[&str]) -> Option<Vec<u32>> {
    let mut result = Vec::new();
    for item in items {
        match price_of(item) {
            Some(p) => result.push(p),
            None => return None,
        }
    }
    Some(result)
}
// ANCHOR_END: procedural

// ANCHOR: collect
// Rust (functional style: collect turns many Options into one)
// Rust（関数型スタイル：collect が、たくさんの Option を1つにまとめる）
fn prices(items: &[&str]) -> Option<Vec<u32>> {
    items.iter().map(|item| price_of(item)).collect()
}
// ANCHOR_END: collect

fn main() {
    let good = ["potion", "herb", "sword"];
    let bad = ["potion", "shield"];
    assert_eq!(prices_loop(&good), Some(vec![50, 10, 300]));
    assert_eq!(prices_loop(&bad), None);
    assert_eq!(prices(&good), Some(vec![50, 10, 300]));
    assert_eq!(prices(&bad), None);
    println!("ok");
}
