// The Rust code in the chapter on reading data: serde reads JSON into a
// struct, and stops at the first problem.
// データを読む章の Rust のコード：serde は JSON を構造体に読み、最初の
// 問題で止まる
use serde::Deserialize;

// ANCHOR: serde
// Rust (serde): the shape comes from the struct, by derive
// Rust（serde）：形は derive で構造体から作る
#[derive(Deserialize)]
struct Order {
    item: String,
    count: u32,
    price: u32,
}

#[derive(Deserialize)]
struct Orders {
    orders: Vec<Order>,
}

const BAD: &str = r#"{"orders": [
  {"item": "sticker", "count": "four", "price": 300.5},
  {"item": "mug", "count": 1, "price": 1200},
  {"name": "T-shirt, large", "count": 2}]}"#;

fn first_problem() -> String {
    match serde_json::from_str::<Orders>(BAD) {
        Ok(o) => o
            .orders
            .iter()
            .map(|x| format!("{} {}", x.item, x.count * x.price))
            .collect::<Vec<_>>()
            .join("\n"),
        Err(e) => e.to_string(),
    }
}
// ANCHOR_END: serde

#[test]
fn stops_at_the_first_problem() {
    let message = first_problem();
    println!("{message}");
    assert_eq!(
        message,
        "invalid type: string \"four\", expected u32 at line 2 column 37"
    );
}
