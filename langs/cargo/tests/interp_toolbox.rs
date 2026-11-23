// The totals of the household account, in Rust: serde writes a map as
// JSON, and a type of our own joins with #[derive(Serialize)].
// 家計簿の合計を Rust で。serde は map を JSON に書き、自分の型は
// #[derive(Serialize)] で仲間になる
use serde::Serialize;
use std::collections::BTreeMap;

// ANCHOR: serde
// Rust
#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "lowercase")]
enum Category {
    Food,
    Books,
    Transport,
}

fn totals() -> BTreeMap<Category, i64> {
    let mut sums = BTreeMap::new();
    for (c, n) in [
        (Category::Food, 1200),
        (Category::Books, 3300),
        (Category::Food, 800),
        (Category::Transport, 450),
        (Category::Food, 650),
    ] {
        *sums.entry(c).or_insert(0) += n;
    }
    sums
}
// ANCHOR_END: serde

#[test]
fn writes_the_totals_as_json() {
    // The order of the enum: food, books, transport.
    // enum の順：food・books・transport
    assert_eq!(
        serde_json::to_string(&totals()).unwrap(),
        r#"{"food":2650,"books":3300,"transport":450}"#
    );
    assert_eq!(
        serde_json::to_string(&(Some(1.5), None::<f64>)).unwrap(),
        "[1.5,null]"
    );
}
