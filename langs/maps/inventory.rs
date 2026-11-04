// The Rust code in the Map chapter.
// Map の章の Rust のコード
use std::collections::HashMap;
fn main() {
let mut bag: HashMap<&str, i64> = HashMap::from([("potion", 3), ("herb", 1), ("elixir", 0)]);
    // ANCHOR: procedural
    // Rust (procedural style)
    // Rust（手続き型スタイル）
    *bag.entry("potion").or_insert(0) += 2;
    // ANCHOR_END: procedural
assert_eq!(bag["potion"], 5);
}
