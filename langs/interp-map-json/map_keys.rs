// The Rust code in the chapter on Map and JSON: keys that are pairs.
// Map と JSON の章の Rust のコード：組をキーにする
use std::collections::{BTreeMap, BTreeSet};

fn main() {
    let moves = ["R", "R", "D", "L", "U", "U"];
    let treasures: BTreeMap<(i32, i32), &str> = BTreeMap::from([
        ((2, 0), "herb"),
        ((1, 1), "elixir"),
        ((5, 5), "key"),
    ]);

    // ANCHOR: btree
    // Rust (procedural style): BTreeMap and BTreeSet keep their keys in
    // order, so a key must be Ord (comparable), like Lam's Comparable.
    // Rust（手続き型スタイル）：BTreeMap と BTreeSet はキーを順に並べて
    // 持つので、キーは Ord（比べられる）でなければならない
    let (mut x, mut y) = (0, 0);
    let mut visited = BTreeSet::from([(x, y)]);
    for m in moves {
        match m {
            "R" => x += 1,
            "L" => x -= 1,
            "D" => y += 1,
            "U" => y -= 1,
            _ => {}
        }
        visited.insert((x, y));
    }
    let found: Vec<&str> = treasures
        .iter()
        .filter(|(cell, _)| visited.contains(cell))
        .map(|(_, item)| *item)
        .collect();
    println!("{} {:?}", visited.len(), found); // 6 ["elixir", "herb"]
    // ANCHOR_END: btree

    assert_eq!(visited.len(), 6);
    assert_eq!(found, vec!["elixir", "herb"]);
}
