// The best score, in Rust: an iterator's max gives an Option.
// いちばんよい点を、Rust で：イテレーターの max は Option をくれる
use std::collections::BTreeMap;

fn main() {
    // ANCHOR: book
    // Rust
    let totals =
        BTreeMap::from([("Python", 247), ("TS", 159), ("Rust", 262)]);
    // max gives Some(..), or None when there is nothing.
    // max は Some(..)。何もなければ None
    let top: Option<&i32> = totals.values().max();
    let empty: Vec<i32> = Vec::new();
    let nothing: Option<&i32> = empty.iter().max();
    // An Option is a container too: it can be summed.
    // Option も入れ物：足し合わせられる
    let three: i32 = Some(3).into_iter().sum();
    // ANCHOR_END: book
    assert_eq!(top, Some(&262));
    assert_eq!(nothing, None);
    assert_eq!(three, 3);
    let all: i32 = totals.values().sum();
    assert_eq!(all, 668);
    println!("ok best.rs");
}
