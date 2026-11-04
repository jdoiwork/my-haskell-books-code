// The party's skills of the chapter "Logic and sets", in Rust.
// 「論理と集合」の章のパーティーのスキルを、Rust で
use std::collections::BTreeSet;

fn set(items: &[&'static str]) -> BTreeSet<&'static str> {
    items.iter().copied().collect()
}

fn main() {
    let needed = set(&["heal", "fire", "stealth"]);
    let members = vec![
        set(&["fire", "sword"]),
        set(&["heal", "fire"]),
        set(&["fire", "shield"]),
    ];
    // ANCHOR: functional
    // Rust
    let party: BTreeSet<_> = members.iter().flatten().copied().collect();
    let missing = &needed - &party;      // difference / 差集合
    let shared = members
        .iter()
        .skip(1)
        .fold(members[0].clone(), |acc, m| &acc & m);
    let everyone_fights = members.iter().all(|m| m.contains("fire"));
    // ANCHOR_END: functional
    assert_eq!(missing, set(&["stealth"]));
    assert_eq!(shared, set(&["fire"]));
    assert!(everyone_fights);
    println!("ok");
}
