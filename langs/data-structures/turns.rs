// The Rust code in the data structures chapter.
// データ構造の選び方の章の Rust のコード
use std::collections::VecDeque;

// ANCHOR: vecdeque
// Rust (VecDeque: fast at both ends)
// Rust（VecDeque：両端が速い）
fn take_turns(n: usize, heroes: &[&str]) -> Vec<String> {
    let mut line: VecDeque<String> =
        heroes.iter().map(|h| h.to_string()).collect();
    let mut order = Vec::new();
    for _ in 0..n {
        if let Some(hero) = line.pop_front() {
            order.push(hero.clone());
            line.push_back(hero);
        }
    }
    order
}
// ANCHOR_END: vecdeque

fn main() {
    let order = take_turns(7, &["Lambda", "Python", "Rust"]);
    assert_eq!(order, vec!["Lambda", "Python", "Rust", "Lambda",
                           "Python", "Rust", "Lambda"]);
    assert!(take_turns(3, &[]).is_empty());
    println!("ok");
}
