// The Rust code in the expressions chapter.
// 式・型・関数の章の Rust のコード
// ANCHOR: functional
// Rust (functional style)
// Rust（関数型スタイル）
fn status(hp: i64) -> &'static str {
    if hp <= 0 { "Defeated!" } else { "Still standing" }
}
// ANCHOR_END: functional
fn damage(attack: i64, defense: i64) -> i64 {
    (attack - defense).max(1)
}
fn main() {
assert_eq!(status(0), "Defeated!");
assert_eq!(status(5), "Still standing");
    // ANCHOR: functional_2
    // Rust (functional style)
    // Rust（関数型スタイル）
    let hero_attack = |defense: i64| damage(50, defense);
    let results = [10, 30, 60].map(hero_attack);
    // ANCHOR_END: functional_2
assert_eq!(results, [40, 20, 1]);
}
