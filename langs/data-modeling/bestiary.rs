// The Rust code in the data-modeling chapter.
// データをモデリングする章の Rust のコード
// ANCHOR: enum
// Rust (enum)
// Rust（enum）
#[derive(Debug, PartialEq)]
enum Element {
    Fire,
    Water,
    Grass,
}
// ANCHOR_END: enum
// ANCHOR: functional
// Rust (functional style)
// Rust（関数型スタイル）
enum Action {
    Attack(i64),
    Spell(Element),
    Heal(i64),
    Defend,
}

fn describe(action: &Action) -> String {
    match action {
        Action::Attack(power) => format!("Attack with power {power}"),
        Action::Spell(e) => format!("Cast a {e:?} spell"),
        Action::Heal(amount) => format!("Heal {amount} HP"),
        Action::Defend => "Defend".to_string(),
    }
}
// ANCHOR_END: functional
fn main() {
assert_eq!(Element::Fire, Element::Fire);
assert_ne!(Element::Water, Element::Grass);
assert_eq!(describe(&Action::Attack(30)), "Attack with power 30");
assert_eq!(describe(&Action::Spell(Element::Fire)), "Cast a Fire spell");
assert_eq!(describe(&Action::Heal(10)), "Heal 10 HP");
assert_eq!(describe(&Action::Defend), "Defend");
}
