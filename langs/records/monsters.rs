// The Rust code in the records chapter.
// レコードと newtype の章の Rust のコード
#[derive(Debug, Clone, Copy, PartialEq)]
enum Element { Water }

#[derive(Debug)]
struct Monster {
    name: String,
    hp: i64,
    element: Element,
}
// ANCHOR: functional
// Rust (functional style)
// Rust（関数型スタイル）
fn hit_monster(atk: i64, monster: Monster) -> Monster {
    Monster {
        hp: (monster.hp - atk).max(0),
        ..monster
    }
}
// ANCHOR_END: functional
fn main() {
let slime = Monster { name: "Slime".to_string(), hp: 30, element: Element::Water };
let hit = hit_monster(10, slime);
assert_eq!(hit.hp, 20);
assert_eq!(hit.name, "Slime");
assert_eq!(hit.element, Element::Water);
assert_eq!(hit_monster(100, hit).hp, 0);
}
