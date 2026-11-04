// The serde code in the JSON chapter.
// JSON を扱う章の serde のコード
fn main() -> Result<(), serde_json::Error> {
let text = r#"{"name": "Slime", "hp": 30, "element": "Water", "drops": ["gel"]}"#;
    // ANCHOR: serde
    // Rust (serde)
    // Rust（serde を使う）
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize)]
    enum Element { Fire, Water, Grass }

    #[derive(Serialize, Deserialize)]
    struct Monster {
        name: String,
        hp: i32,
        element: Element,
        drops: Vec<String>,
    }

    let monster: Monster = serde_json::from_str(text)?;
    // ANCHOR_END: serde
assert_eq!(monster.hp, 30);
assert!(matches!(monster.element, Element::Water));
assert_eq!(monster.drops, ["gel"]);
let _ = (&monster.name, Element::Fire, Element::Grass);
Ok(())
}
