// The outcome of a quest, for the chapter "ADTs are everywhere".
// 依頼の結果。「代数的データ型は、どこにでもある」の章

// ANCHOR: enum
// Rust (enum and match)
// Rust（enum と match）
enum Outcome {
    Cleared { gold: u32 },
    Failed { place: String },
    Fled,
}

fn describe(outcome: &Outcome) -> String {
    match outcome {
        Outcome::Cleared { gold } => format!("cleared! +{gold} G"),
        Outcome::Failed { place } => format!("failed at {place}"),
        Outcome::Fled => "ran away".to_string(),
    }
}
// ANCHOR_END: enum

fn main() {
    assert_eq!(describe(&Outcome::Cleared { gold: 50 }), "cleared! +50 G");
    let failed = Outcome::Failed { place: "cave".to_string() };
    assert_eq!(describe(&failed), "failed at cave");
    assert_eq!(describe(&Outcome::Fled), "ran away");
    println!("ok");
}
