// The Rust code in the chapter on patterns and sugar.
// パターンと糖衣構文の章の Rust のコード

// ANCHOR: closures
// Rust: patterns in closure arguments and in let, and iterator chains
// Rust：クロージャーの引数と let のパターン、イテレーターの連なり
fn main() {
    let scores = [("Python", 82), ("TS", 75), ("Rust", 91), ("Go", 68)];

    let passed: Vec<&str> = scores
        .iter()
        .filter(|&&(_, score)| score >= 70)
        .map(|&(name, _)| name)
        .collect();

    let total: i32 = scores.iter().map(|&(_, score)| score).sum();

    let (q, r) = (17 / 5, 17 % 5);
    // ANCHOR_END: closures

    assert_eq!(passed, ["Python", "TS", "Rust"]);
    assert_eq!(total, 316);
    assert_eq!((q, r), (3, 2));

    // ANCHOR: letElse
    // A pattern that may not fit needs let ... else.
    // 外れうるパターンには、let ... else が要る
    let maybe: Option<i32> = Some(3);
    let Some(x) = maybe else {
        panic!("no value");
    };
    // ANCHOR_END: letElse
    assert_eq!(x, 3);
}
