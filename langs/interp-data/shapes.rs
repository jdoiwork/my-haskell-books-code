// The Rust code in the chapter on data and case.
// data と case の章の Rust のコード

// ANCHOR: enum
// Rust: an enum, match with guards, and derive to compare
// Rust：enum、ガード付きの match、比べるための derive
#[derive(Debug, PartialEq, PartialOrd)]
enum Shape {
    Circle(i64),
    Rect(i64, i64),
}

fn name(s: &Shape) -> &'static str {
    match s {
        Shape::Circle(_) => "circle",
        Shape::Rect(w, h) if w == h => "square",
        Shape::Rect(_, _) => "rect",
    }
}

fn main() {
    println!("{}", name(&Shape::Rect(3, 3)));              // square
    // The variant written first is the smaller one.
    // 先に書いたものが小さい
    println!("{}", Shape::Circle(5) < Shape::Rect(1, 1));  // true
    // ANCHOR_END: enum
    assert_eq!(name(&Shape::Circle(1)), "circle");
    assert_eq!(name(&Shape::Rect(2, 5)), "rect");
    assert!(Shape::Circle(5) < Shape::Rect(1, 1));
    assert!(Shape::Rect(1, 2) < Shape::Rect(1, 3));
    // ANCHOR: enumEnd
}
// ANCHOR_END: enumEnd
