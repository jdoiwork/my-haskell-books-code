// Lengths in types, in Rust: const generics, for the chapter
// "Type-level programming".
// Rust で長さを型に：const ジェネリクス。
// 「型レベルプログラミング入門」の章

// ANCHOR: zip
// Rust (const generics: N is a number that lives in the type)
// Rust（const ジェネリクス：N は、型の中に住む数）
fn zip<A: Copy, B: Copy, const N: usize>(
    a: [A; N],
    b: [B; N],
) -> [(A, B); N] {
    std::array::from_fn(|i| (a[i], b[i]))
}

fn main_part() -> [(&'static str, &'static str); 3] {
    let party = ["Lambda", "Python", "Rust"];
    let weapons = ["sword", "staff", "shield"];
    // zip(party, ["sword", "staff"]) would not compile: 3 and 2 differ.
    // zip(party, ["sword", "staff"]) はコンパイルできない：3 と 2 がちがう
    zip(party, weapons)
}
// ANCHOR_END: zip

fn main() {
    let armed = main_part();
    assert_eq!(armed[0], ("Lambda", "sword"));
    assert_eq!(armed.len(), 3);
    println!("ok");
}
