// 🧾 Where is the + on this line? Rust's strings are UTF-8: .len() is
//    the bytes.
// この行の + は、どこ？ Rust の文字列は UTF-8 なので、.len() はバイトの数
fn main() {
    let line = "f\"🧾 total: {total items + 100}\"";
    let before = &line[..line.find('+').unwrap()];

    // ANCHOR: procedural
    // Rust (procedural style)
    // Rust（手続き型スタイル）
    let mut units = 0;
    for ch in before.chars() {
        units += ch.len_utf16();
    }
    // ANCHOR_END: procedural

    // ANCHOR: functional
    // Rust (functional style)
    // Rust（関数型スタイル）
    let letters = before.chars().count(); // 24
    let utf16 = before.encode_utf16().count(); // 25
    let utf8 = before.len(); // 27
    // ANCHOR_END: functional

    assert_eq!((letters, utf16, utf8), (24, 25, 27));
    assert_eq!(units, utf16);
    println!("columns.rs: ok");
}
