// include_bytes! of Rust: the bytes of a file, put into the program
// when it is compiled (the path is from this source file).
// Rust の include_bytes!：ファイルのバイト列を、コンパイルのときに
// プログラムに入れる（道は、このソースのファイルから）

// ANCHOR: include
// Rust
static LOGO: &[u8] = include_bytes!("public/logo.png");
// ANCHOR_END: include

fn main() {
    // A PNG starts with these 8 bytes.
    // PNG は、この8バイトで始まる
    assert_eq!(&LOGO[..8], b"\x89PNG\r\n\x1a\n");
    assert_eq!(LOGO.len(), 150);
    println!("ok");
}
