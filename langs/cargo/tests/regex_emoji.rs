// The Rust code in the emoji chapter of MiniRegex: len counts bytes,
// chars counts code points, the regex crate knows \p{Emoji} but not \X.
// MiniRegex の絵文字の章の Rust のコード：len はバイトを、chars は
// コードポイントを数える。regex のクレートは \p{Emoji} を知っているが、
// \X は知らない
use regex::Regex;

// 👨‍👩‍👧: a man, a woman and a girl, with ZWJs between
// 👨‍👩‍👧：男の人、女の人、女の子のあいだに ZWJ
const FAMILY: &str = "\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}";

#[test]
fn counts() {
    // ANCHOR: count
    // Rust: bytes and code points (a grapheme counter is another crate)
    // Rust：バイトとコードポイント（書記素を数えるのは、別のクレート）
    println!("{} {}", FAMILY.len(), FAMILY.chars().count());
    // ANCHOR_END: count
    assert_eq!(FAMILY.len(), 18);
    assert_eq!(FAMILY.chars().count(), 5);
}

#[test]
fn emoji_but_no_cluster() {
    // ANCHOR: emoji
    // Rust: \p{Emoji} takes 7 and # too; \X is an error
    // Rust：\p{Emoji} は 7 や # も取る。\X はエラー
    let post = format!("No.7 #1 {FAMILY}");
    let found: Vec<&str> = Regex::new(r"\p{Emoji}").unwrap()
        .find_iter(&post)
        .map(|m| m.as_str())
        .collect();
    println!("{:?}", found);
    println!("{}", Regex::new(r"\X").is_err());
    // ANCHOR_END: emoji
    assert_eq!(found, ["7", "#", "1", "👨", "👩", "👧"]);
    assert!(Regex::new(r"\X").is_err());
}
