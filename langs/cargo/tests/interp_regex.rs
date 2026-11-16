// The Rust code in the chapter on regular expressions: the regex crate.
// It runs in time that grows in a line with the text, so it has no
// look-around and no backreferences.
// 正規表現の章の Rust のコード：regex クレート。文字列の長さに比例する
// 時間で動くかわりに、先読み・後読みと後方参照がない
use regex::{Captures, Regex};

// ANCHOR: replace
// Rust (regex crate): replace_all with a closure
// Rust（regex クレート）：クロージャーを渡す replace_all
fn hide(phone: &Regex, text: &str) -> String {
    phone
        .replace_all(text, |caps: &Captures| {
            format!("***-****-{}", &caps["last"])
        })
        .into_owned()
}
// ANCHOR_END: replace

#[test]
fn hides_the_phone_numbers() {
    let phone = Regex::new(r"\d{2,4}-\d{2,4}-(?<last>\d{4})").unwrap();
    assert_eq!(
        hide(&phone, "call 080-9876-5432 or 03-1234-5678 now"),
        "call ***-****-5432 or ***-****-5678 now"
    );
}

#[test]
fn has_no_look_around_and_no_backreferences() {
    // ANCHOR: unsupported
    // Rust: Regex::new says no to these (an Err, not a panic)
    // Rust：Regex::new は、これらを受け付けない（panic ではなく Err）
    assert!(Regex::new(r"(?<=x)a").is_err());
    assert!(Regex::new(r"(a)\1").is_err());
    // ANCHOR_END: unsupported
}

#[test]
fn does_not_blow_up_on_nested_repeats() {
    // ANCHOR: linear
    // Rust: no backtracking, so (a+)+$ is quick even on a long text
    // Rust：バックトラックしないので、長い文字列でも (a+)+$ はすぐ終わる
    let nested = Regex::new(r"^(a+)+$").unwrap();
    let text = "a".repeat(100_000) + "b";
    assert!(!nested.is_match(&text));
    // ANCHOR_END: linear
}
