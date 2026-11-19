// The Rust code in the matcher chapter of MiniRegex: the regex crate
// takes the left branch, too, but it does not backtrack.
// MiniRegex のマッチャーの章の Rust のコード：regex クレートも左の枝を
// 取る。ただし、バックトラックはしない
use regex::Regex;
use std::time::Instant;

#[test]
fn takes_the_left_branch() {
    // ANCHOR: first
    // Rust: the left branch wins, as in Perl (not the longest)
    // Rust：Perl と同じく、左の枝が勝つ（いちばん長いものではない）
    let re = Regex::new(r"a|ab").unwrap();
    println!("{:?}", re.find("abc").map(|m| m.as_str()));
    // ANCHOR_END: first
    assert_eq!(re.find("abc").map(|m| m.as_str()), Some("a"));
}

#[test]
fn does_not_try_every_way() {
    // ANCHOR: steps
    // Rust: (a+)+b on 10000 a's answers at once
    // Rust：a が 1万個でも、(a+)+b はすぐに答える
    let re = Regex::new(r"(a+)+b").unwrap();
    let text = "a".repeat(10_000) + "c";
    let start = Instant::now();
    println!("{}", re.is_match(&text));
    // ANCHOR_END: steps
    assert!(!re.is_match(&text));
    assert!(start.elapsed().as_secs_f64() < 1.0);
}

#[test]
fn has_no_backreferences() {
    // Rust: a backreference is an error (the regex crate has none)
    // Rust：後方参照はエラー（regex クレートは、持っていない）
    assert!(Regex::new(r"(a)\1").is_err());
}
