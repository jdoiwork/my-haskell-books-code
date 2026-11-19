// The Rust code in the Unicode chapter of MiniRegex: \d is Unicode,
// (?-u) for ASCII, \p{...} is the script, flags in the middle of a
// pattern, and places counted in bytes.
// MiniRegex の Unicode の章の Rust のコード：\d は Unicode、ASCII には
// (?-u)、\p{...} は文字体系、パターンの途中のフラグ、バイトで数える場所
use regex::Regex;

#[test]
fn digits_are_unicode() {
    // ANCHOR: digits
    // Rust: \d is every decimal digit; (?-u:\d) is 0 to 9 only
    // Rust：\d は全部の10進の数字。(?-u:\d) は 0 から 9 だけ
    let record = "２０２６年１１月１９日、2026-11-19";
    let all: Vec<&str> = Regex::new(r"\d{4}").unwrap()
        .find_iter(record)
        .map(|m| m.as_str())
        .collect();
    let ascii: Vec<&str> = Regex::new(r"(?-u:\d){4}").unwrap()
        .find_iter(record)
        .map(|m| m.as_str())
        .collect();
    println!("{:?} {:?}", all, ascii);
    // ANCHOR_END: digits
    assert_eq!(all, ["２０２６", "2026"]);
    assert_eq!(ascii, ["2026"]);
}

#[test]
fn scripts_and_cases() {
    // ANCHOR: scripts
    // Rust: \p{Katakana} is the script only; scx= takes ー too
    // Rust：\p{Katakana} は文字体系だけ。scx= なら ー も入る
    let sc = Regex::new(r"\p{Katakana}+").unwrap();
    let scx = Regex::new(r"\p{scx=Katakana}+").unwrap();
    println!("{} {}", sc.find("記録室のノート").unwrap().as_str(),
             scx.find("記録室のノート").unwrap().as_str());
    // ANCHOR_END: scripts
    assert_eq!(sc.find("記録室のノート").unwrap().as_str(), "ノ");
    assert_eq!(scx.find("記録室のノート").unwrap().as_str(), "ノート");
    // ANCHOR: flags
    // Rust: (?i) in the middle of a pattern, for the rest of it
    // Rust：パターンの途中の (?i)。そこから先に効く
    let re = Regex::new(r"a(?i)b").unwrap();
    println!("{} {}", re.is_match("aB"), re.is_match("AB"));
    let kelvin = Regex::new(r"(?i)k").unwrap();
    println!("{}", kelvin.is_match("\u{212a}"));
    // ANCHOR_END: flags
    assert_eq!((re.is_match("aB"), re.is_match("AB")), (true, false));
    assert!(kelvin.is_match("\u{212a}"));
    // simple case folding, like PCRE2: ß is ẞ, but not SS
    // PCRE2 と同じ単純な畳み方：ß は ẞ。SS ではない
    let sharp = Regex::new(r"^(?i)ß$").unwrap();
    assert!(sharp.is_match("ẞ") && !sharp.is_match("SS"));
}

#[test]
fn places_in_bytes() {
    // ANCHOR: places
    // Rust: a match's place is in bytes of UTF-8
    // Rust：一致の場所は UTF-8 のバイトで数える
    let m = Regex::new("Lam").unwrap().find("名前: Lam").unwrap();
    println!("{}..{}", m.start(), m.end());
    // ANCHOR_END: places
    assert_eq!((m.start(), m.end()), (8, 11));
}
