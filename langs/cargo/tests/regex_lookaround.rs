// The Rust code in the chapter on looking around of MiniRegex: the
// regex crate has no lookaround (so that it can promise linear time);
// a group does the work instead. \b is Unicode.
// MiniRegex のまわりを見る章の Rust のコード：regex クレートには
// 「見る」ものがない（線形時間を約束するため）。かわりにグループで。
// \b は Unicode
use regex::Regex;

#[test]
fn no_lookaround() {
    // ANCHOR: group
    // Rust: no (?=...); take " gold" too, and keep only group 1
    // Rust：(?=...) はない。" gold" ごと食べて、グループ1だけを取る
    assert!(Regex::new(r"\d+(?= gold)").is_err());
    let gold = Regex::new(r"(\d+) gold").unwrap();
    let ledger = "for 300 gold, a herb for 5 silver, price:1200 gold";
    let amounts: Vec<&str> = gold
        .captures_iter(ledger)
        .map(|c| c.get(1).unwrap().as_str())
        .collect();
    println!("{:?}", amounts);
    // ANCHOR_END: group
    assert_eq!(amounts, ["300", "1200"]);
    assert!(Regex::new(r"(?<=price:)\d+").is_err());
}

#[test]
fn no_atomic() {
    // ANCHOR: possessive
    // Rust: no (?>...); and a*+ is not possessive: it is (?:a*)+, a
    //   repeat of a repeat, which gives back
    // Rust：(?>...) はない。a*+ は所有的ではなく (?:a*)+、くり返しの
    //   くり返しで、返す
    assert!(Regex::new(r"(?>a|ab)c").is_err());
    let plus = Regex::new(r"a*+a").unwrap();
    println!("{}", plus.find("aaa").unwrap().as_str());
    // ANCHOR_END: possessive
    assert_eq!(plus.find("aaa").unwrap().as_str(), "aaa");
}

#[test]
fn boundary_is_unicode() {
    // ANCHOR: boundary
    // Rust: \b looks at the Unicode \w; (?-u:\b) at the ASCII one
    // Rust：\b は Unicode の \w を見る。(?-u:\b) なら ASCII の \w
    let unicode = Regex::new(r"\bcafé\b").unwrap();
    let ascii = Regex::new(r"(?-u:\b)café(?-u:\b)").unwrap();
    println!("{} {}", unicode.is_match("a café."), ascii.is_match("a café."));
    // ANCHOR_END: boundary
    assert!(unicode.is_match("a café."));
    assert!(!ascii.is_match("a café."));
}
