// Rust's regex reads its pattern when the program runs: a wrong pattern
// is an Err of Regex::new, and a group name with a typo is a None of
// caps.name (caps["yaer"] would panic). Compare with [re|…|] and
// [typed|…|] of regex-polish.md, which stop the build.
// Rust の regex は、プログラムが動くときにパターンを読む。まちがった
// パターンは Regex::new の Err、打ちまちがえたグループの名前は caps.name の
// None（caps["yaer"] なら panic）。regex-polish.md の [re|…|] と
// [typed|…|] は、ビルドを止める
use regex::Regex;

#[test]
fn typos_are_found_when_it_runs() {
    // ANCHOR: typos
    // Rust (regex crate)
    // Rust（regex クレート）
    let bad = Regex::new(r"(?<year>\d{4})-(?<month>\d{2}");
    assert!(bad.is_err());

    let date = Regex::new(r"(?<year>\d{4})-(?<month>\d{2})").unwrap();
    let caps = date.captures("since 2026-11").unwrap();
    assert_eq!(&caps["year"], "2026");
    // a typo in the name: None (caps["yaer"] would panic)
    // 名前の打ちまちがい：None（caps["yaer"] なら panic）
    assert!(caps.name("yaer").is_none());
    // ANCHOR_END: typos
    let msg = bad.unwrap_err().to_string();
    assert!(msg.contains("unclosed group"), "{msg}");
}
