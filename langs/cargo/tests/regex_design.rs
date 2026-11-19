// The Rust code in the design chapter of MiniRegex: the regex crate.
// Regex::new gives a Result, and each method has one kind of answer.
// MiniRegex の設計の章の Rust のコード：regex クレート。Regex::new は
// Result を返し、どのメソッドも答えの形は1つ
use regex::Regex;

const RECORDS: &str = "2026-11-19 Mira sold a dragon scale: 300 gold
2026-11-20 Ruby bought herb, sword, shield
On 2026-11-21 a dragon was seen near the lake
Room 1234-56-789 is closed
";

#[test]
fn uses_the_regex_crate() -> Result<(), regex::Error> {
    // ANCHOR: api
    // Rust (regex crate): new, is_match, find, find_iter, replace_all,
    // split
    // Rust（regex クレート）：new・is_match・find・find_iter・
    // replace_all・split
    let date = Regex::new(r"[0-9]{4}-[0-9]{2}-[0-9]{2}")?;
    let dragon = Regex::new(r"dragon")?;
    let comma = Regex::new(r", ")?;
    let first = dragon.find(RECORDS); // Option<Match>
    let dates: Vec<&str> = date
        .find_iter(RECORDS)
        .map(|m| m.as_str())
        .collect();
    let renamed = dragon.replace_all(RECORDS, "wyvern");
    let loot: Vec<&str> = comma.split("herb, sword, shield").collect();
    // ANCHOR_END: api
    assert!(date.is_match(RECORDS));
    let m = first.unwrap();
    assert_eq!((m.as_str(), m.start(), m.end()), ("dragon", 23, 29));
    assert_eq!(
        dates,
        ["2026-11-19", "2026-11-20", "2026-11-21", "1234-56-78"]
    );
    assert!(renamed.starts_with("2026-11-19 Mira sold a wyvern scale"));
    assert_eq!(loot, ["herb", "sword", "shield"]);
    Ok(())
}

#[test]
fn tells_a_wrong_pattern_when_made() {
    // ANCHOR: error
    // Rust: a wrong pattern is an Err from Regex::new
    // Rust：まちがったパターンは、Regex::new の Err
    match Regex::new(r"ab\") {
        Ok(_) => println!("compiled"),
        Err(e) => println!("{e}"),
    }
    // ANCHOR_END: error
    assert!(Regex::new(r"ab\").is_err());
}

#[test]
fn counts_places_in_bytes() {
    // The places are counted in bytes of UTF-8: 日 and 本 are 3 each.
    // 場所は UTF-8 のバイトで数える：日も本も3バイト
    let b = Regex::new("b").unwrap();
    let m = b.find("日本b").unwrap();
    assert_eq!((m.start(), m.end()), (6, 7));
}

#[test]
fn splits_with_the_empty_pattern() {
    // The empty pattern, between every character, at both ends too.
    // 空のパターンは、すべての文字のあいだに。両端にも
    let empty = Regex::new("").unwrap();
    let pieces: Vec<&str> = empty.split("abc").collect();
    assert_eq!(pieces, ["", "a", "b", "c", ""]);
}
