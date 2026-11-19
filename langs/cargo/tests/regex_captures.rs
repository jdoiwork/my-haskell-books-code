// The Rust code in the groups chapter of MiniRegex: a group with its
// place, all the matches after an empty one, and a pitfall of $name in
// a replacement text.
// MiniRegex のグループの章の Rust のコード：場所つきのグループ、空の一致の
// あとの全部の一致、置き換えの文字列の $name の落とし穴
use regex::Regex;

#[test]
fn takes_a_group_with_its_place() {
    // ANCHOR: groups
    // Rust: a group is a Match, with its text and its place
    // Rust：グループは Match。文字列と場所を持つ
    let date =
        Regex::new(r"(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})").unwrap();
    let caps = date.captures("On 2026-11-19").unwrap();
    let month = caps.name("month").unwrap();
    println!("{} {}..{}", month.as_str(), month.start(), month.end());
    // ANCHOR_END: groups
    assert_eq!((month.as_str(), month.start(), month.end()), ("11", 8, 10));
    // a group that took no part is None
    // 使われなかったグループは None
    let caps = Regex::new(r"(a)|(b)").unwrap().captures("b").unwrap();
    assert!(caps.get(1).is_none());
    // in a repeat, the group keeps what it found (like PCRE2)
    // くり返しの中のグループは、見つけたものを覚えておく（PCRE2 と同じ）
    let caps = Regex::new(r"(?:(a)|b)*").unwrap().captures("ab").unwrap();
    assert_eq!(caps.get(1).map(|m| m.as_str()), Some("a"));
}

#[test]
fn finds_all_after_an_empty_match() {
    // ANCHOR: findall
    // Rust: no empty match right after a match, and after an empty
    //   match, one character on
    // Rust：一致のすぐうしろの空の一致は出さない。空の一致のあとは、
    //   1文字先へ
    let all = |p: &str, t: &str| -> Vec<String> {
        let re = Regex::new(p).unwrap();
        re.find_iter(t).map(|m| m.as_str().to_string()).collect()
    };
    println!("{:?}", all(r"x*", "axbxx"));
    println!("{:?}", all(r"a??", "a"));
    // ANCHOR_END: findall
    assert_eq!(all(r"x*", "axbxx"), ["", "x", "xx"]);
    assert_eq!(all(r"a??", "a"), ["", ""]);
}

#[test]
fn reads_a_long_name() {
    let date =
        Regex::new(r"(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})").unwrap();
    // ANCHOR: replace
    // Rust: $y_ is the group named "y_" (there is none: ""); ${y} ends
    //   the name
    // Rust：$y_ は "y_" という名前のグループ（ないので ""）。${y} なら、
    //   名前はそこで終わる
    println!("{}", date.replace_all("On 2026-11-19", "$d/$m/$y"));
    println!("{}", date.replace_all("On 2026-11-19", "$y_$m"));
    println!("{}", date.replace_all("On 2026-11-19", "${y}_$m"));
    // ANCHOR_END: replace
    assert_eq!(
        date.replace_all("On 2026-11-19", "$d/$m/$y"),
        "On 19/11/2026"
    );
    assert_eq!(date.replace_all("On 2026-11-19", "$y_$m"), "On 11");
    assert_eq!(date.replace_all("On 2026-11-19", "${y}_$m"), "On 2026_11");
}
