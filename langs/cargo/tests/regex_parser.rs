// The Rust code in the parser chapter of MiniRegex: how the regex crate
// tells a wrong pattern, and what it does with a { that is not a
// quantifier.
// MiniRegex のパーサーの章の Rust のコード：regex クレートが、まちがった
// パターンをどう知らせるか。量指定子でない { をどう扱うか
use regex::Regex;

#[test]
fn points_at_the_trouble() {
    // ANCHOR: error
    // Rust: the error of Regex::new shows the pattern, with ^ under it
    // Rust：Regex::new のエラーは、パターンと、その下の ^ を見せる
    match Regex::new(r"a{5,2}") {
        Ok(_) => println!("compiled"),
        Err(e) => println!("{e}"),
    }
    // ANCHOR_END: error
    let message = Regex::new(r"a{5,2}").unwrap_err().to_string();
    println!("{message}");
    assert!(message.contains("a{5,2}"));
    assert!(message.contains("^^^^^"));
}

#[test]
fn takes_a_brace_strictly() {
    // ANCHOR: braces
    // Rust: a { must start a quantifier; x{a} and a{,3} are errors
    // Rust：{ は量指定子の始まりでなければならない。x{a} も a{,3} もエラー
    for p in [r"x{a}", r"a{,3}", r"a{ 2 , 3 }"] {
        match Regex::new(p) {
            Ok(_) => println!("{p}: a pattern"),
            Err(_) => println!("{p}: an error"),
        }
    }
    // ANCHOR_END: braces
    assert!(Regex::new(r"x{a}").is_err());
    assert!(Regex::new(r"a{,3}").is_err());
    assert!(Regex::new(r"a{").is_err());
    assert!(Regex::new(r"{").is_err());
    assert!(Regex::new(r"^a{ 2 , 3 }$").unwrap().is_match("aa"));
    // a** is (a*)* in Rust; PCRE2 refuses it
    // Rust では a** は (a*)*。PCRE2 は断る
    assert!(Regex::new(r"a**").is_ok());
}
