// The Rust code in the chapter on the VM with ST: the regex crate's
// bounded backtracker keeps one stack, with "go on here" and "put this
// group back" frames on it, and never tries a state twice at a place.
// ST の VM の章の Rust のコード：regex クレートの BoundedBacktracker は、
// 「ここから続ける」と「このグループを書き戻す」を同じ1つのスタックに
// 積み、同じ場所で同じ状態を2度試さない
use regex_automata::nfa::thompson::backtrack::BoundedBacktracker;
use std::time::Instant;

#[test]
fn answers_the_name_field_at_once() {
    // ANCHOR: bounded
    // Rust: the name field of the window, with 8 names and a "!"
    // Rust：窓口の名前の欄。名前が8つと「!」
    let re = BoundedBacktracker::new(r"^(\w+\s?)+$").unwrap();
    let mut cache = re.create_cache();
    let field = "Mira Ruby Lam GHC Perl Mira Ruby Lam!";
    println!("{:?}", re.try_is_match(&mut cache, field));
    println!("{}", re.max_haystack_len());
    // ANCHOR_END: bounded
    let start = Instant::now();
    assert_eq!(re.try_is_match(&mut cache, field), Ok(false));
    assert!(start.elapsed().as_secs_f64() < 1.0);
}

#[test]
fn says_no_to_a_long_text() {
    let re = BoundedBacktracker::new(r"^(\w+\s?)+$").unwrap();
    let mut cache = re.create_cache();
    // ANCHOR: toolong
    // Rust: a text longer than that is not searched at all
    // Rust：それより長い文字列は、探さずに断る
    let long = "a".repeat(re.max_haystack_len() + 1);
    println!("{:?}", re.try_is_match(&mut cache, &long));
    // ANCHOR_END: toolong
    assert!(re.try_is_match(&mut cache, &long).is_err());
}
