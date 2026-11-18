// The comparison code of the chapter "Tests and debugging: lam test".
// cargo test runs the functions marked #[test]. For a fake world, the
// function takes its input and output as arguments (any Read, any
// Write): a String and a Vec<u8> in the tests.
// 「テストとデバッグ：lam test」の章の比較のコード。cargo test は #[test]
// の付いた関数を動かす。偽物の世界のために、関数は入力と出力を引数に
// もらう（どの Read でも、どの Write でも）。テストでは String と Vec<u8>
use regex::Regex;
use std::collections::BTreeMap;
use std::io::{Read, Write};

// ANCHOR: code
// Rust (the code to test: the world comes in as arguments)
// Rust（テストするコード：世界は引数でもらう）
fn split_into_words(text: &str) -> Vec<String> {
    let word = Regex::new(r"[a-z']+").unwrap();
    let lower = text.to_lowercase();
    word.find_iter(&lower).map(|m| m.as_str().to_string()).collect()
}

fn wordfreq(
    mut input: impl Read,
    mut out: impl Write,
) -> std::io::Result<()> {
    let mut text = String::new();
    input.read_to_string(&mut text)?;
    let mut counts: BTreeMap<String, u32> = BTreeMap::new();
    for w in split_into_words(&text) {
        *counts.entry(w).or_insert(0) += 1;
    }
    let mut ranking: Vec<_> = counts.into_iter().collect();
    // A stable sort: the same count stays a to z.
    // 安定な並べ替え：同じ回数なら a から z のまま
    ranking.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
    for (word, n) in ranking.into_iter().take(3) {
        writeln!(out, "{word:<8}{n}")?;
    }
    Ok(())
}
// ANCHOR_END: code

// ANCHOR: tests
// Rust (cargo test)
// Rust（cargo test）
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keeps_dont_as_one_word() {
        assert_eq!(split_into_words("Don't panic"), ["don't", "panic"]);
    }

    #[test]
    fn reads_the_input() {
        let mut out = Vec::new();
        wordfreq("b a b\nc a b\n".as_bytes(), &mut out).unwrap();
        assert_eq!(String::from_utf8(out).unwrap(),
                   "b       3\na       2\nc       1\n");
    }
}
// ANCHOR_END: tests
