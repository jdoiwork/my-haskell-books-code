// Read a big file one line at a time, and bytes that are not UTF-8, in
// Rust. The test makes its own file in the temporary folder.
// 大きなファイルを1行ずつ読む。UTF-8 でないバイトも。Rust で。テストは、
// 一時フォルダーに自分でファイルを作る
use std::collections::BTreeMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Write};

// ANCHOR: procedural
// Rust (procedural style)
// Rust（手続き型スタイル）
fn count_statuses(path: &str) -> std::io::Result<BTreeMap<String, u64>> {
    let mut counts = BTreeMap::new();
    let reader = BufReader::new(File::open(path)?);
    for line in reader.lines() {
        // A line that is not UTF-8 is an Err here.
        // UTF-8 でない行は、ここで Err
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() == 5 {
            *counts.entry(parts[3].to_string()).or_insert(0) += 1;
        }
    }
    Ok(counts)
}
// ANCHOR_END: procedural

// ANCHOR: decode
// Rust: from_utf8 says where; from_utf8_lossy puts U+FFFD
// Rust：from_utf8 は場所を言い、from_utf8_lossy は U+FFFD を置く
fn decode_both(raw: &[u8]) -> (Result<String, usize>, String) {
    let strict = String::from_utf8(raw.to_vec())
        .map_err(|e| e.utf8_error().valid_up_to());
    let lossy = String::from_utf8_lossy(raw).into_owned();
    (strict, lossy)
}
// ANCHOR_END: decode

fn main() {
    let (strict, lossy) = decode_both(&[99, 97, 102, 233]);
    assert_eq!(strict, Err(3));
    assert_eq!(lossy, "caf\u{FFFD}");

    let path = std::env::temp_dir().join("lam-streams-rust.log");
    let path = path.to_str().unwrap().to_string();
    let statuses = ["200", "200", "304", "404", "200", "500"];
    {
        let mut f = std::io::BufWriter::new(File::create(&path).unwrap());
        for i in 0..200000 {
            writeln!(f, "t GET / {} 1ms", statuses[i % 6]).unwrap();
        }
    }
    let counts = count_statuses(&path).unwrap();
    assert_eq!(counts["200"], 100001);
    assert_eq!(counts["500"], 33333);

    // A broken line is an Err of lines().
    // 壊れた行は、lines() の Err
    std::fs::write(&path, b"a 1 2 200 x\ncaf\xe9\n").unwrap();
    assert!(count_statuses(&path).is_err());
    std::fs::remove_file(&path).unwrap();
    println!("ok");
}
