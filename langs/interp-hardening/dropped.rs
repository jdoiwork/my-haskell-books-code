// Rust: a Result thrown away is a warning (#[must_use]), not an error.
// Rust：捨てた Result は警告（#[must_use]）。エラーではない
use std::fs;

// ANCHOR: dropped
// Rust
fn main() {
    // warning: unused `Result` that must be used
    fs::copy("no-such-memos.txt", "backup-memos.txt");
    // Thrown away on purpose: no warning.
    // わざと捨てる：警告なし
    let _ = fs::remove_file("no-such-old.txt");
    // ANCHOR_END: dropped
    assert!(fs::metadata("backup-memos.txt").is_err());
    println!("done");
}
