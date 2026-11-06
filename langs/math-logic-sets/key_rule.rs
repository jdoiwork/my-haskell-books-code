// The guild's key rule of the chapter "Logic and sets", in Rust.
// 「論理と集合」の章のギルドの鍵の決まりを、Rust で

// ANCHOR: functional
// Rust
fn key_rule_ok(level: u32, has_key: bool) -> bool {
    !has_key || level >= 10
}
// ANCHOR_END: functional

fn main() {
    assert!(key_rule_ok(15, false));
    assert!(key_rule_ok(12, true));
    assert!(!key_rule_ok(5, true));
    assert!(key_rule_ok(5, false));
    println!("ok");
}
