// Rust's main may give back a Result: an Err is written to the
// standard error with Debug ({:?}), and the exit code is 1.
// Rust の main は Result を返せる。Err は標準エラーに Debug（{:?}）で
// 書かれ、終了コードは 1 になる
// ANCHOR: main
// Rust (fn main() -> Result)
// Rust（Result を返す main）
fn main() -> Result<(), String> {
    println!("the shop opens");
    let orders: Option<&str> = None;
    let text = orders.ok_or("orders.txt: no such file".to_string())?;
    println!("{text}");
    Ok(())
}
// ANCHOR_END: main
