// The Rust code in the laziness chapter.
// 遅延評価の章の Rust のコード
fn main() {
    // ANCHOR: lazy
    // Rust (an iterator: nothing runs until collect pulls values)
    // Rust（イテレーター：collect が値を引き出すまで、何も動かない）
    let first3: Vec<u64> = (1..)
        .map(|n| {
            println!("making {}", n * 2);
            n * 2
        })
        .take(3)
        .collect();
    // ANCHOR_END: lazy
    assert_eq!(first3, vec![2, 4, 6]);

    // ANCHOR: strings
    // Rust (len counts UTF-8 bytes)
    // Rust（len は UTF-8 のバイト数）
    assert_eq!("冒険".len(), 6);
    assert_eq!("😀".len(), 4);
    assert_eq!("😀".chars().count(), 1);
    // ANCHOR_END: strings
    println!("ok");
}
