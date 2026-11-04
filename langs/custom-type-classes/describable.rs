// The Rust code in the custom type classes chapter.
// 型クラスを作る章の Rust のコード
// ANCHOR: trait
// Rust (trait)
// Rust（トレイト）
trait Describable {
    fn describe(&self) -> String;
}

impl Describable for bool {
    fn describe(&self) -> String {
        if *self { "yes, indeed".into() } else { "no way".into() }
    }
}

fn announce<T: Describable>(x: &T) -> String {
    format!("Look! {}", x.describe())
}
// ANCHOR_END: trait
fn main() {
assert_eq!(announce(&true), "Look! yes, indeed");
assert_eq!(announce(&false), "Look! no way");
}
