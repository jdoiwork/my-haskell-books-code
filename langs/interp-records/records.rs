// The Rust code in the chapter on type annotations and records.
// 型の注釈とレコードの章の Rust のコード

// ANCHOR: update
// Rust (a struct, and the struct update syntax)
// Rust（構造体と、構造体の更新の構文）
#[derive(Debug, Clone, PartialEq)]
struct Member {
    name: String,
    lang: String,
    days: u32,
}

// ..m takes the other fields from m. name and lang are moved out of
// m, so m cannot be used after this (it is taken by value here).
// ..m は、ほかのフィールドを m から取る。name と lang は m から
// ムーブするので、このあと m は使えない（ここでは値で受け取る）
fn next_day(m: Member) -> Member {
    Member { days: m.days + 1, ..m }
}
// ANCHOR_END: update

// ANCHOR: trait
// Rust ("anything with a name" is a trait, written by hand)
// Rust（「name を持つものなら何でも」は、自分で書くトレイト）
trait Named {
    fn name(&self) -> &str;
}

impl Named for Member {
    fn name(&self) -> &str {
        &self.name
    }
}

fn label(r: &impl Named) -> String {
    format!("[{}]", r.name())
}
// ANCHOR_END: trait

fn main() {
    let lam = Member { name: "Lam".into(), lang: "lam".into(), days: 19 };
    let older = next_day(lam.clone());
    assert_eq!(older.days, 20);
    assert_eq!(lam.days, 19);
    assert_eq!(label(&older), "[Lam]");
    println!("{} {:?}", label(&older), older);
}
