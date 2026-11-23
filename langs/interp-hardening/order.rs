// Rust: items (fn, struct, ...) may be in any order, and may call each
// other. A local fn may be generic but cannot use local variables; a
// closure may use them but has one type.
// Rust：アイテム（fn、struct など）はどの順でもよく、おたがいを呼べる。
// 中の fn はジェネリックにできるが、ローカルな変数を使えない。クロージャー
// は使えるが、型は1つ

// ANCHOR: order
// Rust
fn main() {
    println!("{} {}", is_even(10), is_odd(7));
    println!("{}", row(3, "lam"));
    println!("{}", tagged("#", 3, "lam"));
    assert_eq!(row(3, "lam"), "[3] | [lam]");
    assert_eq!(tagged("#", 3, "lam"), "#:3 #:lam");
}

fn is_even(n: u32) -> bool {
    n == 0 || is_odd(n - 1)
}

fn is_odd(n: u32) -> bool {
    n != 0 && is_even(n - 1)
}
// ANCHOR_END: order

// ANCHOR: local
// A local fn: generic, and closed (it cannot see n or name).
// 中の fn：ジェネリックで、閉じている（n も name も見えない）
fn row(n: i32, name: &str) -> String {
    fn cell<T: std::fmt::Display>(x: T) -> String {
        format!("[{x}]")
    }
    cell(n) + " | " + &cell(name)
}

// A closure sees tag, but has one type: so two closures, or a fn that
// takes tag as an argument.
// クロージャーは tag が見えるが、型は1つ。なので、クロージャーを2つにするか、
// tag を引数にする fn にする
fn tagged(tag: &str, n: i32, name: &str) -> String {
    fn label<T: std::fmt::Display>(tag: &str, x: T) -> String {
        format!("{tag}:{x}")
    }
    label(tag, n) + " " + &label(tag, name)
}
// ANCHOR_END: local
