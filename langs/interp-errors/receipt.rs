// The Rust code in the chapter on ? and cleaning up: ? sends an Err
// back at once, and changes its type with From; Drop cleans up at the
// end of a scope, the last made first.
// ? と後始末の章の Rust のコード：? は Err をすぐに返し、From で型を
// 変える。Drop は、スコープの終わりに、後に作ったものから片付ける
use std::cell::RefCell;
use std::num::ParseIntError;

// ANCHOR: question
// Rust (?): each ? gives back an Err at once
// Rust（?）：? のたびに、Err ならすぐに返す
fn total(read: impl Fn(&str) -> Result<String, String>) -> Result<u64, String> {
    let text = read("orders.txt")?;
    let mut sum = 0;
    for word in text.split_whitespace() {
        let n: u64 = word.parse().map_err(|e: ParseIntError| e.to_string())?;
        sum += n;
    }
    Ok(sum)
}
// ANCHOR_END: question

// ANCHOR: from
// Rust (From): ? turns a ParseIntError into a ShopError by itself
// Rust（From）：? が、ParseIntError を ShopError にひとりでに変える
#[derive(Debug, PartialEq)]
enum ShopError {
    NoFile(String),
    BadNumber(String),
}

impl From<ParseIntError> for ShopError {
    fn from(e: ParseIntError) -> Self {
        ShopError::BadNumber(e.to_string())
    }
}

fn total_of(text: Option<&str>) -> Result<u64, ShopError> {
    let text = text.ok_or(ShopError::NoFile("orders.txt".to_string()))?;
    let mut sum = 0;
    for word in text.split_whitespace() {
        sum += word.parse::<u64>()?; // From<ParseIntError> is called here
    }
    Ok(sum)
}
// ANCHOR_END: from

// ANCHOR: drop
// Rust (Drop): the shop closes when `shop` goes out of its scope
// Rust（Drop）：shop がスコープを出るときに、お店が閉まる
struct Shop<'a> {
    name: &'a str,
    log: &'a RefCell<Vec<String>>,
}

impl Drop for Shop<'_> {
    fn drop(&mut self) {
        self.log.borrow_mut().push(format!("{} closes", self.name));
    }
}

fn open_and_count(log: &RefCell<Vec<String>>, text: &str) -> Result<u64, ShopError> {
    let _shop = Shop { name: "the shop", log };
    let _register = Shop { name: "the register", log };
    let sum = total_of(Some(text))?; // an Err here still closes both
    log.borrow_mut().push(format!("total {sum}"));
    Ok(sum)
}
// ANCHOR_END: drop

fn main() {
    let read = |name: &str| match name {
        "orders.txt" => Ok("4 1 3".to_string()),
        _ => Err(format!("{name}: no such file")),
    };
    assert_eq!(total(read), Ok(8));
    let broken = |_: &str| Ok("4 one 3".to_string());
    assert_eq!(total(broken), Err("invalid digit found in string".to_string()));
    let missing = |name: &str| Err(format!("{name}: no such file"));
    assert_eq!(total(missing), Err("orders.txt: no such file".to_string()));

    assert_eq!(total_of(Some("4 1 3")), Ok(8));
    assert_eq!(
        total_of(Some("4 x")),
        Err(ShopError::BadNumber("invalid digit found in string".to_string()))
    );
    assert_eq!(total_of(None), Err(ShopError::NoFile("orders.txt".to_string())));

    // Dropped in reverse order of making, after an Err too.
    // 作ったのと逆の順に片付く。Err のあとでも
    let log = RefCell::new(Vec::new());
    assert_eq!(open_and_count(&log, "4 1 3"), Ok(8));
    assert!(open_and_count(&log, "4 x").is_err());
    assert_eq!(
        *log.borrow(),
        vec![
            "total 8",
            "the register closes",
            "the shop closes",
            "the register closes",
            "the shop closes"
        ]
    );
    println!("ok receipt.rs");
}
