// Scouts sent out at the same time, for the chapter
// "Structured concurrency".
// 同時に送り出す偵察隊。「構造化された並行性」の章
use std::sync::Mutex;
use std::thread;
use std::time::Duration;

fn scout(report: &Mutex<Vec<String>>, name: &str, millis: u64) {
    thread::sleep(Duration::from_millis(millis));
    report.lock().unwrap().push(name.to_string());
}

// ANCHOR: scope
// Rust (std::thread::scope: every thread joins before the scope ends)
// Rust（std::thread::scope：スコープを出る前に、全部のスレッドを待つ）
fn party(report: &Mutex<Vec<String>>) {
    thread::scope(|s| {
        s.spawn(|| scout(report, "slime", 50));
        s.spawn(|| scout(report, "goblin", 100));
    });
}
// ANCHOR_END: scope

fn main() {
    let report = Mutex::new(Vec::new());
    party(&report);
    // Both are back by the time party returns.
    // party から戻ったときには、2人とも帰っている
    assert_eq!(*report.lock().unwrap(), vec!["slime", "goblin"]);
    println!("ok");
}
