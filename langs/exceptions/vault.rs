// The Rust code in the "IO exceptions" chapter.
// IO の例外をちゃんと扱うの章の Rust のコード
use std::cell::RefCell;

thread_local! {
    static LOG: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
}

fn note(s: &str) {
    LOG.with(|log| log.borrow_mut().push(s.to_string()));
}

fn take_log() -> String {
    LOG.with(|log| log.borrow_mut().drain(..).collect::<Vec<_>>().join("|"))
}

#[derive(Debug)]
enum VaultAccident {
    GoblinAlarm,
}

fn count_coins() -> Result<(), VaultAccident> {
    note("counting: 1, 2, 3, ...");
    Err(VaultAccident::GoblinAlarm)
}

// ANCHOR: drop
// Rust (a guard: Drop closes the door when it goes out of scope)
// Rust（見張り役の値：スコープを出るときに、Drop が扉を閉める）
struct OpenDoor;

impl Drop for OpenDoor {
    fn drop(&mut self) {
        note("door: closed");
    }
}

fn open_door() -> OpenDoor {
    note("door: open");
    OpenDoor
}

fn night_shift() -> Result<(), VaultAccident> {
    let _door = open_door();
    count_coins()?; // returns early with the Err / Err なら、ここで抜ける
    note("counting: done");
    Ok(())
}
// ANCHOR_END: drop

fn main() {
    let result = night_shift();
    assert!(matches!(result, Err(VaultAccident::GoblinAlarm)));
    assert_eq!(take_log(), "door: open|counting: 1, 2, 3, ...|door: closed");

    // A panic also runs Drop on the way out.
    // panic のときも、抜けていく途中で Drop が動く
    std::panic::set_hook(Box::new(|_| {}));
    let caught = std::panic::catch_unwind(|| {
        let _door = open_door();
        panic!("goblins!");
    });
    assert!(caught.is_err());
    assert_eq!(take_log(), "door: open|door: closed");
    println!("ok");
}
