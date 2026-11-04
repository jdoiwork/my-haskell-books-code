// The Rust code in the DB-independent design chapter.
// DB に縛られない設計の章の Rust のコード
use std::collections::HashMap;

// ANCHOR: trait
// Rust (a trait, and a struct for each store)
// Rust（トレイトと、置き場ごとの構造体）
#[derive(Clone, Debug, PartialEq)]
struct Adventurer { id: u32, name: String, job: String, level: u32 }

#[derive(Debug, PartialEq)]
enum AddError { NameTaken(String) }

trait RosterStore {
    fn add_adventurer(&mut self, name: &str, job: &str, level: u32)
        -> Result<u32, AddError>;
    fn list_adventurers(&self) -> Vec<Adventurer>;
}

#[derive(Default)]
struct MemoryStore { by_name: HashMap<String, Adventurer>, next_id: u32 }

impl RosterStore for MemoryStore {
    fn add_adventurer(&mut self, name: &str, job: &str, level: u32)
        -> Result<u32, AddError> {
        if self.by_name.contains_key(name) {
            return Err(AddError::NameTaken(name.to_string()));
        }
        self.next_id += 1;
        let a = Adventurer {
            id: self.next_id, name: name.to_string(),
            job: job.to_string(), level,
        };
        self.by_name.insert(name.to_string(), a);
        Ok(self.next_id)
    }

    fn list_adventurers(&self) -> Vec<Adventurer> {
        let mut all: Vec<Adventurer> =
            self.by_name.values().cloned().collect();
        all.sort_by_key(|a| a.id);
        all
    }
}

// The guild's work only knows the trait.
// ギルドの仕事は、トレイトだけを知っている
fn enlist(store: &mut dyn RosterStore, name: &str, job: &str, level: u32)
    -> String {
    match store.add_adventurer(name, job, level) {
        Ok(id) => format!("Welcome, {name}! (no. {id})"),
        Err(AddError::NameTaken(who)) => format!("The name {who} is taken."),
    }
}
// ANCHOR_END: trait

fn main() {
    let mut store = MemoryStore::default();
    assert_eq!(enlist(&mut store, "Lambda", "Mage", 3),
               "Welcome, Lambda! (no. 1)");
    assert_eq!(enlist(&mut store, "Lambda", "Mage", 3),
               "The name Lambda is taken.");
    assert_eq!(store.list_adventurers().len(), 1);
    println!("ok");
}
