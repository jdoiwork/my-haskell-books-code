// The Rust code in the database chapter (rusqlite).
// データベース入門の章の Rust のコード（rusqlite）

// ANCHOR: rusqlite
// Rust
use rusqlite::{params, Connection, Result};

#[derive(Debug, PartialEq)]
struct Adventurer {
    id: i64,
    name: String,
    job: String,
    level: i64,
}

fn list_adventurers(conn: &Connection) -> Result<Vec<Adventurer>> {
    let mut stmt =
        conn.prepare("SELECT id, name, job, level FROM adventurers")?;
    let rows = stmt.query_map([], |row| {
        Ok(Adventurer {
            id: row.get(0)?,
            name: row.get(1)?,
            job: row.get(2)?,
            level: row.get(3)?,
        })
    })?;
    rows.collect()
}

fn roster() -> Result<Vec<Adventurer>> {
    let conn = Connection::open_in_memory()?;
    conn.execute(
        "CREATE TABLE adventurers (id INTEGER PRIMARY KEY, \
         name TEXT NOT NULL UNIQUE, job TEXT NOT NULL, \
         level INTEGER NOT NULL)",
        [],
    )?;
    conn.execute(
        "INSERT INTO adventurers (name, job, level) VALUES (?1, ?2, ?3)",
        params!["Lambda", "Mage", 3],
    )?;
    conn.execute(
        "INSERT INTO adventurers (name, job, level) VALUES (?1, ?2, ?3)",
        params!["Python", "Archer", 5],
    )?;
    conn.execute(
        "UPDATE adventurers SET level = level + 1 WHERE name = ?1",
        params!["Lambda"],
    )?;
    conn.execute(
        "DELETE FROM adventurers WHERE name = ?1",
        params!["Python"],
    )?;
    list_adventurers(&conn)
}
// ANCHOR_END: rusqlite

#[test]
fn roster_works() {
    let expected = vec![Adventurer {
        id: 1,
        name: "Lambda".to_string(),
        job: "Mage".to_string(),
        level: 4,
    }];
    assert_eq!(roster().unwrap(), expected);
}
