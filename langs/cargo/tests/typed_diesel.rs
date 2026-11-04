// The Rust code in the "typed SQL" chapter (Diesel).
// 「型で SQL を守る」の章の Rust のコード（Diesel）

// ANCHOR: diesel
// Rust
use diesel::prelude::*;
use diesel::sqlite::SqliteConnection;

diesel::table! {
    adventurers (id) {
        id -> Integer,
        name -> Text,
        job -> Text,
        level -> Integer,
    }
}

#[derive(Queryable, Debug, PartialEq)]
struct Adventurer {
    id: i32,
    name: String,
    job: String,
    level: i32,
}

#[derive(Insertable)]
#[diesel(table_name = adventurers)]
struct NewAdventurer<'a> {
    name: &'a str,
    job: &'a str,
    level: i32,
}

fn strong(
    conn: &mut SqliteConnection,
    lv: i32,
) -> QueryResult<Vec<Adventurer>> {
    adventurers::table
        .filter(adventurers::level.ge(lv))
        .order(adventurers::level.desc())
        .load(conn)
}
// ANCHOR_END: diesel

fn setup() -> SqliteConnection {
    let mut conn = SqliteConnection::establish(":memory:").unwrap();
    diesel::sql_query(
        "CREATE TABLE adventurers (id INTEGER PRIMARY KEY, \
         name TEXT NOT NULL UNIQUE, job TEXT NOT NULL, \
         level INTEGER NOT NULL)",
    )
    .execute(&mut conn)
    .unwrap();
    conn
}

#[test]
fn diesel_crud_works() {
    let mut conn = setup();
    diesel::insert_into(adventurers::table)
        .values(&vec![
            NewAdventurer { name: "Lambda", job: "Mage", level: 3 },
            NewAdventurer { name: "Python", job: "Archer", level: 5 },
            NewAdventurer { name: "Rust", job: "Knight", level: 1 },
        ])
        .execute(&mut conn)
        .unwrap();
    diesel::update(adventurers::table.filter(adventurers::name.eq("Lambda")))
        .set(adventurers::level.eq(adventurers::level + 1))
        .execute(&mut conn)
        .unwrap();
    diesel::delete(adventurers::table.filter(adventurers::name.eq("Rust")))
        .execute(&mut conn)
        .unwrap();
    let rows = strong(&mut conn, 3).unwrap();
    let names: Vec<&str> = rows.iter().map(|a| a.name.as_str()).collect();
    assert_eq!(names, vec!["Python", "Lambda"]);
    assert_eq!(rows[1].level, 4);
}
