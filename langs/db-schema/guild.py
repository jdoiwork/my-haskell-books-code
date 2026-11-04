# The Python code in the "growing the database" chapter.
# 「データベースを育てる」の章の Python のコード
import sqlite3

MIGRATIONS = [
    "CREATE TABLE adventurers (id INTEGER PRIMARY KEY,"
    " name TEXT NOT NULL UNIQUE, job TEXT NOT NULL, level INTEGER NOT NULL)",
    "CREATE TABLE quests (id INTEGER PRIMARY KEY, title TEXT NOT NULL,"
    " reward INTEGER NOT NULL, taken_by INTEGER REFERENCES adventurers (id),"
    " done INTEGER NOT NULL DEFAULT 0)",
    "ALTER TABLE adventurers ADD COLUMN gold INTEGER NOT NULL DEFAULT 0",
]

# ANCHOR: migrate
# Python (procedural style)
# Python（手続き型スタイル）
def migrate(conn):
    current = conn.execute("PRAGMA user_version").fetchone()[0]
    for version, step in enumerate(MIGRATIONS, start=1):
        if version <= current:
            continue
        with conn:  # one transaction for each step
            conn.execute(step)
            conn.execute(f"PRAGMA user_version = {version}")
    return conn.execute("PRAGMA user_version").fetchone()[0]
# ANCHOR_END: migrate

# ANCHOR: complete
# Python (procedural style)
# Python（手続き型スタイル）
def complete_quest(conn, quest_id):
    with conn:  # all or nothing
        row = conn.execute(
            "SELECT taken_by, reward, done FROM quests WHERE id = ?",
            (quest_id,)).fetchone()
        if row is None:
            raise ValueError(f"there is no quest {quest_id}")
        taken_by, reward, done = row
        if done:
            raise ValueError(f"quest {quest_id} is already done")
        if taken_by is None:
            raise ValueError(f"no one has taken quest {quest_id}")
        conn.execute("UPDATE quests SET done = 1 WHERE id = ?", (quest_id,))
        conn.execute("UPDATE adventurers SET gold = gold + ? WHERE id = ?",
                     (reward, taken_by))
        return reward
# ANCHOR_END: complete

conn = sqlite3.connect(":memory:")
assert migrate(conn) == 3
assert migrate(conn) == 3
conn.execute("INSERT INTO adventurers (name, job, level) VALUES (?, ?, 1)",
             ("Lambda", "Mage"))
conn.execute("INSERT INTO quests (title, reward, taken_by) VALUES (?, ?, 1)",
             ("Slay the slime", 50))
conn.commit()
assert complete_quest(conn, 1) == 50
assert conn.execute("SELECT gold FROM adventurers").fetchone()[0] == 50
for bad in (1, 9):
    try:
        complete_quest(conn, bad)
        raise AssertionError("should fail")
    except ValueError:
        pass

# A failure in the middle of `with conn:` is rolled back.
# with conn: の途中で失敗すると、元に戻る
try:
    with conn:
        conn.execute("UPDATE adventurers SET gold = gold + 100")
        raise RuntimeError("the guild caught fire")
except RuntimeError:
    pass
assert conn.execute("SELECT gold FROM adventurers").fetchone()[0] == 50
print("ok")
