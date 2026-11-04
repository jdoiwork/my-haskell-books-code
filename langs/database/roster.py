# The Python code in the database chapter.
# データベース入門の章の Python のコード
import sqlite3

# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
conn = sqlite3.connect(":memory:")
conn.execute("""
    CREATE TABLE adventurers
      ( id    INTEGER PRIMARY KEY
      , name  TEXT    NOT NULL UNIQUE
      , job   TEXT    NOT NULL
      , level INTEGER NOT NULL
      )
""")
conn.execute("INSERT INTO adventurers (name, job, level) VALUES (?, ?, ?)",
             ("Lambda", "Mage", 3))
conn.execute("INSERT INTO adventurers (name, job, level) VALUES (?, ?, ?)",
             ("Python", "Archer", 5))
conn.execute("UPDATE adventurers SET level = level + 1 WHERE name = ?",
             ("Lambda",))
conn.execute("DELETE FROM adventurers WHERE name = ?", ("Python",))
for row in conn.execute("SELECT id, name, job, level FROM adventurers"):
    print(row)  # (1, 'Lambda', 'Mage', 4)
# ANCHOR_END: procedural
assert list(conn.execute("SELECT id, name, job, level FROM adventurers")) \
    == [(1, "Lambda", "Mage", 4)]

# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
from dataclasses import dataclass

@dataclass(frozen=True)
class Adventurer:
    id: int
    name: str
    job: str
    level: int

def list_adventurers(conn):
    rows = conn.execute("SELECT id, name, job, level FROM adventurers")
    return [Adventurer(*row) for row in rows]

print(list_adventurers(conn))
# [Adventurer(id=1, name='Lambda', job='Mage', level=4)]
# ANCHOR_END: functional
assert list_adventurers(conn) == [Adventurer(1, "Lambda", "Mage", 4)]

# The ? keeps a strange name from becoming SQL.
# ? のおかげで、変な名前が SQL にならない
evil = "x' OR '1'='1"
assert list(conn.execute(
    "SELECT * FROM adventurers WHERE name = ?", (evil,))) == []
assert len(list(conn.execute(
    "SELECT * FROM adventurers WHERE name = '" + evil + "'"))) == 1
