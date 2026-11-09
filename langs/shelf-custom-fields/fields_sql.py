# Run fields.sql with SQLite in memory, and check the answers.
# fields.sql を、メモリ上の SQLite で動かして確かめる
import json
import re
import sqlite3
from pathlib import Path

SQL = (Path(__file__).parent / "fields.sql").read_text()


# 🔖 The text between "-- ANCHOR: name" and "-- ANCHOR_END: name".
# 「-- ANCHOR: 名前」と「-- ANCHOR_END: 名前」のあいだの文
def anchor(name):
    m = re.search(rf"-- ANCHOR: {name}\n(.*?)-- ANCHOR_END: {name}\n",
                  SQL, re.S)
    assert m, name
    return m.group(1)


db = sqlite3.connect(":memory:")
db.executescript("""
CREATE TABLE works (
  id    INTEGER PRIMARY KEY,
  title TEXT NOT NULL,
  attrs TEXT NOT NULL DEFAULT '{}' CHECK (json_valid(attrs))
);
CREATE TABLE work_attrs (
  work_id INTEGER NOT NULL REFERENCES works (id),
  name    TEXT    NOT NULL,
  value   TEXT    NOT NULL,
  PRIMARY KEY (work_id, name)
);
""")

WORKS = [
    ("はじめてのドラゴン学", {"推し度": 4, "形": "紙"}),
    ("図書室の司書に転生した件", {"推し度": 5, "形": "Web"}),
    ("迷宮図書館の写本係", {"推し度": 3, "形": "Web"}),
    ("ダンジョン司書", {}),
]
for i, (title, attrs) in enumerate(WORKS, start=1):
    db.execute("INSERT INTO works VALUES (?, ?, ?)",
               (i, title, json.dumps(attrs, ensure_ascii=False)))
    db.executemany("INSERT INTO work_attrs VALUES (?, ?, ?)",
                   [(i, k, str(v)) for k, v in attrs.items()])

both = [WORKS[1][0], WORKS[2][0]]
assert [r[0] for r in db.execute(anchor("json"))] == both
assert [r[0] for r in db.execute(anchor("eav"))] == both

db.executescript(anchor("index"))
plan = db.execute("EXPLAIN QUERY PLAN SELECT title FROM works "
                  "WHERE json_extract(attrs, '$.推し度') >= 3").fetchall()
assert any("works_oshi" in row[-1] for row in plan), plan

# A JSON column keeps any shape: "すごく" goes in, and SQLite says
# text is bigger than any number, so it passes 推し度 >= 3.
# JSON の列は、どんな形でも受け取る。"すごく" も入り、SQLite では文字は
# どんな数より大きいので、推し度 >= 3 を通ってしまう
db.execute("INSERT INTO works VALUES (5, 'すごい本', ?)",
           ('{"推し度": "すごく"}',))
found = [r[0] for r in db.execute(
    "SELECT title FROM works WHERE json_extract(attrs, '$.推し度') >= 3")]
assert "すごい本" in found, found

db.executescript(anchor("rename"))
assert db.execute("SELECT attrs FROM works WHERE id = 2").fetchone()[0] \
    == '{"形":"Web","推し":5}'
assert db.execute("SELECT count(*) FROM work_attrs WHERE name = '推し'") \
    .fetchone()[0] == 3
print("ok")
