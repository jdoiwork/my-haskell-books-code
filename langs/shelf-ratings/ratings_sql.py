# Run the queries in ratings.sql with SQLite in memory, and check them.
# ratings.sql の問い合わせを、メモリ上の SQLite で動かして確かめる
import re
import sqlite3
from pathlib import Path

SQL = (Path(__file__).parent / "ratings.sql").read_text()


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
  id       INTEGER PRIMARY KEY,
  title    TEXT    NOT NULL,
  position INTEGER NOT NULL
);
CREATE TABLE ratings (
  work_id INTEGER NOT NULL REFERENCES works (id),
  points  INTEGER NOT NULL CHECK (points BETWEEN 0 AND 100)
);
""")

# The shelf (the game is not rated yet) and the ratings.
# 本棚（ゲームはまだ評価していない）と評価
WORKS = ["はじめてのドラゴン学", "図書室の司書に転生した件",
         "迷宮図書館の写本係", "王都の路地裏探偵", "最後のドラゴン",
         "ダンジョン司書"]
POINTS = [[90], [100], [100, 100, 90, 70, 100] * 6,
          [80, 70, 90, 80], [60], []]
for i, (title, ps) in enumerate(zip(WORKS, POINTS), start=1):
    db.execute("INSERT INTO works VALUES (?, ?, ?)", (i, title, i))
    db.executemany("INSERT INTO ratings VALUES (?, ?)",
                   [(i, p) for p in ps])

# The plain mean: the librarian (one vote) first, the copyist second.
# ふつうの平均：1票の司書が1位、写本係が2位
rows = db.execute(anchor("avg")).fetchall()
assert [r[0] for r in rows[:2]] == [WORKS[1], WORKS[2]], rows
assert rows[0][1:] == (100.0, 1) and rows[1][1:] == (92.0, 30), rows
assert len(rows) == 5  # the game has no ratings / ゲームは評価なし

# The Bayesian average.
# ベイズ平均
rows = db.execute(anchor("bayes")).fetchall()
assert [r[0] for r in rows] == [WORKS[i] for i in [2, 1, 0, 3, 4]], rows
expected = [3182 / 35, 87.0, 256 / 3, 742 / 9, 241 / 3]
for (_, score), e in zip(rows, expected):
    assert abs(score - e) < 1e-9, (score, e)

# Without 1.0 *, this shelf would still give the right usual points,
# as each work's mean is a whole number by chance. An integer division
# drops the fraction: 3 / 2 is 1, while 1.0 * 3 / 2 is 1.5.
# この本棚では作品ごとの平均がたまたま整数なので、1.0 * がなくても
# 合ってしまう。整数の割り算は端数を捨てる：3 / 2 は 1、1.0 * 3 / 2 は 1.5
assert db.execute("SELECT 3 / 2, 1.0 * 3 / 2").fetchone() == (1, 1.5)
print("ok")
