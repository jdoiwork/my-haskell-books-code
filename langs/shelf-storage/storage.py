# The shelf in SQLite with Python's sqlite3, for the chapter
# "The window and the records: what we can do as a type, kept in SQLite".
# 「窓口と記録」の章の、SQLite の本棚を Python の sqlite3 で
import sqlite3

# ANCHOR: procedural
# Python (procedural style: for loops inside one transaction)
# Python（手続き型スタイル：1つのトランザクションの中で for 文）
SCHEMA = """
CREATE TABLE works (id INTEGER PRIMARY KEY AUTOINCREMENT,
                    title TEXT NOT NULL);
CREATE TABLE entries (
  id        INTEGER PRIMARY KEY AUTOINCREMENT,
  work_id   INTEGER NOT NULL REFERENCES works   ON DELETE CASCADE,
  parent_id INTEGER          REFERENCES entries ON DELETE CASCADE,
  position  INTEGER NOT NULL,
  heading   TEXT    NOT NULL,
  size      INTEGER NOT NULL CHECK (size >= 0));
CREATE TABLE notes (id INTEGER PRIMARY KEY AUTOINCREMENT,
  work_id INTEGER NOT NULL REFERENCES works ON DELETE CASCADE,
  body    TEXT    NOT NULL);
"""


# 🌿 Insert the children of one parent, and their children in turn.
# 1つの親の子を入れ、その子の子も順に入れる
def insert_entries(conn, work_id, parent_id, children):
    for position, (heading, size, sub) in enumerate(children):
        cur = conn.execute(
            "INSERT INTO entries (work_id, parent_id, position,"
            " heading, size) VALUES (?, ?, ?, ?, ?)",
            (work_id, parent_id, position, heading, size))
        insert_entries(conn, work_id, cur.lastrowid, sub)


# 📚 Add a work and its tree: all of it, or none of it.
# 作品と目次の木を足す：全部か、何もなしか
def add_work(conn, title, toc):
    with conn:  # COMMIT at the end, ROLLBACK on an error
        cur = conn.execute("INSERT INTO works (title) VALUES (?)",
                           (title,))
        work_id = cur.lastrowid  # the number the DB gave / DB が付けた番号
        insert_entries(conn, work_id, None, toc)
    return work_id


# 🌲 Read the tree back from the roots, with depths, in order.
# 根から木を読み戻す。深さつきで、順番どおりに
TOC = """
WITH RECURSIVE toc (id, depth, heading, path) AS (
  SELECT id, 0, heading, printf('%06d', position) FROM entries
   WHERE work_id = ? AND parent_id IS NULL
  UNION ALL
  SELECT e.id, toc.depth + 1, e.heading,
         toc.path || '.' || printf('%06d', e.position)
    FROM entries AS e JOIN toc ON e.parent_id = toc.id)
SELECT depth, heading FROM toc ORDER BY path
"""

conn = sqlite3.connect(":memory:")
conn.execute("PRAGMA foreign_keys = ON")  # off by default! / 既定では切!
conn.executescript(SCHEMA)
# ANCHOR_END: procedural


# The book from the chapter: (heading, size, children).
# 章の本：(見出し, 大きさ, 子)
DRAGONS = [
    ("第1部 ドラゴンを知る", 2, [
        ("第1章 見分け方", 4, [
            ("1.1 うろこの色", 8, []),
            ("1.2 火を吐くもの", 10, []),
        ]),
        ("第2章 すみかと季節", 24, []),
    ]),
    ("第2部 ドラゴンと話す", 2, [
        ("第3章 あいさつの作法", 16, []),
        ("第4章 怒らせたときは", 12, []),
    ]),
]

w1 = add_work(conn, "はじめてのドラゴン学", DRAGONS)
assert w1 == 1
toc = conn.execute(TOC, (w1,)).fetchall()
assert [d for d, _ in toc] == [0, 1, 2, 2, 1, 0, 1, 1], toc
assert [h for _, h in toc] == [
    "第1部 ドラゴンを知る", "第1章 見分け方", "1.1 うろこの色",
    "1.2 火を吐くもの", "第2章 すみかと季節", "第2部 ドラゴンと話す",
    "第3章 あいさつの作法", "第4章 怒らせたときは"], toc
assert conn.execute("SELECT count(*) FROM entries").fetchone() == (8,)

# Deleting a work removes its entries and notes (ON DELETE CASCADE).
# 作品を消すと、その目次と感想も消える
conn.execute("INSERT INTO notes (work_id, body) VALUES (?, ?)",
             (w1, "うろこの色の図がきれい"))
with conn:
    conn.execute("DELETE FROM works WHERE id = ?", (w1,))
assert conn.execute("SELECT count(*) FROM notes").fetchone() == (0,)
assert conn.execute("SELECT count(*) FROM entries").fetchone() == (0,)

# Numbers are never used again (AUTOINCREMENT).
# 番号は使い回さない
w2 = add_work(conn, "はじめてのドラゴン学", DRAGONS)
assert w2 == 2, w2

# A negative size in the third entry: CHECK fails, nothing is left.
# 3つ目の見出しの大きさが負：CHECK で止まり、何も残らない
bad = [("第1部", 2, [("第1章", 4, []), ("第2章", -1, [])])]
try:
    add_work(conn, "こわれた本", bad)
    raise AssertionError("CHECK should fail")
except sqlite3.IntegrityError as e:
    assert "CHECK" in str(e), e
assert conn.execute("SELECT count(*) FROM works").fetchone() == (1,)
assert conn.execute("SELECT count(*) FROM entries").fetchone() == (8,)

# Without the PRAGMA, the notes would stay behind.
# PRAGMA がなければ、感想は残ってしまう
loose = sqlite3.connect(":memory:")
loose.executescript(SCHEMA)
lw = add_work(loose, "はじめてのドラゴン学", DRAGONS)
loose.execute("INSERT INTO notes (work_id, body) VALUES (?, 'x')", (lw,))
loose.execute("DELETE FROM works WHERE id = ?", (lw,))
assert loose.execute("SELECT count(*) FROM notes").fetchone() == (1,)
