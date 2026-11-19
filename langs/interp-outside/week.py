# Saving a week of the forecast in one transaction, in Python with the
# standard library (sqlite3). The book shows the parts between the
# ANCHOR lines; the rest checks them with SQLite in memory.
# 1週間の予報を1つのトランザクションで貯める。Python の標準ライブラリ
# （sqlite3）で。本に載せるのは ANCHOR のあいだ。残りは、メモリーの
# SQLite で確かめる
import sqlite3

SCHEMA = (
    "CREATE TABLE weather (day TEXT PRIMARY KEY,"
    " pop INTEGER CHECK (pop BETWEEN 0 AND 100))"
)
INSERT = "INSERT INTO weather VALUES (?, ?)"


# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def save_week(con, week):
    try:
        for day, pop in week:
            con.execute(INSERT, (day, pop))
        con.commit()
    except sqlite3.Error:
        con.rollback()
        raise
# ANCHOR_END: procedural


# ANCHOR: with
# Python (with a context manager)
# Python（コンテキストマネージャーで）
def save_week_with(con, week):
    with con:  # COMMIT at the end, or ROLLBACK on an exception
        for day, pop in week:
            con.execute(INSERT, (day, pop))
# ANCHOR_END: with


# The check: the fourth day has 130%, which the table says no to.
# 確かめ：4日目は 130%。表が断る
odd = [("11-17", 100), ("11-18", 100), ("11-19", 6), ("11-20", 130)]
for save in (save_week, save_week_with):
    con = sqlite3.connect(":memory:")
    con.execute(SCHEMA)
    try:
        save(con, odd)
        raise AssertionError("it should fail")
    except sqlite3.IntegrityError as e:
        assert "CHECK constraint failed" in str(e)
    count = con.execute("SELECT count(*) FROM weather").fetchone()[0]
    assert count == 0, count
    save(con, odd[:3])
    assert con.execute("SELECT count(*) FROM weather").fetchone()[0] == 3
print("ok")
