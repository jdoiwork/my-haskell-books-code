# Carrying the connection and the logger, for the chapter
# "Monad transformers and application design".
# 接続とロガーを持ち回る。「モナド変換子とアプリケーション設計」の章
import sqlite3
from contextvars import ContextVar
from dataclasses import dataclass


@dataclass
class Env:
    conn: sqlite3.Connection
    log: object


def setup(conn):
    conn.execute("CREATE TABLE adventurers "
                 "(id INTEGER PRIMARY KEY, name TEXT UNIQUE)")


# ANCHOR: procedural
# Python (procedural style: pass the environment to every function)
# Python（手続き型スタイル：どの関数にも環境を渡す）
def add_adventurer(env, name):
    cur = env.conn.execute("INSERT INTO adventurers (name) VALUES (?)",
                           (name,))
    env.log(f"added {name}")
    return cur.lastrowid


def enlist(env, name):
    taken = env.conn.execute("SELECT 1 FROM adventurers WHERE name = ?",
                             (name,)).fetchone()
    if taken:
        return ("taken", name)
    return ("ok", add_adventurer(env, name))
# ANCHOR_END: procedural


# ANCHOR: contextvar
# Python (contextvars: the environment is "just there", like Reader)
# Python（contextvars：環境は「そこにある」。Reader のように）
current_env: ContextVar[Env] = ContextVar("current_env")


def add_adventurer_cv(name):
    env = current_env.get()
    cur = env.conn.execute("INSERT INTO adventurers (name) VALUES (?)",
                           (name,))
    env.log(f"added {name}")
    return cur.lastrowid


def enlist_cv(name):
    env = current_env.get()
    taken = env.conn.execute("SELECT 1 FROM adventurers WHERE name = ?",
                             (name,)).fetchone()
    if taken:
        return ("taken", name)
    return ("ok", add_adventurer_cv(name))
# ANCHOR_END: contextvar


logs = []
env = Env(sqlite3.connect(":memory:"), logs.append)
setup(env.conn)
assert enlist(env, "Lambda") == ("ok", 1)
assert enlist(env, "Lambda") == ("taken", "Lambda")

logs2 = []
env2 = Env(sqlite3.connect(":memory:"), logs2.append)
setup(env2.conn)
token = current_env.set(env2)
try:
    assert enlist_cv("Lambda") == ("ok", 1)
    assert enlist_cv("Lambda") == ("taken", "Lambda")
finally:
    current_env.reset(token)
assert logs == logs2 == ["added Lambda"]
print("ok")
