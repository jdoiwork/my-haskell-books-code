# The pytest and Hypothesis code in the database testing chapter.
# データベースをテストする章の pytest と Hypothesis のコード
import sqlite3

import pytest


def create_table(conn):
    conn.execute(
        "CREATE TABLE adventurers (id INTEGER PRIMARY KEY,"
        " name TEXT NOT NULL UNIQUE, job TEXT NOT NULL,"
        " level INTEGER NOT NULL)"
    )


def add_adventurer(conn, name, job, level):
    conn.execute(
        "INSERT INTO adventurers (name, job, level) VALUES (?, ?, ?)",
        (name, job, level),
    )


def names(conn):
    rows = conn.execute("SELECT name FROM adventurers ORDER BY id")
    return [name for (name,) in rows]


# ANCHOR: fixture
# Python (a pytest fixture: prepare, hand over, clean up)
# Python（pytest の fixture：準備して、渡して、片付ける）
@pytest.fixture
def conn():
    conn = sqlite3.connect(":memory:")
    create_table(conn)
    yield conn
    conn.close()


def test_starts_empty(conn):
    assert names(conn) == []


def test_rejects_the_same_name_twice(conn):
    add_adventurer(conn, "Lambda", "Mage", 3)
    with pytest.raises(sqlite3.IntegrityError):
        add_adventurer(conn, "Lambda", "Mage", 3)
# ANCHOR_END: fixture


# ANCHOR: state_machine
# Python (a model-based test with Hypothesis)
# Python（Hypothesis でモデルベースのテスト）
from hypothesis import strategies as st
from hypothesis.stateful import (
    RuleBasedStateMachine, invariant, rule)

who = st.sampled_from(["Lambda", "Python", "Rust"])


class RosterMachine(RuleBasedStateMachine):
    def __init__(self):
        super().__init__()
        self.conn = sqlite3.connect(":memory:")
        create_table(self.conn)
        self.model = {}  # name -> level

    @rule(name=who, level=st.integers(1, 10))
    def add(self, name, level):
        try:
            add_adventurer(self.conn, name, "Mage", level)
            added = True
        except sqlite3.IntegrityError:
            added = False
        assert added == (name not in self.model)
        if added:
            self.model[name] = level

    @rule(name=who)
    def remove(self, name):
        cur = self.conn.execute(
            "DELETE FROM adventurers WHERE name = ?", (name,))
        assert cur.rowcount == (1 if name in self.model else 0)
        self.model.pop(name, None)

    @invariant()
    def same_roster(self):
        # Compare the levels too, not only the names.
        # 名前だけでなく、レベルまでくらべる
        rows = self.conn.execute("SELECT name, level FROM adventurers")
        assert dict(rows) == self.model

    def teardown(self):
        self.conn.close()


TestRoster = RosterMachine.TestCase
# ANCHOR_END: state_machine
