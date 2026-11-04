# The psycopg and testcontainers code in the PostgreSQL chapter.
# PostgreSQL の章の psycopg と testcontainers のコード
# It needs Docker; without it, the tests are skipped.
# Docker が要る。なければテストを飛ばす
import shutil
import subprocess

import pytest


def docker_running():
    if shutil.which("docker") is None:
        return False
    result = subprocess.run(["docker", "info"], capture_output=True)
    return result.returncode == 0


pytestmark = pytest.mark.skipif(not docker_running(),
                                reason="Docker is not running")


# ANCHOR: fixture
# Python (psycopg and testcontainers with pytest)
# Python（pytest で psycopg と testcontainers）
import psycopg
from testcontainers.community.postgres import PostgresContainer


@pytest.fixture(scope="module")
def postgres():
    with PostgresContainer("postgres:18", driver=None) as pg:
        yield pg.get_connection_url()


@pytest.fixture
def conn(postgres):
    with psycopg.connect(postgres) as conn:
        conn.execute("DROP TABLE IF EXISTS adventurers")
        conn.execute(
            "CREATE TABLE adventurers ("
            " id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,"
            " name TEXT NOT NULL UNIQUE, job TEXT NOT NULL,"
            " level INTEGER NOT NULL)")
        yield conn


def test_returns_the_new_id(conn):
    row = conn.execute(
        "INSERT INTO adventurers (name, job, level)"
        " VALUES (%s, %s, %s) RETURNING id",
        ("Lambda", "Mage", 3)).fetchone()
    assert row == (1,)


def test_rejects_the_same_name_twice(conn):
    insert = ("INSERT INTO adventurers (name, job, level)"
              " VALUES (%s, %s, %s)")
    conn.execute(insert, ("Lambda", "Mage", 3))
    with pytest.raises(psycopg.errors.UniqueViolation):
        conn.execute(insert, ("Lambda", "Mage", 3))
# ANCHOR_END: fixture
