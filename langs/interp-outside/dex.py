# The Pokédex cache of the chapter, in Python, with only the standard
# library (urllib and sqlite3). The book shows the part between the
# ANCHOR lines; the rest checks it with a fake web, offline.
# 章のポケモン図鑑のキャッシュを、Python の標準ライブラリだけで
# （urllib と sqlite3）。本に載せるのは ANCHOR のあいだ。残りは、偽物の
# Web で、ネットなしに確かめる
# ANCHOR: dex
import json
import sqlite3
import urllib.request

API = "https://pokeapi.co/api/v2/"


# 🌐 GET a URL, and read the body as JSON. A 404 raises HTTPError.
# URL を GET して、本文を JSON として読む。404 は HTTPError を投げる
def get_json(url):
    with urllib.request.urlopen(url) as response:
        return json.load(response)


# 🔎 The row of a Pokémon: from dex.db if it is there, or from the web.
# ポケモンの行：dex.db にあればそこから、なければ Web から
def look_up(db, name, fetch=get_json):
    row = db.execute("SELECT * FROM dex WHERE name = ?", (name,)).fetchone()
    if row is not None:
        return row, "cache"
    pokemon = fetch(f"{API}pokemon/{name}")
    db.execute(
        "INSERT INTO dex VALUES (?, ?, ?, ?)",
        (name, pokemon["id"], pokemon["height"], pokemon["weight"]),
    )
    db.commit()
    row = db.execute("SELECT * FROM dex WHERE name = ?", (name,)).fetchone()
    return row, "web"
# ANCHOR_END: dex


# The check: a fake web that counts the requests.
# 確かめ：頼みごとの数を数える偽物の Web
asked = []


def fake_fetch(url):
    asked.append(url)
    return {"id": 25, "height": 4, "weight": 60}


db = sqlite3.connect(":memory:")
db.execute(
    "CREATE TABLE dex (name TEXT PRIMARY KEY, id INTEGER,"
    " height INTEGER, weight INTEGER)"
)
assert look_up(db, "pikachu", fake_fetch) == (("pikachu", 25, 4, 60), "web")
assert look_up(db, "pikachu", fake_fetch) == (("pikachu", 25, 4, 60), "cache")
assert asked == [API + "pokemon/pikachu"]
print("ok")
