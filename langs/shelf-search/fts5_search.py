# Full-text search with SQLite's FTS5 from Python's sqlite3, for the
# chapter "Searching what is inside: full-text search with SQLite FTS5".
# 「中身を探す」の章の全文検索を、Python の sqlite3 と SQLite の FTS5 で
import sqlite3

DOCUMENTS = [
    ("はじめてのドラゴン学", "title"),
    ("最後のドラゴン", "title"),
    ("絵が多くて読みやすい", "note"),
    ("冬になると、ドラゴンは山の洞くつで宝を数えて過ごします。", "text"),
    ("ドラゴンは怠けものではありません。寝ていても宝の数を覚えています。",
     "text"),
]

conn = sqlite3.connect(":memory:")
conn.execute("CREATE VIRTUAL TABLE documents"
             " USING fts5(body, place UNINDEXED, tokenize = 'trigram')")
conn.executemany("INSERT INTO documents VALUES (?, ?)", DOCUMENTS)

# ANCHOR: procedural
# Python (procedural style: build the query word by word)
# Python（手続き型スタイル：問い合わせを1語ずつ組み立てる）
def fts_query(typed):
    long_words = []
    for word in typed.split():
        if len(word) >= 3:
            # a string in double quotes; a " inside is written twice
            # 二重引用符の文字列に。中の " は2つ重ねる
            long_words.append('"' + word.replace('"', '""') + '"')
    return " ".join(long_words)


def search(conn, typed):
    found = []
    rows = conn.execute(
        "SELECT place, highlight(documents, 0, '[', ']')"
        " FROM documents WHERE documents MATCH ? ORDER BY rank",
        (fts_query(typed),))
    for place, marked in rows:
        found.append((place, marked))
    return found
# ANCHOR_END: procedural


assert fts_query('宝 怠けもの a"b') == '"怠けもの" "a""b"'
assert search(conn, "怠けもの") == [
    ("text",
     "ドラゴンは[怠けもの]ではありません。寝ていても宝の数を覚えています。")]
# the words typed are never read as FTS5's operators
# 打った語が、FTS5 の演算子として読まれることはない
assert search(conn, "NEAR( ドラゴン") == []


# ANCHOR: functional
# Python (functional style: comprehensions, and LIKE for short words)
# Python（関数型スタイル：内包表記と、短い語は LIKE で）
def quoted(word):
    return '"' + word.replace('"', '""') + '"'


def like_pattern(word):
    # % and _ in what was typed are just characters
    # 打った % と _ は、ただの文字
    escaped = (word.replace("\\", "\\\\")
                   .replace("%", "\\%")
                   .replace("_", "\\_"))
    return f"%{escaped}%"


def search_all(conn, typed):
    words = typed.split()
    long_words = [w for w in words if len(w) >= 3]
    short_words = [w for w in words if len(w) < 3]
    conditions = ["body LIKE ? ESCAPE '\\'" for _ in short_words]
    params = [like_pattern(w) for w in short_words]
    if long_words:
        conditions.insert(0, "documents MATCH ?")
        params.insert(0, " ".join(quoted(w) for w in long_words))
    sql = "SELECT place FROM documents WHERE " + " AND ".join(conditions)
    return [place for (place,) in conn.execute(sql, params)]
# ANCHOR_END: functional


# "宝" is one character: MATCH finds nothing, without an error
# 「宝」は1文字。MATCH は、エラーも出さずに何も見つけない
assert conn.execute("SELECT count(*) FROM documents WHERE documents MATCH ?",
                    ('"宝"',)).fetchone() == (0,)
assert search_all(conn, "宝") == ["text", "text"]
assert search_all(conn, "ドラゴン 宝 冬") == ["text"]
assert search_all(conn, "ドラゴン") == ["title", "title", "text", "text"]
assert search_all(conn, "%") == []
print("ok")
