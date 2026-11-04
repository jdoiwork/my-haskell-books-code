# A tiny key-value store in Python, for the chapter
# "Design: values and commands as types".
# 「設計：値とコマンドを型で描く」の章の、小さなキーと値のストアを Python で

# ANCHOR: procedural
# Python (procedural style: a dict, and values of any type)
# Python（手続き型スタイル：dict と、どんな型でも入る値）
store = {}
store["hero"] = "Lambda"                  # SET
store.setdefault("party", []).insert(0, "Rust")   # LPUSH
try:
    store.setdefault("hero", []).insert(0, "x")   # LPUSH on a string
except AttributeError as e:
    message = str(e)          # 'str' object has no attribute 'insert'
# ANCHOR_END: procedural

assert store == {"hero": "Lambda", "party": ["Rust"]}
assert message == "'str' object has no attribute 'insert'"
print("ok")
