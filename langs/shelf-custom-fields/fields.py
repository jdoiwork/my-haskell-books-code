# The gate for a form of my own attributes in Python, for the chapter
# "Attributes and states of my own".
# 「自分で決める属性と状態」の章の、自分で決めた属性のフォームの門を Python で
from datetime import date

# 📒 The definitions: name -> kind (a list is a choice).
# 定義：名前 → 種類（リストは選択肢）
DEFINITIONS = {
    "推し度": "number",
    "買った日": "date",
    "サイン本": "yesno",
    "形": ["紙", "電子", "Web"],
}
FORM = {"推し度": "すごく", "買った日": "11月1日",
        "サイン": "はい", "形": "Web"}


def procedural():
    # ANCHOR: procedural
    # Python (procedural style: a loop that collects the problems)
    # Python（手続き型スタイル：まずい所をためていくループ）

    # 📥 Read one field by its kind. Raise ValueError if it does not fit.
    # 欄を1つ、種類で読む。合わなければ ValueError
    def read_value(kind, typed):
        if kind == "number":
            return int(typed)
        if kind == "date":
            return date.fromisoformat(typed)
        if kind == "yesno":
            if typed == "はい":
                return True
            if typed == "いいえ":
                return False
            raise ValueError(typed)
        if isinstance(kind, list):
            if typed in kind:
                return typed
            raise ValueError(typed)
        return typed

    # 🚪 Read every field. Keep going after a problem, to find them all.
    # どの欄も読む。まずい所があっても、全部見つけるまで続ける
    def read_fields(form):
        values = {}
        problems = []
        for name, typed in form.items():
            if name not in DEFINITIONS:
                problems.append(f"「{name}」という属性はありません")
                continue
            try:
                values[name] = read_value(DEFINITIONS[name], typed)
            except ValueError:
                problems.append(f"{name} に「{typed}」は入りません")
        return values, problems
    # ANCHOR_END: procedural
    return read_fields


def functional():
    # ANCHOR: functional
    # Python (functional style: a reader for each kind, and comprehensions)
    # Python（関数型スタイル：種類ごとの読み手と、内包表記）
    YES_NO = {"はい": True, "いいえ": False}
    READERS = {
        "number": int,
        "date": date.fromisoformat,
        "yesno": lambda typed: YES_NO[typed],
    }

    # 📥 A reader for a kind: a choice is a reader made from its list.
    # 種類ごとの読み手。選択肢は、そのリストから読み手を作る
    def reader(kind):
        if isinstance(kind, list):
            return lambda typed: {c: c for c in kind}[typed]
        return READERS[kind]

    # 🧪 One field: (True, value) or (False, problem).
    # 欄を1つ：(True, 値) か (False, まずい所)
    def check(name, typed):
        if name not in DEFINITIONS:
            return False, f"「{name}」という属性はありません"
        try:
            return True, reader(DEFINITIONS[name])(typed)
        except (ValueError, KeyError):
            return False, f"{name} に「{typed}」は入りません"

    def read_fields(form):
        checked = {name: check(name, typed) for name, typed in form.items()}
        values = {n: v for n, (ok, v) in checked.items() if ok}
        problems = [p for ok, p in checked.values() if not ok]
        return values, problems
    # ANCHOR_END: functional
    return read_fields


for read_fields in [procedural(), functional()]:
    values, problems = read_fields(FORM)
    assert values == {"形": "Web"}, values
    assert problems == ["推し度 に「すごく」は入りません",
                        "買った日 に「11月1日」は入りません",
                        "「サイン」という属性はありません"], problems
    good, none = read_fields({"推し度": "4", "サイン本": "いいえ",
                              "買った日": "2026-11-01"})
    assert good == {"推し度": 4, "サイン本": False,
                    "買った日": date(2026, 11, 1)} and none == [], good
    # int() is kind: spaces and full-width digits are numbers too.
    # int() はやさしい：空白や全角の数字も、数として読む
    assert read_fields({"推し度": " ４ "}) == ({"推し度": 4}, [])
print("ok")
