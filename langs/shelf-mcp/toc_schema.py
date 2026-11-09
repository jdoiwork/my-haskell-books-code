# The table of contents an AI sends, as pydantic models, for comparing
# with the hand-written JSON Schema of the Haskell MCP server. pydantic
# makes the JSON Schema from the types, and checks what comes in.
# AI が送る目次を pydantic のモデルで。Haskell の MCP サーバーの、手で書いた
# JSON Schema とくらべるため。pydantic は型から JSON Schema を作り、
# 入ってくるものを確かめる
from __future__ import annotations

from pydantic import BaseModel, Field, ValidationError


# ANCHOR: models
# Python (pydantic: the types are the schema)
# Python（pydantic：型がそのままスキーマ）
class Chapter(BaseModel):
    heading: str = Field(min_length=1)
    page: int = Field(ge=1)
    children: list[Chapter] = []


class TocDraft(BaseModel):
    title: str
    creator: str = ""
    last_page: int = Field(ge=1)
    toc: list[Chapter]


schema = TocDraft.model_json_schema()
# ANCHOR_END: models

# A chapter holds chapters: the schema refers to itself through $defs.
# 章は章を持つ：スキーマは $defs を通して自分を指す
assert schema["properties"]["toc"]["items"] == {"$ref": "#/$defs/Chapter"}
chapter = schema["$defs"]["Chapter"]
assert chapter["properties"]["children"]["items"] == {"$ref": "#/$defs/Chapter"}
assert chapter["required"] == ["heading", "page"]

# ANCHOR: errors
# Python (what pydantic says about a bad tree: places, not headings)
# Python（まずい木に pydantic が言うこと：場所は番号の道。見出しではない）
bad = {
    "title": "竜の飼い方入門",
    "last_page": 120,
    "toc": [
        {"heading": "第1章 卵", "page": 1,
         "children": [{"heading": "", "page": 3}]},
        {"heading": "第2章 ひな", "page": 0},
    ],
}
try:
    TocDraft.model_validate(bad)
except ValidationError as e:
    places = [err["loc"] for err in e.errors()]
# ANCHOR_END: errors

assert places == [
    ("toc", 0, "children", 0, "heading"),
    ("toc", 1, "page"),
]

# What a type cannot say: a page that comes before the one above.
# 型では言えないこと：前の章より前から始まるページ
backwards = TocDraft.model_validate({
    "title": "x", "last_page": 99,
    "toc": [{"heading": "a", "page": 30}, {"heading": "b", "page": 12}],
})
assert [c.page for c in backwards.toc] == [30, 12]   # it passes / 通ってしまう
print("ok")
