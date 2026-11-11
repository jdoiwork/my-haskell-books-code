# The shelf's terminal screen in Python's Textual, for comparing with the
# brick screen of the chapter. The state lives in reactive attributes, and
# the screen is drawn again from them whenever they change. A real screen
# would ask the JSON API; here the works are fixed, so it runs offline.
# 本棚のターミナルの画面を、Python の Textual で。章の brick の画面と
# くらべるため。状態は reactive な属性に置き、変わるたびに、そこから描き
# 直す。本物の画面なら JSON の API に聞くが、ここでは作品を決め打ちにして、
# ネットなしで動かす
import asyncio

# ANCHOR: app
# Python (Textual: draw from the state, again whenever it changes)
# Python（Textual：状態から描き、変わるたびに描き直す）
from dataclasses import dataclass, replace

from textual.app import App, ComposeResult
from textual.reactive import reactive
from textual.widgets import Footer, Static


@dataclass(frozen=True)
class Work:
    title: str
    rating: int  # 0..100


class ShelfApp(App):
    BINDINGS = [
        ("j", "move(1)", "下へ"),
        ("k", "move(-1)", "上へ"),
        ("plus", "rate(10)", "★を半分ふやす"),
        ("minus", "rate(-10)", "★を半分へらす"),
        ("q", "quit", "閉じる"),
    ]

    # The state: change it, and the screen is drawn again.
    # 状態：変えると、画面が描き直される
    works: reactive[tuple[Work, ...]] = reactive(())
    cursor: reactive[int] = reactive(0)

    def compose(self) -> ComposeResult:
        yield Static(id="shelf")
        yield Footer()

    def lines(self) -> list[str]:
        return [
            ("▶ " if i == self.cursor else "  ")
            + f"{w.title}  {stars(w.rating)}"
            for i, w in enumerate(self.works)
        ]

    def watch_works(self) -> None:
        self.redraw()

    def watch_cursor(self) -> None:
        self.redraw()

    def redraw(self) -> None:
        if self.is_mounted:
            self.query_one("#shelf", Static).update("\n".join(self.lines()))

    def on_mount(self) -> None:
        self.redraw()

    def action_move(self, d: int) -> None:
        self.cursor = max(0, min(len(self.works) - 1, self.cursor + d))

    def action_rate(self, d: int) -> None:
        # A new tuple, not a change inside the old one: Textual sees
        # the new value and draws again.
        # 古いタプルの中を変えず、新しいタプルを入れる。Textual が新しい
        # 値に気づいて、描き直す
        if not self.works:  # an empty shelf: nothing to rate / 空の本棚
            return
        w = self.works[self.cursor]
        new = replace(w, rating=max(0, min(100, w.rating + d)))
        self.works = tuple(
            new if i == self.cursor else x for i, x in enumerate(self.works)
        )


def stars(rating: int) -> str:
    # Points to stars, in halves, rounding the middle up
    # (73 → ★3.5, 75 → ★4), as the chapter's shadows.ts does.
    # 点を★に。半分刻みで、真ん中は切り上げ（73 → ★3.5、75 → ★4）。
    # 章の shadows.ts と同じ
    halves = (rating + 5) // 10
    return f"★{halves / 2:g}"
# ANCHOR_END: app


# ANCHOR: test
# Python (Textual: press keys with no terminal, in run_test)
# Python（Textual：run_test で、端末なしにキーを押す）
async def test_shelf() -> None:
    app = ShelfApp()
    async with app.run_test() as pilot:
        await pilot.press("plus")  # an empty shelf / 空の本棚
        assert app.works == ()
        app.works = (Work("竜の飼い方入門", 60), Work("星の地図", 80))
        await pilot.press("j", "plus")
        assert app.cursor == 1
        assert app.works[1].rating == 90
        shown = str(app.query_one("#shelf", Static).content)
        assert shown.splitlines()[1] == "▶ 星の地図  ★4.5"
        await pilot.press("j")  # no row below / 下の行はない
        assert app.cursor == 1
# ANCHOR_END: test


asyncio.run(test_shelf())
assert [stars(n) for n in (0, 73, 75, 100)] == ["★0", "★3.5", "★4", "★5"]
print("ok")
