# The Bayesian ranking with pandas, for the chapter "Gathering stars".
# 「★を集める」の章の、ベイズ平均のランキングを pandas で
import pandas as pd

# ANCHOR: pandas
# Python (pandas: groupby and agg)
# Python（pandas：groupby と agg）
C = 5  # how many usual votes to add / ふつうの点を何票ぶん足すか


# 🏆 Rank the rated works; one row per rating comes in.
# 評価した作品を並べる。評価1件につき1行の表を受け取る
def ranking(ratings: pd.DataFrame) -> pd.Series:
    # sort=False keeps the shelf's order for ties.
    # sort=False で、同点の順を本棚の順のままにする
    per_work = ratings.groupby("title", sort=False)["points"].agg(
        ["count", "sum", "mean"]
    )
    usual = per_work["mean"].mean()  # one work, one vote / 1作品1票
    score = (C * usual + per_work["sum"]) / (C + per_work["count"])
    return score.sort_values(ascending=False, kind="stable")
# ANCHOR_END: pandas


DRAGON = "はじめてのドラゴン学"
LIBRARIAN = "図書室の司書に転生した件"
COPYIST = "迷宮図書館の写本係"
DETECTIVE = "王都の路地裏探偵"
MOVIE = "最後のドラゴン"
ROWS = (
    [(DRAGON, 90), (LIBRARIAN, 100)]
    + [(COPYIST, p) for p in [100, 100, 90, 70, 100] * 6]
    + [(DETECTIVE, p) for p in [80, 70, 90, 80]]
    + [(MOVIE, 60)]
)
table = pd.DataFrame(ROWS, columns=["title", "points"])

ranked = ranking(table)
assert list(ranked.index) == [COPYIST, LIBRARIAN, DRAGON, DETECTIVE, MOVIE]
assert abs(ranked[COPYIST] - 3182 / 35) < 1e-9
assert abs(ranked[LIBRARIAN] - 87) < 1e-9

# The plain mean puts the librarian first.
# ふつうの平均では司書が1位
plain = table.groupby("title", sort=False)["points"].mean()
assert plain.sort_values(ascending=False).index[0] == LIBRARIAN

# Ties keep the order of the shelf.
# 同点なら本棚の順のまま
ties = pd.DataFrame([("C", 80), ("B", 80), ("A", 80)],
                    columns=["title", "points"])
assert list(ranking(ties).index) == ["C", "B", "A"]
print("ok")
