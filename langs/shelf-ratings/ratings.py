# The ranking of the shelf in Python, for the chapter
# "Gathering stars: a ranking a few 5-star votes cannot fool".
# 「★を集める」の章の、本棚のランキングを Python で

# ANCHOR: procedural
# Python (procedural style: count with dicts, then sort)
# Python（手続き型スタイル：辞書で数えてから並べる）
C = 5  # how many usual votes to add / ふつうの点を何票ぶん足すか


# 🏆 Rank the rated works by the Bayesian average.
# 評価した作品を、ベイズ平均で並べる
def rank(works, ratings):
    # Count the votes and add up the points of each work.
    # 作品ごとに、票の数と点の合計を数える
    count = {}
    total = {}
    for title, points in ratings:
        count[title] = count.get(title, 0) + 1
        total[title] = total.get(title, 0) + points

    # The usual points: the mean of each work's mean (one work, one vote).
    # ふつうの点：作品ごとの平均の平均（1作品1票）
    means = 0
    for title in count:
        means += total[title] / count[title]
    usual = means / len(count)

    # Add C usual votes to each work, and take the mean.
    # 各作品に、ふつうの点を C 票足してから平均をとる
    ranking = []
    for title in works:
        if title in count:
            score = (C * usual + total[title]) / (C + count[title])
            ranking.append((title, score))
    # sort is stable: ties keep the order of the shelf.
    # sort は安定：同点なら本棚の順のまま
    ranking.sort(key=lambda pair: pair[1], reverse=True)
    return ranking
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: comprehensions, mean and sorted)
# Python（関数型スタイル：内包表記と mean と sorted）
from fractions import Fraction
from statistics import mean

C = 5  # how many usual votes to add / ふつうの点を何票ぶん足すか


# ⚖️ The Bayesian average: C usual votes plus the work's own.
# ベイズ平均：ふつうの点 C 票と、その作品の票
def bayes(points, usual):
    return (C * usual + sum(points)) / (C + len(points))


# 🏆 Rank the rated works; Fraction keeps the scores exact.
# 評価した作品を並べる。Fraction で点をぴったり保つ
def ranking(works, ratings):
    points = {w: [p for t, p in ratings if t == w] for w in works}
    rated = {w: ps for w, ps in points.items() if ps}
    usual = mean(mean(map(Fraction, ps)) for ps in rated.values())
    scores = [(w, bayes(ps, usual)) for w, ps in rated.items()]
    return sorted(scores, key=lambda pair: pair[1], reverse=True)
# ANCHOR_END: functional


# The shelf, in its own order, and the ratings (0 to 100; one star = 20).
# 本棚（本棚の順）と、評価（0〜100 点。★1つ = 20 点）
DRAGON = "はじめてのドラゴン学"
LIBRARIAN = "図書室の司書に転生した件"
COPYIST = "迷宮図書館の写本係"
DETECTIVE = "王都の路地裏探偵"
MOVIE = "最後のドラゴン"
GAME = "ダンジョン司書"
WORKS = [DRAGON, LIBRARIAN, COPYIST, DETECTIVE, MOVIE, GAME]
RATINGS = (
    [(DRAGON, 90), (LIBRARIAN, 100)]
    + [(COPYIST, p) for p in [100, 100, 90, 70, 100] * 6]
    + [(DETECTIVE, p) for p in [80, 70, 90, 80]]
    + [(MOVIE, 60)]
)

ORDER = [COPYIST, LIBRARIAN, DRAGON, DETECTIVE, MOVIE]

# The plain mean puts the librarian (one vote of 100) first.
# ふつうの平均では、1票だけの司書（100 点）が1位
plain = {w: mean(p for t, p in RATINGS if t == w)
         for w in WORKS if any(t == w for t, _ in RATINGS)}
assert max(plain, key=plain.get) == LIBRARIAN
assert sorted(plain, key=plain.get, reverse=True)[:2] == [LIBRARIAN, COPYIST]
assert plain[COPYIST] == 92

a = rank(WORKS, RATINGS)
b = ranking(WORKS, RATINGS)
assert [w for w, _ in a] == ORDER, a
assert [w for w, _ in b] == ORDER, b
expected = {
    COPYIST: Fraction(3182, 35),
    LIBRARIAN: Fraction(87),
    DRAGON: Fraction(256, 3),
    DETECTIVE: Fraction(742, 9),
    MOVIE: Fraction(241, 3),
}
assert dict(b) == expected, b
for w, s in a:
    assert abs(s - expected[w]) < 1e-9, (w, s)
assert abs(dict(a)[COPYIST] - 90.914) < 1e-3

# Ties keep the order of the shelf (sort is stable, even with reverse).
# 同点なら本棚の順のまま（reverse でも sort は安定）
TIES = [("A", 80), ("B", 80), ("C", 80)]
assert [w for w, _ in rank(["A", "B", "C"], TIES)] == ["A", "B", "C"]
assert [w for w, _ in ranking(["C", "B", "A"], TIES)] == ["C", "B", "A"]
print("ok")
