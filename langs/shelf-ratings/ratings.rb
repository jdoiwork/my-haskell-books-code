# The ranking of the shelf in Ruby, for the chapter "Gathering stars".
# 「★を集める」の章の、本棚のランキングを Ruby で

# ANCHOR: procedural
# Ruby (procedural style: count with each, then sort)
# Ruby（手続き型スタイル：each で数えてから並べる）
C = 5 # how many usual votes to add / ふつうの点を何票ぶん足すか

# 🏆 Rank the rated works by the Bayesian average.
# 評価した作品を、ベイズ平均で並べる
def rank(works, ratings)
  # Count the votes and add up the points of each work.
  # 作品ごとに、票の数と点の合計を数える
  count = Hash.new(0)
  total = Hash.new(0)
  ratings.each do |title, points|
    count[title] += 1
    total[title] += points
  end
  # Nothing rated yet: an empty ranking.
  # まだ何も評価していなければ、空のランキング
  return [] if count.empty?

  # The usual points: the mean of each work's mean (one work, one vote).
  # ふつうの点：作品ごとの平均の平均（1作品1票）
  means = 0.0
  count.each_key { |title| means += total[title].fdiv(count[title]) }
  usual = means / count.size

  # Add C usual votes to each work, and take the mean.
  # 各作品に、ふつうの点を C 票足してから平均をとる
  ranking = []
  works.each do |title|
    next unless count.key?(title)
    score = (C * usual + total[title]) / (C + count[title])
    ranking << [title, score]
  end
  # sort_by is not stable: the index keeps ties in the shelf's order.
  # sort_by は安定ではない。番号を添えて、同点を本棚の順のままにする
  ranking.sort_by.with_index { |(_, score), i| [-score, i] }
end
# ANCHOR_END: procedural

# ANCHOR: functional
# Ruby (functional style: group_by, sum, Rational and sort_by)
# Ruby（関数型スタイル：group_by と sum と Rational と sort_by）

# ⚖️ The Bayesian average: C usual votes plus the work's own.
# ベイズ平均：ふつうの点 C 票と、その作品の票
def bayes(points, usual)
  (C * usual + points.sum) / (C + points.size)
end

# 🏆 Rank the rated works; Rational keeps the scores exact.
# 評価した作品を並べる。Rational で点をぴったり保つ
def ranking(works, ratings)
  points = ratings.group_by(&:first).transform_values { it.map(&:last) }
  rated = works.filter_map { |w| [w, points[w]] if points.key?(w) }
  return [] if rated.empty? # nothing rated yet / まだ何も評価していない
  usual = rated.sum { |_, ps| Rational(ps.sum, ps.size) } / rated.size
  rated.map { |w, ps| [w, bayes(ps, usual)] }
       .sort_by.with_index { |(_, score), i| [-score, i] }
end
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
RATINGS = [[DRAGON, 90], [LIBRARIAN, 100]] +
          ([100, 100, 90, 70, 100] * 6).map { [COPYIST, it] } +
          [80, 70, 90, 80].map { [DETECTIVE, it] } +
          [[MOVIE, 60]]
ORDER = [COPYIST, LIBRARIAN, DRAGON, DETECTIVE, MOVIE]

# The plain mean puts the librarian (one vote of 100) first.
# ふつうの平均では、1票だけの司書（100 点）が1位
plain = RATINGS.group_by(&:first)
               .transform_values { |rs| rs.sum(&:last).fdiv(rs.size) }
raise unless plain.max_by { _2 }.first == LIBRARIAN
raise unless plain[COPYIST] == 92

a = rank(WORKS, RATINGS)
b = ranking(WORKS, RATINGS)
raise a.inspect unless a.map(&:first) == ORDER
raise b.inspect unless b.map(&:first) == ORDER
EXPECTED = {
  COPYIST => Rational(3182, 35), LIBRARIAN => Rational(87),
  DRAGON => Rational(256, 3), DETECTIVE => Rational(742, 9),
  MOVIE => Rational(241, 3)
}
raise b.inspect unless b.to_h == EXPECTED
a.each { |w, s| raise w unless (s - EXPECTED[w]).abs < 1e-9 }
raise unless (a.to_h[COPYIST] - 90.914).abs < 1e-3

# Ties keep the order of the shelf.
# 同点なら本棚の順のまま
TIES = [["A", 80], ["B", 80], ["C", 80]]
raise unless rank(%w[A B C], TIES).map(&:first) == %w[A B C]
raise unless rank(%w[A B], []) == [] && ranking(%w[A B], []) == []
raise unless ranking(%w[C B A], TIES).map(&:first) == %w[C B A]
puts "ok"
