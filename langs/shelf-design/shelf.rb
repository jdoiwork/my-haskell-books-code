# The bookshelf's types in Ruby, for the chapter
# "Design: works, contents, notes and ratings as types".
# 「設計：作品・目次・感想・評価を型で描く」の章の、本棚の型を Ruby で

# Expect an error from the block. / ブロックがエラーになることを確かめる
def refused?
  yield
  false
rescue ArgumentError
  true
end

# ANCHOR: procedural
# Ruby (procedural style: a plain number and style names as symbols)
# Ruby（手続き型スタイル：ただの数と、シンボルの見せ方の名前）

# 💯 Check a score: a whole number from 0 to 100.
# 点を確かめる：0〜100 の整数
def make_score(n)
  raise ArgumentError, "bad score: #{n}" unless n.between?(0, 100)
  n
end

# 🖼️ Show a score: :hundred, :stars, :ten or :ten1 (7.3/10).
# 点を見せる：:hundred、:stars、:ten、:ten1（7.3/10）
def render(score, style)
  if style == :hundred
    "#{score}点"
  elsif style == :stars
    halves = (score + 5) / 10        # halves go up / 真ん中は切り上げ
    if halves.even?
      "★#{halves / 2}"
    else
      "★#{halves / 2}.5"
    end
  elsif style == :ten
    "#{(score + 5) / 10}/10"
  elsif style == :ten1
    "#{score / 10}.#{score % 10}/10"
  else
    raise ArgumentError, "bad style: #{style}"
  end
end
# ANCHOR_END: procedural

# ANCHOR: moves
# Ruby (procedural style: the map of reading as a Hash)
# Ruby（手続き型スタイル：読書の地図を Hash で）

# 🗺️ The map of reading: [state, event] => next state.
# 読書の地図：[状態, できごと] => 次の状態
MOVES = {
  [:piled, :start] => :reading,
  [:reading, :finish] => :finished,
  [:reading, :stop] => :paused,
  [:paused, :resume] => :reading,
  [:finished, :reread] => :rereading,
  [:rereading, :finish] => :finished,
  [:rereading, :stop] => :finished
}

def advance(event, state)
  MOVES.fetch([state, event]) do
    raise ArgumentError, "not allowed: #{event} from #{state}"
  end
end
# ANCHOR_END: moves

raise unless make_score(73) == 73
raise unless [-1, 101].all? { |n| refused? { make_score(n) } }
shown = %i[hundred stars ten ten1].map { |s| render(73, s) }
raise unless shown == ["73点", "★3.5", "7/10", "7.3/10"]
raise unless render(75, :stars) == "★4"
raise unless render(25, :stars) == "★1.5"
raise unless render(4, :stars) == "★0"
raise unless render(75, :ten) == "8/10"
raise unless render(80, :ten1) == "8.0/10"
raise unless refused? { render(73, :starz) }
# Ruby's round goes away from zero at halves (Python's goes to even).
# Ruby の round は、真ん中を0から遠いほうへ（Python は偶数へ）
raise unless 2.5.round == 3 && 2.5.round(half: :even) == 2
raise unless advance(:start, :piled) == :reading
raise unless advance(:stop, :rereading) == :finished
raise unless refused? { advance(:start, :finished) }

# ANCHOR: functional
# Ruby (functional style: a value made with Data.define, and a helper)
# Ruby（関数型スタイル：Data.define で作る値と、ヘルパー）

# 💯 The model: a score from 0 to 100. Checked when made.
# モデル：0〜100 の点。作るときに確かめる
Score = Data.define(:points) do
  def initialize(points:)
    unless points.is_a?(Integer) && points.between?(0, 100)
      raise ArgumentError, "bad score: #{points}"
    end
    super
  end
end

# 🎨 Ways to show a score. None of them is the score itself.
# 点の見せ方。どれも、点そのものではない
STYLES = %i[hundred five_stars out_of_ten out_of_ten_decimal].freeze

# 📏 How many points one step of the style is worth.
# その見せ方の1目盛りが、何点ぶんか
def step(style)
  case style
  in :hundred | :out_of_ten_decimal then 1
  in :five_stars | :out_of_ten then 10
  end
end
# ANCHOR_END: functional

# ANCHOR: helper
# Ruby (functional style: a view helper, as in Rails)
# Ruby（関数型スタイル：Rails のような、ビューのヘルパー）
module RatingHelper
  # 🖼️ Show a score; a coarse style rounds halves up (75 is ★4).
  # 点を見せる。粗い見せ方は、真ん中を切り上げる（75 点は★4）
  def rating_text(score, style)
    n = score.points
    steps = (n + step(style) / 2) / step(style)
    case style
    in :hundred then "#{n}点"
    in :five_stars then "★#{tenths(steps * 5)}"
    in :out_of_ten then "#{steps}/10"
    in :out_of_ten_decimal then "#{n / 10}.#{n % 10}/10"
    end
  end

  # 🔢 35 tenths is "3.5"; 40 is "4".
  # 0.1 が 35 個なら "3.5"、40 個なら "4"
  def tenths(t)
    whole, decimal = t.divmod(10)
    decimal.zero? ? whole.to_s : "#{whole}.#{decimal}"
  end
end
# ANCHOR_END: helper

# ANCHOR: parse
# Ruby (functional style: read typed text back into the model)
# Ruby（関数型スタイル：打ちこまれた文字を、モデルに読み戻す）
module RatingHelper
  MARKS = { hundred: /点\z/, five_stars: /\A★/,
            out_of_ten: %r{/10\z}, out_of_ten_decimal: %r{/10\z} }.freeze

  # 📥 Read text in a style back into a score. Marks may be left out.
  # その見せ方の文字を、点に読み戻す。印は省いてもよい
  def parse_rating(style, text)
    number = text.strip.sub(MARKS.fetch(style), "").strip
    m = /\A([0-9]{1,4})(?:\.([0-9]))?\z/.match(number)
    t = m && (m[1].to_i * 10 + m[2].to_i)    # nil.to_i is 0 / nil は 0
    case [style, t]
    in [:hundred, Integer] if t % 10 == 0 && t <= 1000 then Score[t / 10]
    in [:five_stars, Integer] if t % 5 == 0 && t <= 50 then Score[t * 2]
    in [:out_of_ten, Integer] if t % 10 == 0 && t <= 100 then Score[t]
    in [:out_of_ten_decimal, Integer] if t <= 100 then Score[t]
    else raise ArgumentError, "bad rating: #{text}"
    end
  end
end
# ANCHOR_END: parse

include RatingHelper
raise unless Score.new(points: 73).points == 73
raise unless [-1, 101, 7.5].all? { |n| refused? { Score[n] } }
raise unless Score[73].frozen?
shown = STYLES.map { |s| rating_text(Score[73], s) }
raise unless shown == ["73点", "★3.5", "7/10", "7.3/10"]
raise unless rating_text(Score[75], :five_stars) == "★4"
raise unless rating_text(Score[25], :five_stars) == "★1.5"
raise unless rating_text(Score[4], :five_stars) == "★0"
raise unless rating_text(Score[100], :five_stars) == "★5"
raise unless rating_text(Score[75], :out_of_ten) == "8/10"
raise unless rating_text(Score[80], :out_of_ten_decimal) == "8.0/10"
raise unless parse_rating(:five_stars, "★4.5") == Score[90]
raise unless parse_rating(:five_stars, " 4 ") == Score[80]
raise unless parse_rating(:hundred, "73点") == Score[73]
raise unless parse_rating(:out_of_ten_decimal, "7.5") == Score[75]
raise unless parse_rating(:out_of_ten_decimal, "8.0/10") == Score[80]
raise unless parse_rating(:out_of_ten, "7/10") == Score[70]
bad = [[:five_stars, "★6"], [:five_stars, "★3.3"], [:out_of_ten, "7.5"],
       [:out_of_ten_decimal, "10.1"], [:hundred, "101"], [:hundred, "7.5"],
       [:hundred, "-3"], [:hundred, "７３"], [:five_stars, "3.25"],
       [:hundred, ""]]
bad.each do |style, text|
  raise "#{style} #{text}" unless refused? { parse_rating(style, text) }
end
STYLES.each do |style|
  (0..100).each do |n|
    back = parse_rating(style, rating_text(Score[n], style)).points
    # A shadow is near the model; on a step, it is the model itself.
    # 影はモデルの近く。目盛りの上なら、モデルそのもの
    raise "#{style} #{n}" unless (back - n).abs <= step(style) / 2
    raise "#{style} #{n}" if n % step(style) == 0 && back != n
  end
end
# A style with no line fails only when it runs: NoMatchingPatternError.
# 行のない見せ方は、動かしたときに初めて失敗する（NoMatchingPatternError）
begin
  rating_text(Score[73], :out_of_five)
  raise "should not match"
rescue NoMatchingPatternError
end

# ANCHOR: kinds
# Ruby (pattern matching with case/in over the kinds of works)
# Ruby（作品の種類で case/in のパターンマッチ）
# toc: [[heading, pages], ...] / toc：[[見出し, ページ], ...]
Book = Data.define(:toc)
Film = Data.define(:minutes)
Game = Data.define

# 📏 How long is it? Each kind answers in its own unit.
# どれくらいの長さ？種類ごとに、自分の単位で答える
def length(work)
  case work
  in Book(toc:) then "#{toc.sum { |_, pages| pages }} pages"
  in Film(minutes:) then "#{minutes} minutes"
  in Game then "no table of contents"
  end
end
# ANCHOR_END: kinds

dragon_book = Book.new(toc: [["第1部 ドラゴンを知る", 120],
                             ["第2部 ドラゴンと話す", 200]])
last_dragon = Film.new(minutes: 121)
raise unless length(dragon_book) == "320 pages"
raise unless length(last_dragon) == "121 minutes"
raise unless length(Game.new) == "no table of contents"
# A kind with no line fails only when it runs: NoMatchingPatternError.
# 行のない種類は、動かしたときに初めて失敗する（NoMatchingPatternError）
Serial = Data.define(:toc)
begin
  length(Serial.new(toc: []))
  raise "should not match"
rescue NoMatchingPatternError
end
