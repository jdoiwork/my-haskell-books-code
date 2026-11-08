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
# Ruby (procedural style: plain numbers, symbols and a Hash)
# Ruby（手続き型スタイル：ただの数とシンボルと Hash）

# ⭐ Check the stars: 1 to 5, by halves. Keep them as half stars.
# ★を確かめる：1〜5 で、0.5刻み。半分の★の数で持つ
def make_stars(x)
  unless x.between?(1, 5) && (x * 2) == (x * 2).to_i
    raise ArgumentError, "bad stars: #{x}"
  end
  (x * 2).to_i
end

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
# ANCHOR_END: procedural

raise unless make_stars(3.5) == 7
raise unless [6, 3.3, -3].all? { |x| refused? { make_stars(x) } }
raise unless advance(:start, :piled) == :reading
raise unless advance(:stop, :rereading) == :finished
raise unless refused? { advance(:start, :finished) }

# ANCHOR: functional
# Ruby (functional style: a frozen value object made with Data.define)
# Ruby（関数型スタイル：Data.define で作る、変えられない値）

# ⭐ Stars kept as half stars (2 to 10). Checked when made.
# ★は半分の★の数（2〜10）で持つ。作るときに確かめる
Stars = Data.define(:halves) do
  def initialize(halves:)
    raise ArgumentError, "bad stars" unless (2..10).include?(halves)
    super
  end

  def self.of(x)
    raise ArgumentError, "bad stars: #{x}" unless x * 2 == (x * 2).to_i
    new(halves: (x * 2).to_i)
  end

  # 💯 1 star is 20 points; 5 stars are 100.
  # ★1 が 20 点、★5 が 100 点
  def to_score = halves * 10

  # 🔁 A score to stars: round halves up, never below 1 star.
  # 点を★に：真ん中は切り上げ。★1 より下にはしない
  def self.from_score(n) = new(halves: ((n + 5) / 10).clamp(2, 10))
end
# ANCHOR_END: functional

raise unless Stars.of(3.5) == Stars.new(halves: 7)
raise unless [6, 3.3, -3].all? { |x| refused? { Stars.of(x) } }
raise unless Stars.from_score(25).halves == 3
raise unless Stars.from_score(24).halves == 2
all_stars = (2..10).map { |h| Stars.new(halves: h) }
raise unless all_stars.all? { |s| Stars.from_score(s.to_score) == s }
raise unless Stars.of(3.5).frozen?

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
