# The progress of reading in Ruby, for the chapter
# "The tree of contents and how far you have read".
# 「目次の木と進み具合」の章の、読んだ量の集計を Ruby で

# ANCHOR: procedural
# Ruby (procedural style: walk the tree with a stack and each)
# Ruby（手続き型スタイル：スタックと each で木を歩く）

# 📖 Add up the pages read and all the pages of the contents.
#    Each entry's pages are its own, not its children's.
# 目次の、読んだ量と全体の量を足していく。
# 各項目の量は、その項目だけの量（子の量は入らない）
def progress(entries, read)
  done = 0
  total = 0
  stack = entries.dup
  until stack.empty?
    entry = stack.pop
    total += entry[:pages]
    done += entry[:pages] if read.include?(entry[:no])
    entry[:children].each { |child| stack.push(child) }
  end
  [done, total]
end

# 📊 Percent, rounded half up; 1 to 99 while you are reading.
# 百分率。四捨五入して、読んでいる途中は 1〜99 におさめる
def percent(done, total)
  return 0 if total == 0
  p = (done * 200 + total) / (2 * total)
  p = 1 if done > 0 && p == 0        # started / 読み始めたら 0% にしない
  p = 99 if done < total && p == 100 # not finished / 100% にしない
  p
end
# ANCHOR_END: procedural

# ANCHOR: functional
# Ruby (functional style: recursion and sum over a value you can add)
# Ruby（関数型スタイル：再帰と、足し算できる値の sum）

# 📑 One entry of the contents, with its own pages and its children.
# 目次の1項目。その項目だけの量と、子の項目を持つ
Entry = Data.define(:no, :title, :pages, :children) do
  def initialize(no:, title:, pages:, children: [])
    super
  end
end

# ➕ Progress is a value you can add; EMPTY is where adding starts.
# 進み具合は足し算できる値。EMPTY は足し始めの値
Progress = Data.define(:done, :total) do
  def +(other)
    Progress.new(done + other.done, total + other.total)
  end

  def percent
    return 0 if total == 0
    p = (done * 200 + total) / (2 * total)
    done.between?(1, total - 1) ? p.clamp(1, 99) : p
  end
end
Progress::EMPTY = Progress.new(0, 0)

# 🌳 Fold a tree: its own progress plus the sum of the children's.
# 木を畳む：自分の進み具合に、子の進み具合の合計を足す
def progress_of(entry, read)
  own = Progress.new(read.include?(entry.no) ? entry.pages : 0,
                     entry.pages)
  entry.children.sum(own) { |child| progress_of(child, read) }
end

# 📚 Fold all the trees, starting from EMPTY.
# すべての木を畳む。EMPTY から足し始める
def progress_all(entries, read)
  entries.sum(Progress::EMPTY) { |entry| progress_of(entry, read) }
end
# ANCHOR_END: functional

# The sample book, "Dragonology for Beginners".
# サンプルの本『はじめてのドラゴン学』
CHAPTER1 = Entry.new(no: 2, title: "第1章 見分け方", pages: 4, children: [
  Entry.new(no: 3, title: "1.1 うろこの色", pages: 8),
  Entry.new(no: 4, title: "1.2 火を吐くもの", pages: 10)
])
PART1 = Entry.new(no: 1, title: "第1部 ドラゴンを知る", pages: 2, children: [
  CHAPTER1,
  Entry.new(no: 5, title: "第2章 すみかと季節", pages: 24)
])
PART2 = Entry.new(no: 6, title: "第2部 ドラゴンと話す", pages: 2, children: [
  Entry.new(no: 7, title: "第3章 あいさつの作法", pages: 16),
  Entry.new(no: 8, title: "第4章 怒らせたときは", pages: 12)
])
BOOK = [PART1, PART2]
READ = [1, 2, 3, 4, 5].to_set

# The procedural style reads plain hashes.
# 手続き型スタイルは、ただの Hash を読む
def to_hash(e)
  { no: e.no, title: e.title, pages: e.pages,
    children: e.children.map { |c| to_hash(c) } }
end

def both(entries, read)
  a = progress(entries.map { |e| to_hash(e) }, read)
  b = progress_all(entries, read)
  raise "#{a} #{b}" unless a == [b.done, b.total]
  raise "percent #{a}" unless percent(*a) == b.percent
  a
end

AFTERWORD = Entry.new(no: 1, title: "あとがき", pages: 3)
raise unless both(BOOK, READ) == [48, 78]
raise unless percent(48, 78) == 62
raise unless both([PART1], READ) == [48, 48] && percent(48, 48) == 100
raise unless both([PART2], READ) == [0, 30] && percent(0, 30) == 0
raise unless both([CHAPTER1], READ) == [22, 22]
raise unless both([], READ) == [0, 0]
raise unless both([AFTERWORD], Set[1]) == [3, 3]
raise unless both([AFTERWORD], Set[]) == [0, 3]
raise unless both(BOOK, Set[]) == [0, 78]
raise unless both(BOOK, (1..8).to_set) == [78, 78]
[[48, 78, 62], [1, 1000, 1], [999, 1000, 99], [0, 30, 0],
 [30, 30, 100], [0, 0, 0], [1, 2, 50], [1, 8, 13]].each do |d, t, pc|
  raise "#{d}/#{t}" unless percent(d, t) == pc
  raise "#{d}/#{t}" unless Progress.new(d, t).percent == pc
end
