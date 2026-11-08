# The ranking of the shelf with Rails (ActiveRecord), for the chapter
# "Gathering stars". Runs with the gems in tools/ruby
# (activerecord, sqlite3).
# 「★を集める」の章の、本棚のランキングを Rails（ActiveRecord）で。
# tools/ruby の gem（activerecord・sqlite3）で動く
require "active_record"

ActiveRecord::Base.establish_connection(adapter: "sqlite3",
                                        database: ":memory:")
ActiveRecord::Migration.verbose = false
ActiveRecord::Schema.define do
  create_table :works do |t|
    t.string  :title,    null: false
    t.integer :position, null: false
  end
  create_table :ratings do |t|
    t.references :work,   null: false
    t.integer    :points, null: false
  end
end

# ANCHOR: rails
# Ruby on Rails (average, count and sum, grouped by the work)
# Ruby on Rails（作品ごとに group して average・count・sum）
class Work < ActiveRecord::Base
  has_many :ratings
end

class Rating < ActiveRecord::Base
  belongs_to :work
  validates :points, inclusion: { in: 0..100 }
end

C = 5 # how many usual votes to add / ふつうの点を何票ぶん足すか

# 🏆 Rank the rated works by the Bayesian average.
# 評価した作品を、ベイズ平均で並べる
def bayes_ranking
  by_work = Rating.group(:work_id)
  means  = by_work.average(:points) # { work_id => mean }
  counts = by_work.count            # { work_id => votes }
  sums   = by_work.sum(:points)     # { work_id => total }
  usual  = means.values.sum / means.size
  Work.where(id: means.keys).order(:position)
      .map { |w| [w.title, (C * usual + sums[w.id]) / (C + counts[w.id])] }
      .sort_by.with_index { |(_, score), i| [-score, i] }
end
# ANCHOR_END: rails

# The shelf (the game is not rated yet) and the ratings.
# 本棚（ゲームはまだ評価していない）と評価
SHELF = {
  "はじめてのドラゴン学" => [90],
  "図書室の司書に転生した件" => [100],
  "迷宮図書館の写本係" => [100, 100, 90, 70, 100] * 6,
  "王都の路地裏探偵" => [80, 70, 90, 80],
  "最後のドラゴン" => [60],
  "ダンジョン司書" => []
}
SHELF.each.with_index(1) do |(title, points), i|
  work = Work.create!(title: title, position: i)
  points.each { work.ratings.create!(points: it) }
end
titles = SHELF.keys

# The plain mean: the librarian (one vote) first.
# ふつうの平均：1票の司書が1位
means = Rating.group(:work_id).average(:points)
best = Work.find(means.max_by { _2 }.first)
raise best.title unless best.title == titles[1]
raise unless means.size == 5

ranking = bayes_ranking
raise ranking.inspect unless
  ranking.map(&:first) == titles.values_at(2, 1, 0, 3, 4)
expected = [3182r / 35, 87r, 256r / 3, 742r / 9, 241r / 3]
ranking.zip(expected) do |(title, score), e|
  raise "#{title} #{score}" unless (score - e).abs < 1e-9
end

# The validation refuses points out of 0..100.
# 0〜100 の外の点は、検証が断る
raise if Rating.new(work: best, points: 101).valid?
puts "ok"
