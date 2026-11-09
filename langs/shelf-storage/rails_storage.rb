# The shelf in SQLite with Rails (ActiveRecord), for the chapter
# "The window and the records". Runs with the gems in tools/ruby
# (activerecord, sqlite3).
# 「窓口と記録」の章の、SQLite の本棚を Rails（ActiveRecord）で。
# tools/ruby の gem（activerecord・sqlite3）で動く
require "active_record"

ActiveRecord::Base.establish_connection(adapter: "sqlite3",
                                        database: ":memory:")
ActiveRecord::Migration.verbose = false

# ANCHOR: schema
# Ruby on Rails (the migration DSL: tables, references and a CHECK)
# Ruby on Rails（マイグレーションの DSL：表、参照、CHECK）
ActiveRecord::Schema.define do
  create_table :works do |t|
    t.string :title, null: false
  end
  create_table :entries do |t|
    t.references :work,   null: false, foreign_key: { on_delete: :cascade }
    t.references :parent, foreign_key: { to_table: :entries,
                                         on_delete: :cascade }
    t.integer :position, null: false
    t.string  :heading,  null: false
    t.integer :size,     null: false
    t.check_constraint "size >= 0"
  end
  create_table :notes do |t|
    t.references :work, null: false, foreign_key: { on_delete: :cascade }
    t.text :body, null: false
  end
end
# ANCHOR_END: schema

# ANCHOR: rails
# Ruby on Rails (models with associations, and one transaction)
# Ruby on Rails（関連つきのモデルと、1つのトランザクション）
class Work < ActiveRecord::Base
  has_many :entries, dependent: :destroy
  has_many :notes,   dependent: :destroy
  has_many :roots, -> { where(parent_id: nil).order(:position) },
           class_name: "Entry"
end

class Entry < ActiveRecord::Base
  belongs_to :work
  belongs_to :parent, class_name: "Entry", optional: true
  has_many :children, -> { order(:position) },
           class_name: "Entry", foreign_key: :parent_id
  validates :size, numericality: { greater_than_or_equal_to: 0 }
end

class Note < ActiveRecord::Base
  belongs_to :work
end

# 📚 Add a work and its tree: all of it, or none of it.
# 作品と目次の木を足す：全部か、何もなしか
def add_work(title, toc)
  Work.transaction do
    work = Work.create!(title: title)
    add_entries(work, nil, toc)
    work
  end
end

# 🌿 Add the children of one parent, and their children in turn.
# 1つの親の子を足し、その子の子も順に足す
def add_entries(work, parent, children)
  children.each_with_index do |(heading, size, sub), position|
    entry = work.entries.create!(parent: parent, position: position,
                                 heading: heading, size: size)
    add_entries(work, entry, sub)
  end
end

# 🌲 Walk the tree from the roots: [depth, heading], in order.
# 根から木を歩く：[深さ, 見出し] を順番どおりに
def toc(entries, depth = 0)
  entries.flat_map do |e|
    [[depth, e.heading], *toc(e.children, depth + 1)]
  end
end
# ANCHOR_END: rails

# The book from the chapter: [heading, size, children].
# 章の本：[見出し, 大きさ, 子]
DRAGONS = [
  ["第1部 ドラゴンを知る", 2, [
    ["第1章 見分け方", 4, [
      ["1.1 うろこの色", 8, []],
      ["1.2 火を吐くもの", 10, []]
    ]],
    ["第2章 すみかと季節", 24, []]
  ]],
  ["第2部 ドラゴンと話す", 2, [
    ["第3章 あいさつの作法", 16, []],
    ["第4章 怒らせたときは", 12, []]
  ]]
].freeze

# SQLite's foreign keys: Rails turns them on for us.
# SQLite の外部キー：Rails が入れておいてくれる
fk = ActiveRecord::Base.connection.select_value("PRAGMA foreign_keys")
raise "foreign_keys #{fk}" unless fk == 1

work = add_work("はじめてのドラゴン学", DRAGONS)
raise work.id.to_s unless work.id == 1
rows = toc(work.roots)
raise rows.inspect unless rows.map(&:first) == [0, 1, 2, 2, 1, 0, 1, 1]
raise rows.inspect unless rows.map(&:last) == [
  "第1部 ドラゴンを知る", "第1章 見分け方", "1.1 うろこの色",
  "1.2 火を吐くもの", "第2章 すみかと季節", "第2部 ドラゴンと話す",
  "第3章 あいさつの作法", "第4章 怒らせたときは"
]
raise unless Entry.count == 8

# destroy removes its entries and notes (dependent: :destroy).
# destroy すると、目次と感想も消える
work.notes.create!(body: "うろこの色の図がきれい")
work.destroy
raise unless Note.count.zero? && Entry.count.zero?

# The database also cascades when we skip the models (delete).
# モデルを通さない delete でも、DB の ON DELETE CASCADE で消える
again = add_work("はじめてのドラゴン学", DRAGONS)
raise again.id.to_s unless again.id == 2 # not used again / 使い回さない
again.notes.create!(body: "x")
Work.where(id: again.id).delete_all
raise unless Note.count.zero? && Entry.count.zero?

# A negative size in the third entry: create! raises, nothing is left.
# 3つ目の見出しの大きさが負：create! が例外を投げ、何も残らない
add_work("はじめてのドラゴン学", DRAGONS)
bad = [["第1部", 2, [["第1章", 4, []], ["第2章", -1, []]]]]
begin
  add_work("こわれた本", bad)
  raise "should fail"
rescue ActiveRecord::RecordInvalid => e
  raise e.message unless e.message.include?("Size")
end
raise Work.count.to_s unless Work.count == 1
raise Entry.count.to_s unless Entry.count == 8

# The CHECK in the database stops it even without the validation.
# 検証を飛ばしても、DB の CHECK が止める
begin
  Entry.insert!({ work_id: Work.first.id, position: 9, heading: "x",
                  size: -1 })
  raise "should fail"
rescue ActiveRecord::StatementInvalid => e
  raise e.message unless e.message.include?("CHECK")
end
