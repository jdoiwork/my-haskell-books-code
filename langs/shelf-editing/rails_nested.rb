# Fixing a work and its table of contents in one form, the Rails way:
# accepts_nested_attributes_for. A row that was there sends its id; a new
# row sends none; a row to remove says _destroy. What hangs on a removed
# row is decided by `dependent:` (nullify: keep it, but on nothing).
# For comparing with tsundoku's reviseWork.
# Rails のやり方で、作品と目次を1つのフォームで直す：
# accepts_nested_attributes_for。もとからある行は id を送り、新しい行は
# 送らない。消す行は _destroy と言う。消える行にぶら下がっていたものを
# どうするかは dependent: で決める（nullify：消さずに、どこにも付けない）。
# tsundoku の reviseWork とくらべるため
require "active_record"

ActiveRecord::Base.establish_connection(adapter: "sqlite3",
                                        database: ":memory:")
ActiveRecord::Migration.verbose = false
ActiveRecord::Schema.define do
  create_table(:works) { |t| t.string :title, null: false }
  create_table :entries do |t|
    t.references :work, null: false
    t.integer :position, null: false
    t.string :heading, null: false
  end
  create_table :notes do |t|
    t.references :work, null: false
    t.references :entry
    t.string :body, null: false
  end
  create_table :entry_ratings do |t|
    t.references :entry, null: false
    t.integer :score, null: false
  end
end

# ANCHOR: models
# Ruby on Rails (a work with its rows, fixed in one form)
# Ruby on Rails（作品と行を、1つのフォームで直す）
class Work < ActiveRecord::Base
  has_many :entries, -> { order(:position) }, dependent: :destroy
  has_many :notes, dependent: :destroy
  accepts_nested_attributes_for :entries, allow_destroy: true
end

class Entry < ActiveRecord::Base
  belongs_to :work
  # a note is my own words: keep it, on the whole work
  # 感想は自分の言葉：消さずに、作品全体へ
  has_many :notes, dependent: :nullify
  # a rating of a row that is gone means nothing: it goes too
  # 消えた行の★は意味がない：一緒に消える
  has_many :entry_ratings, dependent: :destroy
  validates :heading, presence: true
  validates :heading, uniqueness: { scope: :work_id }
end

class Note < ActiveRecord::Base
  belongs_to :work
  belongs_to :entry, optional: true
end

class EntryRating < ActiveRecord::Base
  belongs_to :entry
end
# ANCHOR_END: models

dragon = Work.create!(title: "はじめてのドラゴン学")
e1, e2, e3 = ["1.1 うろこの色", "1.2 火を吐くもの", "第2章 すみかと季節"]
             .each_with_index.map do |h, i|
  dragon.entries.create!(heading: h, position: i + 1)
end
dragon.notes.create!(entry: e2, body: "火を吐かない種もいるとは！")
EntryRating.create!(entry: e2, score: 80)

# ANCHOR: update
# Ruby on Rails (what the form sends: ids for the rows that stay)
# Ruby on Rails（フォームが送るもの：残る行には id を）
dragon.update!(entries_attributes: [
  { id: e1.id, heading: "1.1 うろこの色と火", position: 1 },
  { id: e2.id, _destroy: "1" },
  { id: e3.id, position: 2 },
  { heading: "第5章 ドラゴンと暮らす", position: 3 },
])
# ANCHOR_END: update

dragon.reload
raise "rows" unless dragon.entries.map(&:heading) ==
  ["1.1 うろこの色と火", "第2章 すみかと季節", "第5章 ドラゴンと暮らす"]
raise "kept ids" unless dragon.entries.first(2).map(&:id) == [e1.id, e3.id]
raise "note" unless dragon.notes.map(&:entry_id) == [nil]
raise "rating" unless EntryRating.count.zero?

# A row of another work: Rails refuses its id (RecordNotFound)
# よその作品の行：Rails はその id を断る（RecordNotFound）
other = Work.create!(title: "図書室の司書に転生した件")
stranger = other.entries.create!(heading: "第1話", position: 1)
begin
  dragon.update!(entries_attributes: [{ id: stranger.id, heading: "x" }])
  raise "a stranger was taken"
rescue ActiveRecord::RecordNotFound
end

# The same heading twice: the validation refuses, and nothing changes
# 同じ見出しが2つ：検証が断り、何も変わらない
begin
  dragon.update!(entries_attributes: [{ heading: "第2章 すみかと季節",
                                        position: 4 }])
  raise "a heading twice was taken"
rescue ActiveRecord::RecordInvalid
end
raise "unchanged" unless dragon.reload.entries.size == 3
puts "ok"
