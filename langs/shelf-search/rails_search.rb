# Full-text search in Rails with SQLite's FTS5, for the chapter
# "Searching what is inside". Runs with the gems in tools/ruby
# (activerecord, sqlite3).
# 「中身を探す」の章の全文検索を、Rails と SQLite の FTS5 で。
# tools/ruby の gem（activerecord・sqlite3）で動く
require "active_record"

ActiveRecord::Base.establish_connection(adapter: "sqlite3",
                                        database: ":memory:")
ActiveRecord::Migration.verbose = false

# ANCHOR: migration
# Ruby on Rails (a migration: an FTS5 table beside the notes)
# Ruby on Rails（マイグレーション：感想の横に FTS5 の表）
ActiveRecord::Schema.define do
  create_table :notes do |t|
    t.string :body, null: false
  end
  create_virtual_table :documents, :fts5,
                       ["body", "note_id UNINDEXED", "tokenize = 'trigram'"]
end
# ANCHOR_END: migration

# ANCHOR: models
# Ruby on Rails (models: a callback keeps the FTS5 table in step)
# Ruby on Rails（モデル：コールバックが FTS5 の表を追いかける）
class Document < ActiveRecord::Base
  scope :matching, ->(typed) {
    words = typed.split.select { |w| w.length >= 3 }
    query = words.map { |w| %("#{w.gsub('"', '""')}") }.join(" ")
    where("documents MATCH ?", query).order(:rank)
  }
end

class Note < ActiveRecord::Base
  # an FTS5 table has no id column, so insert the row as it is
  # FTS5 の表には id の列がないので、行をそのまま入れる
  after_create do
    Document.insert!({ body: body, note_id: id }, returning: false)
  end
end
# ANCHOR_END: models

Note.create!(body: "絵が多くて読みやすい")
Note.create!(body: "ドラゴンは怠けものではないと知った")
Note.create!(body: "記号 \"引用\" と NEAR( も")

raise "怠けもの" unless Document.matching("怠けもの").pluck(:note_id) == [2]
raise "NEAR(" unless Document.matching("NEAR(").pluck(:note_id) == [3]
raise "quote" unless Document.matching('"引用"').pluck(:note_id) == [3]
# a word of 2 characters: MATCH finds nothing (no error)
# 2文字の語：MATCH は何も見つけない（エラーにはならない）
raise "絵が" unless Document.where("documents MATCH ?", '"絵が"').empty?
puts "ok"
