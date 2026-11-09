# My own attributes with Rails (a JSON column and store_accessor), for
# the chapter "Attributes and states of my own". Runs with the gems in
# tools/ruby (activerecord, sqlite3).
# 「自分で決める属性と状態」の章の、自分で決めた属性を Rails で
# （JSON の列と store_accessor）。tools/ruby の gem で動く
require "active_record"

ActiveRecord::Base.establish_connection(adapter: "sqlite3",
                                        database: ":memory:")
ActiveRecord::Migration.verbose = false

ActiveRecord::Schema.define do
  create_table :works do |t|
    t.string :title, null: false
    t.json   :attrs, null: false, default: {}
  end
end

# ANCHOR: rails
# Ruby on Rails (store_accessor: keys of the JSON column as attributes)
# Ruby on Rails（store_accessor：JSON の列のキーを、属性として）
class Work < ActiveRecord::Base
  store_accessor :attrs, :推し度, :形

  validates :推し度, numericality: { only_integer: true }, allow_nil: true
  validates :形, inclusion: { in: %w[紙 電子 Web] }, allow_nil: true
end

work = Work.new(title: "図書室の司書に転生した件")
work.推し度 = "すごく"
work.形 = "巻物"
work.valid?   # => false (two problems / まずい所が2つ)
# ANCHOR_END: rails
raise "valid?" if work.valid?
raise work.errors.attribute_names.inspect unless
  work.errors.attribute_names.sort == %i[形 推し度].sort

# ANCHOR: query
work.推し度 = 5
work.形 = "Web"
work.save!
Work.create!(title: "迷宮図書館の写本係", 推し度: 3, 形: "Web")
Work.create!(title: "はじめてのドラゴン学", 推し度: 4, 形: "紙")

# 🔎 Search inside the JSON with SQLite's json_extract.
# JSON の中を、SQLite の json_extract で絞り込む
found = Work.where("json_extract(attrs, '$.推し度') >= ?", 4)
            .where("json_extract(attrs, '$.形') = ?", "Web")
# ANCHOR_END: query
raise found.map(&:title).inspect unless
  found.map(&:title) == ["図書室の司書に転生した件"]
raise work.attrs.inspect unless
  work.reload.attrs == { "推し度" => 5, "形" => "Web" }
puts "ok"
