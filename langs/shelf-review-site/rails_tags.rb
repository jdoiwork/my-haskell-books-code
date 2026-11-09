# Tags in Rails, the way acts-as-taggable-on and most apps do it: a table
# of tags and a table that joins them to works (many to many). For
# comparing with tsundoku, which keeps a work's tags as one JSON column.
# Rails のタグ。acts-as-taggable-on や多くのアプリと同じく、タグの表と、
# タグと作品を結ぶ表（多対多）で。作品のタグを JSON の列1つにしまう
# tsundoku とくらべるため
require "active_record"

ActiveRecord::Base.establish_connection(adapter: "sqlite3",
                                        database: ":memory:")
ActiveRecord::Migration.verbose = false
ActiveRecord::Schema.define do
  create_table(:works) { |t| t.string :title, null: false }
  create_table(:tags) { |t| t.string :name, null: false, index: { unique: true } }
  create_table :taggings do |t|
    t.references :work, null: false
    t.references :tag,  null: false
    t.index %i[work_id tag_id], unique: true
  end
end

# ANCHOR: models
# Ruby on Rails (tags through a join table)
# Ruby on Rails（結ぶ表を通したタグ）
class Tag < ActiveRecord::Base
  has_many :taggings
  has_many :works, through: :taggings
  validates :name, presence: true, length: { maximum: 30 },
                   format: { without: /[,、]/ }
end

class Tagging < ActiveRecord::Base
  belongs_to :work
  belongs_to :tag
end

class Work < ActiveRecord::Base
  has_many :taggings, dependent: :destroy
  has_many :tags, through: :taggings

  # "推理, 魔法" -> the tags 推理 and 魔法 (made if new)
  # 「推理, 魔法」→ タグ 推理 と 魔法（なければ作る）
  def tag_list=(typed)
    names = typed.split(/[,、]/).map(&:strip).reject(&:empty?).uniq
    self.tags = names.map { |n| Tag.find_or_create_by!(name: n) }
  end
end
# ANCHOR_END: models

# ANCHOR: cloud
# Ruby on Rails (the tag cloud: count the joins, most first)
# Ruby on Rails（タグの雲：結びを数えて、多い順）
def tag_cloud
  Tag.joins(:taggings).group(:name)
     .order(Arel.sql("COUNT(*) DESC"), :name).count
end
# ANCHOR_END: cloud

dragon = Work.create!(title: "はじめてのドラゴン学")
dragon.tag_list = "竜, 図鑑、魔法"
copyist = Work.create!(title: "迷宮図書館の写本係")
copyist.tag_list = "図書館, 迷宮, 魔法"

raise "tags" unless dragon.tags.map(&:name).sort == %w[図鑑 竜 魔法].sort
raise "cloud: #{tag_cloud}" unless tag_cloud.first == ["魔法", 2]

# A tag with a comma is refused by the validation
# カンマ入りのタグは、検証が断る
begin
  Tag.create!(name: "a,b")
  raise "a comma was taken"
rescue ActiveRecord::RecordInvalid
end
puts "ok"
