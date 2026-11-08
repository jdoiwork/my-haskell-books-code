# Rails pieces (ActiveRecord and ActionView) for the chapter shelf-design.
# Runs with the gems in tools/ruby (activerecord, actionview, sqlite3).
# shelf-design の章の Rails の部品（ActiveRecord と ActionView）。
# tools/ruby の gem（activerecord・actionview・sqlite3）で動く
require "active_record"
require "action_view"

ActiveRecord::Base.establish_connection(adapter: "sqlite3",
                                        database: ":memory:")
ActiveRecord::Migration.verbose = false
ActiveRecord::Schema.define do
  create_table :reviews do |t|
    t.string  :title,   null: false
    t.integer :score
    t.integer :reading, null: false, default: 0
  end
end

# ANCHOR: model
# Ruby on Rails (the model: one score from 0 to 100)
# Ruby on Rails（モデル：0〜100 の点1つ）
class Review < ActiveRecord::Base
  enum :reading, {
    piled: 0, reading: 1, finished: 2, paused: 3, rereading: 4
  }

  validates :score, numericality: {
    only_integer: true, in: 0..100
  }, allow_nil: true
end
# ANCHOR_END: model

# ANCHOR: helper
# Ruby on Rails (a view helper: the shadow of the score)
# Ruby on Rails（ビューのヘルパー：点の影）
module RatingHelper
  STEPS = { hundred: 1, five_stars: 10, out_of_ten: 10 }.freeze

  def rating_text(score, style)
    step = STEPS.fetch(style)
    steps = (score + step / 2) / step
    case style
    when :hundred    then "#{score}点"
    when :five_stars then "★#{steps / 2}#{".5" if steps.odd?}"
    when :out_of_ten then "#{steps}/10"
    end
  end
end
# ANCHOR_END: helper

TEMPLATE = <<~ERB
  <%# ANCHOR: view %>
  <%# Rails (the view: the same reviews, in the reader's style) %>
  <%# Rails（ビュー：同じ感想を、読む人の好きな見せ方で） %>
  <% @reviews.each do |review| %>
    <p><%= review.title %>：<%= rating_text(review.score, @style) %></p>
  <% end %>
  <%# ANCHOR_END: view %>
ERB

# 🖼️ Render the template with a style, as a controller would.
# コントローラーのように、見せ方を決めてテンプレートを描く
def render_reviews(style)
  view = ActionView::Base.with_empty_template_cache.new(
    ActionView::LookupContext.new([]), { reviews: Review.order(:id),
                                         style: style }, nil)
  view.extend(RatingHelper)
  view.render(inline: TEMPLATE).split("\n").map(&:strip).reject(&:empty?)
end

dragon = Review.create!(title: "はじめてのドラゴン学", score: 73)
Review.create!(title: "最後のドラゴン", score: 75, reading: :finished)

# The model keeps the score; the views only show it.
# モデルは点を持ち、ビューは見せるだけ
raise unless render_reviews(:hundred) ==
             ["<p>はじめてのドラゴン学：73点</p>", "<p>最後のドラゴン：75点</p>"]
raise unless render_reviews(:five_stars) ==
             ["<p>はじめてのドラゴン学：★3.5</p>", "<p>最後のドラゴン：★4</p>"]
raise unless render_reviews(:out_of_ten) ==
             ["<p>はじめてのドラゴン学：7/10</p>", "<p>最後のドラゴン：8/10</p>"]
raise unless dragon.reload.score == 73

# validates refuses 101 when saving; enum refuses an unknown state at once.
# validates は保存のときに 101 を断る。enum は知らない状態を、すぐに断る
bad = Review.new(title: "?", score: 101)
raise if bad.valid?
raise unless bad.errors[:score].any?
raise unless dragon.piled? && Review.finished.count == 1
begin
  dragon.reading = :pilled
  raise "enum took a typo"
rescue ArgumentError
end
puts "ok"
