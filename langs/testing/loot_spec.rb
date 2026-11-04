# The RSpec code in the testing chapter.
# テストの章の RSpec のコード
module Loot
  def self.split_draft(gold, members)
    Array.new(members, gold / members)
  end

  def self.split(gold, members)
    share, rest = gold.divmod(members)
    Array.new(members) { |i| i < rest ? share + 1 : share }
  end
end
# ANCHOR: rspec
# Ruby (RSpec)
# Ruby（RSpec）
RSpec.describe Loot do
  describe ".split_draft" do
    # 「100 ゴールドを4人で分ける」
    it "splits 100 gold among 4 members" do
      expect(Loot.split_draft(100, 4)).to eq([25, 25, 25, 25])
    end
  end
end
# ANCHOR_END: rspec
# ANCHOR: rspec_shared_examples
# Ruby (RSpec, shared examples)
# Ruby（RSpec の shared examples）
# 「公平な分け方」の共通のテスト
RSpec.shared_examples "a fair split" do |split|
  # 「合計は変わらない」
  it "keeps the total" do
    expect(split.call(10, 3).sum).to eq(10)
  end
end

RSpec.describe Loot do
  # 「公平な分け方として、ふるまう」
  it_behaves_like "a fair split", Loot.method(:split)
end
# ANCHOR_END: rspec_shared_examples
