# Enumerable in Ruby: write each, and get max, sum, any? and the rest.
# Ruby の Enumerable：each を書けば、max・sum・any? などが付いてくる

# ANCHOR: shelf
# Ruby
class Shelf
  include Enumerable

  def initialize(*scores)
    @scores = scores
  end

  # each is the one thing Enumerable asks for.
  # Enumerable が求めるのは、each だけ
  def each(&block)
    @scores.each(&block)
    self
  end
end

shelf = Shelf.new(82, 75, 90)
shelf.max                  # => 90
shelf.sum                  # => 247
shelf.any? { |s| s < 80 }  # => true
[].max                     # => nil
# ANCHOR_END: shelf

RSpec.describe "Enumerable" do
  it "gives max, sum and any? from each" do
    expect(shelf.max).to eq(90)
    expect(shelf.sum).to eq(247)
    expect(shelf.any? { |s| s < 80 }).to be(true)
    expect(shelf.sort).to eq([75, 82, 90])
  end

  it "gives nil for the max of nothing" do
    expect([].max).to be_nil
    expect(Shelf.new.max).to be_nil
  end

  # A Hash gives [key, value] pairs to Enumerable.
  # Hash が Enumerable に渡すのは [キー, 値] の組
  it "folds the pairs of a Hash" do
    totals = { "Python" => 247, "TS" => 159 }
    expect(totals.max).to eq(["TS", 159])
    expect(totals.values.max).to eq(247)
  end
end
