# The Ruby code in the regex-base chapter: =~ gives where the match
# starts (or nil), and sets $~ and, for named groups, local variables.
# regex-base の章の Ruby のコード：=~ は一致が始まる場所（か nil）を
# 返し、$~ を、名前付きのグループならローカル変数も作る
RSpec.describe "=~ in Ruby" do
  line = "2026-11-21 Mira sold 300 gold, Ruby sold 25 gold"

  # gives the place where the match starts, or nil
  # 一致が始まる場所か、nil を返す
  it "gives the place, or nil" do
    # ANCHOR: place
    # Ruby: =~ is the place (counted in characters) or nil
    # Ruby：=~ は場所（文字で数える）か nil
    at = line =~ /(\w+) sold (\d+)/
    found = $~                      # the MatchData of the last =~
    none = line =~ /dragon/
    # ANCHOR_END: place
    expect(at).to eq(11)
    expect(found[1]).to eq("Mira")
    expect(found[2]).to eq("300")
    expect(none).to be_nil
    expect("魔法の 300 gold" =~ /\d+/).to eq(4)
  end

  # named groups on the left make local variables
  # 名前付きのグループを左に書くと、ローカル変数ができる
  it "makes local variables from named groups" do
    # ANCHOR: named
    # Ruby: a regex literal on the left, with named groups
    # Ruby：左に正規表現を書き、名前付きのグループを使う
    if /(?<seller>\w+) sold (?<gold>\d+)/ =~ line
      sale = "#{seller}: #{gold}"
    end
    # ANCHOR_END: named
    expect(sale).to eq("Mira: 300")
  end
end
