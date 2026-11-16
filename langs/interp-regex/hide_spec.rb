# The Ruby code in the chapter on regular expressions: =~ and gsub with
# a block.
# 正規表現の章の Ruby のコード：=~ と、ブロックを渡す gsub
RSpec.describe "regular expressions in Ruby" do
  # 「=~ は場所を返し、$~ に見つかったものが入る」
  it "finds with =~ and puts the match in $~" do
    # ANCHOR: match
    # Ruby: =~ is an operator of the language
    # Ruby：=~ は言語の演算子
    line = "2026-11-16 09:15:40 ERROR order 1043"
    if line =~ /(?<date>\d{4}-\d\d-\d\d) [\d:]+ (?<level>[A-Z]+)/
      puts "#{$~[:date]} #{$~[:level]}"
    end
    # ANCHOR_END: match
    expect($~[:level]).to eq("ERROR")
    expect(line =~ /ERROR/).to eq(20)
  end

  # 「gsub にブロックを渡すと、見つかったものを関数で置き換える」
  it "replaces with a block" do
    text = "call 080-9876-5432 or 03-1234-5678 now"
    # ANCHOR: gsub
    # Ruby: gsub with a block
    # Ruby：ブロックを渡す gsub
    hidden = text.gsub(/\d{2,4}-\d{2,4}-(?<last>\d{4})/) do
      "***-****-#{$~[:last]}"
    end
    # ANCHOR_END: gsub
    expect(hidden).to eq("call ***-****-5432 or ***-****-5678 now")
  end
end
