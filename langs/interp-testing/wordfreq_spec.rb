# The comparison code of the chapter "Tests and debugging: lam test".
# RSpec: describe and it make the tree of tests (like Test.group and
# Test.equal); the fake world is a StringIO in place of $stdin.
# 「テストとデバッグ：lam test」の章の比較のコード。RSpec の describe と
# it がテストの木を作る（Test.group と Test.equal のように）。偽物の世界は、
# $stdin の代わりの StringIO
require "stringio"

# ANCHOR: code
# Ruby (the code to test)
# Ruby（テストするコード）
def split_into_words(text)
  text.downcase.scan(/[a-z']+/)
end

def wordfreq(input, out)
  counts = split_into_words(input.read).tally
  counts.sort_by { |word, n| [-n, word] }.first(3).each do |word, n|
    out.puts(format("%-8s%d", word, n))
  end
end
# ANCHOR_END: code

# ANCHOR: spec
# Ruby (RSpec)
# Ruby（RSpec）
# 「split_into_words」
RSpec.describe "split_into_words" do
  # 「小文字の単語にする」
  it "makes lowercase words" do
    expect(split_into_words("Hello, World!")).to eq(["hello", "world"])
  end

  # 「don't は1語のまま」
  it "keeps don't as one word" do
    expect(split_into_words("Don't panic")).to eq(["don't", "panic"])
  end
end

# 「wordfreq」
RSpec.describe "wordfreq" do
  # 「標準入力を読む」
  it "reads the standard input" do
    $stdin = StringIO.new("b a b\nc a b\n")
    expect { wordfreq($stdin, $stdout) }
      .to output("b       3\na       2\nc       1\n").to_stdout
  ensure
    $stdin = STDIN
  end
end
# ANCHOR_END: spec
