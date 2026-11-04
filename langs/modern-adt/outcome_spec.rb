# The outcome of a quest, for the chapter "ADTs are everywhere".
# 依頼の結果。「代数的データ型は、どこにでもある」の章

# ANCHOR: pattern
# Ruby (Data.define and case/in)
# Ruby（Data.define と case/in）
Cleared = Data.define(:gold)
Failed  = Data.define(:place)
Fled    = Data.define

def describe(outcome)
  case outcome
  in Cleared[gold]  then "cleared! +#{gold} G"
  in Failed[place]  then "failed at #{place}"
  in Fled           then "ran away"
  end
end
# ANCHOR_END: pattern

Rescued = Data.define(:by)

RSpec.describe "describe" do
  # 3つの場合を言える
  it "describes each outcome" do
    expect(describe(Cleared.new(50))).to eq("cleared! +50 G")
    expect(describe(Failed.new("cave"))).to eq("failed at cave")
    expect(describe(Fled.new)).to eq("ran away")
  end

  # 知らない場合は、動かしたときにエラーになる
  it "raises at run time for a case it does not know" do
    expect { describe(Rescued.new("Python")) }
      .to raise_error(NoMatchingPatternError)
  end
end
