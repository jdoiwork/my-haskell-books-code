# The Ruby code in the chapter on recursion of MiniRegex: Onigmo, Ruby's
# engine, calls a group with \g<name> (the syntax PCRE2 took), sets the
# group it calls, and refuses a recursion that would never end, before
# running.
# MiniRegex の再帰の章の Ruby のコード：Ruby のエンジンの鬼雲は、
# \g<name> でグループを呼ぶ（PCRE2 が取り入れた書き方）。呼んだグループを
# 書き換え、終わらない再帰は、動かす前に断る
memo = "Mira (healer (2nd class) from Lam (east)) paid 120 gold (cash)"

# ANCHOR: call
# Ruby: \g<0> calls the whole pattern; \g<note> calls the group note
# Ruby：\g<0> はパターン全体を、\g<note> は note のグループを呼ぶ
p memo.scan(/\((?:[^()]++|\g<0>)*\)/)
p memo.scan(/(?<note>\((?:[^()]++|\g<note>)*\))/).flatten
# ANCHOR_END: call

raise unless memo.scan(/\((?:[^()]++|\g<0>)*\)/) ==
             ["(healer (2nd class) from Lam (east))", "(cash)"]

# ANCHOR: sets
# Ruby: the call writes over the group (PCRE2 and Perl keep "sens")
# Ruby：呼び出しがグループを書き換える（PCRE2 と Perl は "sens" のまま）
m = /(?<w>sens|respons)e and \g<w>ibility/.match("sense and responsibility")
p m[:w]
# ANCHOR_END: sets

raise unless m[:w] == "respons"

# ANCHOR: never
# Ruby: a recursion that would never end is wrong when it is made
# Ruby：終わらない再帰は、正規表現を作るときにまちがい
begin
  Regexp.new('a|\g<0>b')
rescue RegexpError => e
  p e.message
end
# ANCHOR_END: never

begin
  Regexp.new('a|\g<0>b')
  raise "not refused"
rescue RegexpError => e
  raise unless e.message.start_with?("never ending recursion")
end
