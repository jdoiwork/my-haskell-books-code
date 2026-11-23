# The line numbers of Ruby's CSV: lineno counts the rows it has read.
# Ruby の CSV の行番号：lineno は、読んだ行の数を数える
require "csv"

text = <<~CSV
  date,category,amount,memo
  2026-11-01,food,1200,lunch
  2026-11-05,transport,450,"bus
  to the workshop"
  2026-11-07,food,abc,typo
CSV

# ANCHOR: lineno
# Ruby
csv = CSV.new(text, headers: true)
seen = []
csv.each do |row|
  seen << [csv.lineno, row["amount"]]
end
# ANCHOR_END: lineno

# The row of "abc" starts on line 5 of the text; lineno says 4.
# "abc" の行は、文字の5行目から始まる。lineno は 4
raise seen.inspect unless seen == [[2, "1200"], [3, "450"], [4, "abc"]]
puts "ok"
