# The Elixir code in the regex-base chapter: =~ is true or false, and
# the right side may be a regex or a string.
# regex-base の章の Elixir のコード：=~ は true か false。右は正規表現
# でも文字列でもよい
line = "2026-11-21 Mira sold 300 gold, Ruby sold 25 gold"

# ANCHOR: elixir
# Elixir: =~ is a boolean; Regex.run and Regex.scan give the matches
# Elixir：=~ は真偽。見つけたものは Regex.run と Regex.scan で
found = line =~ ~r/(\w+) sold (\d+)/
plain = line =~ "dragon"
first = Regex.run(~r/(\w+) sold (\d+)/, line)
golds = Regex.scan(~r/(\d+) gold/, line, capture: :all_but_first)
# ANCHOR_END: elixir

true = found
false = plain
["Mira sold 300", "Mira", "300"] = first
[["300"], ["25"]] = golds
IO.puts("ok")
