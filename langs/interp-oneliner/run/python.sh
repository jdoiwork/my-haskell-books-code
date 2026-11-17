# Python (3.15): -c takes a program, but a for with an if inside needs
# new lines and indents. Run by check_oneliner.py.
# Python の -c。for の中に if を書くには、改行と字下げが要る

# ANCHOR: book
# Python

# ANCHOR: grep
# a list comprehension fits in one line
# 内包表記なら1行
python3 -c 'import sys; [print(l, end="") for l in sys.stdin if "ERROR" in l]' < app.log
# ANCHOR_END: grep

# ANCHOR: grepLoop
# a for with an if needs new lines
# for と if には改行が要る
python3 -c 'import sys
for line in sys.stdin:
    if "ERROR" in line:
        print(line, end="")' < app.log
# ANCHOR_END: grepLoop
# ANCHOR_END: book
