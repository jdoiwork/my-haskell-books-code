# The same recipes with Python's one-liners (python3 -c). Run by
# check_recipes.py.
# 同じレシピを、Python のワンライナー（python3 -c）で。check_recipes.py が
# 動かす

# ANCHOR: text-unique
# Python: dict keeps the first of each key, in order
# dict は、キーごとに最初のものを、順に残す
python3 -c 'import sys; print(*dict.fromkeys(sys.stdin.read().splitlines()),
                               sep="\n")' < fruits.txt
# ANCHOR_END: text-unique

# ANCHOR: data-sum
# Python: csv.DictReader and Counter
# csv.DictReader と Counter で
python3 -c '
import csv, collections
sums = collections.Counter()
for row in csv.DictReader(open("expenses.csv")):
    sums[row["category"]] += int(row["amount"])
for c, n in sums.most_common():
    print(f"{c:<8}{n:>8,}")
print("total".ljust(8) + f"{sum(sums.values()):>8,}")'
# ANCHOR_END: data-sum

# ANCHOR: data-tojsonl
# Python: each row as one line of JSON (no spaces, the keys in order)
# 1行を1行の JSON に（空白なし、キーは並べて）
python3 -c '
import csv, json
for row in csv.DictReader(open("users.csv")):
    print(json.dumps(row, separators=(",", ":"), sort_keys=True))'
# ANCHOR_END: data-tojsonl
