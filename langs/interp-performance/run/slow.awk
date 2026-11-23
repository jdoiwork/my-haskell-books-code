# awk: the requests that failed and were slow, counted by IP (the same
# as examples/perf/slow.lam). awk -f slow.awk big.log
# awk：失敗して遅かった頼みを、IP ごとに数える
# （examples/perf/slow.lam と同じ）
# ANCHOR: slow
# awk
/^[^ ]+ .*" [0-9][0-9][0-9] [0-9]+ rt=[0-9.]+$/ {
  status = $(NF - 2)
  rt = substr($NF, 4)
  if (status >= 500 && rt > 0.5) count[$1]++
}
END {
  # The biggest first; the same counts by IP.
  # 大きい順。同じ数は IP の順
  order = "sort -k1,1nr -k2,2 | head -5"
  for (ip in count) printf "%5d  %s\n", count[ip], ip | order
}
# ANCHOR_END: slow
