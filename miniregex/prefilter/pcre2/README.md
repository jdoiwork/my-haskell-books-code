# pcre2test で確かめた答え

先に探す章（`book/regex-prefilter.md`）が見せる、本物の PCRE2 の答えです。
`*.txt` を `pcre2test` に読ませると、1行目の版の行のあとに、`*.out` と同じものが出ます（PCRE2 10.49 で確かめた）。

```sh
pcre2test prefilter.txt | tail -n +2 | diff - prefilter.out
```

`prefilter.txt` は、`/I` で PCRE2 がパターンから先に調べること（最初の文字・最初の文字の集合・最後に必ず要る文字・頭に固定されているか・長さの下限）を見せ、`(a+)*b` が「必ず要る文字」でただちに No match になること、`no_start_optimize` で先に見るのをやめると手数の上限に届くこと、同じ場所で回る再帰も、必ず要る文字がなければ No match になることを確かめます。
