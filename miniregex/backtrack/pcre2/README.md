# pcre2test で確かめた答え

マッチャーの章（`book/regex-backtrack.md`）が見せる、本物の PCRE2 の答えです。
`*.txt` を `pcre2test` に読ませると、1行目の版の行のあとに、`*.out` と同じものが出ます（PCRE2 10.49 で確かめた）。

```sh
pcre2test leftfirst.txt | tail -n +2 | diff - leftfirst.out
```
