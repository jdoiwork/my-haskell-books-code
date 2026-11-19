# pcre2test で確かめた答え

グループとキャプチャの章（`book/regex-captures.md`）が見せる、本物の PCRE2 の答えです。
`*.txt` を `pcre2test` に読ませると、1行目の版の行のあとに、`*.out` と同じものが出ます（PCRE2 10.49 で確かめた）。

```sh
pcre2test groups.txt | tail -n +2 | diff - groups.out
```
