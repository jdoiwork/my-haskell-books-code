# pcre2test で確かめた答え

測って比べる章（`book/regex-benchmarks.md`）が見せる、本物の PCRE2 の答えです。
`*.txt` を `pcre2test` に読ませると、1行目の版の行のあとに、`*.out` と同じものが出ます（PCRE2 10.49 で確かめた）。

```sh
pcre2test limit.txt | tail -n +2 | diff - limit.out
```

`limit.txt` は、名前の欄のパターン `^(\w+\s?)+$` を、名前の数を変えて試します。名前が7個までは「No match」と答え、8個で、ステップ数の上限（match limit、既定は 1000万）を超えて `error -47` になります。JIT でも同じです。
