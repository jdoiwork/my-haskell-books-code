# pcre2test で確かめた答え

しおりと命令を磨く章（`book/regex-cursor.md`）が見せる、本物の PCRE2 の答えです。
`*.txt` を `pcre2test` に読ませると、1行目の版の行のあとに、`*.out` と同じものが出ます（PCRE2 10.49 で確かめた）。

```sh
pcre2test possess.txt | tail -n +2 | diff - possess.out
```

`possess.txt` は、`/B` で PCRE2 がコンパイルした命令の列を見せます。`a*b` の `a*` は、`a` と `b` が重ならないので、所有的な `a*+` に書き換えられます（自動の所有化）。`no_auto_possess` を付けると、書き換えません。`a*a` は、次の文字も `a` なので書き換えません。`^(\w+\s?)+$` の `\w+` も、うしろの `\s?` が何も食べないことがあるので、書き換えません。
