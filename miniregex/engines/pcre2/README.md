# pcre2test で確かめた答え

エンジンを乗り換える章（`book/regex-engines.md`）が見せる、本物の PCRE2 の答えです。
`*.txt` を `pcre2test` に読ませると、1行目の版の行のあとに、`*.out` と同じものが出ます（PCRE2 10.49 で確かめた）。

```sh
pcre2test possess.txt | tail -n +2 | diff - possess.out
```

`possess.txt` は、`/B` で PCRE2 がコンパイルした命令の列を見せます（自動の所有化）。`\d+ gold`・`[^"]*"`・`(?i)a+B` は、MiniRegex と同じく所有的になります。`a*\b` と `(a+)+b` は、どちらもなりません。`\w+\s?$` と、控えめな `a+?b` は、PCRE2 だけが所有的にします（文字の集まりどうしもくらべ、`\s?` の先の `$` まで見て、控えめなくり返しも書き換える）。
