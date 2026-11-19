# PCRE2 のテストデータ

`testoutput1` は、PCRE2 10.49（2026-09-28）の配布物の `testdata/testoutput1` を、そのまま写したものです。
取ってきた場所：<https://github.com/PCRE2Project/pcre2/blob/pcre2-10.49/testdata/testoutput1>

PCRE2 の `LICENCE.md` によると、`testdata` のフォルダーのデータは著作権がなく、パブリックドメインです
（"The data in the testdata directory is not copyrighted and is in the public domain."）。

PCRE2 の試験の道具 `pcre2test` に `testinput1` を読ませたときの出力で、パターン・入力・期待する答えが、
全部この1つのファイルに入っています。互換度メーター（`cabal run compat-meter`）が読みます。
