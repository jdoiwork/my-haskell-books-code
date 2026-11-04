# モダン Haskell 入門〜中級：サンプルコード

技術書『モダン Haskell 入門〜中級』のサンプルコードです。本文に載っているコードは、すべてここから取りこんでいます。

Sample code for the Japanese book on modern Haskell. Every code listing in the book is included from these files.

## 動かすのに必要なもの

- GHC 9.14.1 と cabal 3.16（本の「環境構築」の章を参照。GHCup で入れられます）
- 一部の章は、ほかの道具も使います（PostgreSQL の章は Docker、WebAssembly の章は ghc-wasm-meta など）。それぞれの章の本文を見てください

## ビルドとテスト

```sh
cabal build all
cabal test all
```

`cabal.project` に、ほとんどのパッケージが入っています。依存の都合で、次のディレクトリーは、それぞれの中に自分の `cabal.project` を持つ別のプロジェクトです。そのディレクトリーに入ってから `cabal build` してください。

- `dbcompare`、`guildapi`、`guilddesign`、`guildpg`、`guildrel8`（GHC 9.10.3）、`guildtyped`
- `review3`、`review6`、`review7`、`review8`、`review9`、`upsert`

`wasm` は、ghc-wasm-meta の `wasm32-wasi-ghc` で組み立てます（`wasm/build.sh`）。

## ディレクトリー

- 章ごとに1つのディレクトリーがあります（例：`setup` は「環境構築」の章、`minisearch` は「ミニ全文検索エンジン」の部）
- `answers/` は、各章のトレーニングの解答です
- `broken/` は、わざとコンパイルが通らないファイルです（本文で、エラーメッセージを見せるために使います）
- `langs/` は、本文で比べている Python・TypeScript・Rust・Ruby のコードです

## ライセンス

MIT License（`LICENSE` を参照）
