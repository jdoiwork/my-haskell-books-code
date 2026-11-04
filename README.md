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

- `dbcompare`、`guildapi`、`guilddesign`、`guildpg`、`guildrel8`（GHC 9.10.3）、`guildtyped`、`guildweb`（React の画面は `guildweb/frontend` で `npm install`。`docker compose up` でも動かせます）
- `review3`、`review6`、`review7`、`review8`、`review9`、`upsert`

`wasm` は、ghc-wasm-meta の `wasm32-wasi-ghc` で組み立てます（`wasm/build.sh`）。

## ディレクトリー

- 章ごとに1つのディレクトリーがあります（例：`setup` は「環境構築」の章、`minisearch` は「ミニ全文検索エンジン」の部）
- `answers/` は、各章のトレーニングの解答です
- `broken/` は、わざとコンパイルが通らないファイルです（本文で、エラーメッセージを見せるために使います）
- `langs/` は、本文で比べている Python・TypeScript・Rust・Ruby のコードです

## 章ごとのコード

<!-- chapter-index: begin -->
| 章 | コード |
|---|---|
| はじめに | [`intro/`](intro/)、[`langs/introduction/`](langs/introduction/) |

### 第I部 走り出す

| 章 | コード |
|---|---|
| 環境構築と最初の5分 | [`setup/`](setup/)、[`setup/wordfreq/`](setup/wordfreq/)、[`setup/compare/`](setup/compare/)、[`langs/setup/`](langs/setup/) |
| 式・型・関数：型で考える | [`expressions/`](expressions/)、[`langs/expressions/`](langs/expressions/) |
| 場合分けと Maybe：case で形を見分ける | [`branching/`](branching/)、[`langs/branching/`](langs/branching/) |
| リスト：データの行列 | [`lists/`](lists/)、[`langs/lists/`](langs/lists/) |
| 高階関数：パイプラインで考える | [`higher-order/`](higher-order/)、[`langs/higher-order/`](langs/higher-order/) |
| 高階関数の道具箱：zipWith から groupBy まで | [`hof-toolbox/`](hof-toolbox/)、[`langs/hof-toolbox/`](langs/hof-toolbox/) |
| Map：キーで引く辞書 | [`maps/`](maps/)、[`upsert/`](upsert/)、[`langs/maps/`](langs/maps/) |
| Text：実用的な文字列 | [`textbook/`](textbook/) |
| データをモデリングする：選択肢を型にする | [`modeling/`](modeling/)、[`langs/data-modeling/`](langs/data-modeling/) |
| レコードと newtype：データに名前とラベルを付ける | [`modeling/`](modeling/)、[`langs/records/`](langs/records/) |
| 型クラス入門：Show・Eq・Ord | [`typeclasses/`](typeclasses/) |
| deriving を使いこなす：Enum・Bounded・Read・Num | [`typeclasses/`](typeclasses/) |
| 型クラスを作る：自分だけの資格 | [`typeclasses/`](typeclasses/)、[`langs/custom-type-classes/`](langs/custom-type-classes/) |
| 動くプログラムを書く：IO と do でミニ RPG | [`minirpg/`](minirpg/)、[`minirpg/compare/`](minirpg/compare/)、[`langs/real-programs/`](langs/real-programs/) |
| ファイルと引数：プログラムを外の世界とつなぐ | [`minirpg/`](minirpg/)、[`book-prelude/`](book-prelude/) |
| 集計する：Map で殿堂の成績表を作る | [`minirpg/`](minirpg/) |
| 競技プログラミングの入出力：読んで、解いて、書く | [`cpintro/`](cpintro/)、[`langs/cp-io/`](langs/cp-io/) |
| 第I部のふりかえり：ベースキャンプで地図を広げる | [`review/`](review/) |

### 第II部 実戦の道具箱

| 章 | コード |
|---|---|
| cabal プロジェクトと開発環境 | [`guild/`](guild/)、[`cabal.project`](cabal.project) |
| エラー処理 | [`registry/`](registry/)、[`langs/error-handling/`](langs/error-handling/) |
| デバッグとログ：プログラムの中をのぞく | [`debuglog/`](debuglog/) |
| 関数とドキュメントの探し方：Hoogle・Hackage・GHCi、そして AI | [`finding/`](finding/) |
| よく使うライブラリ | [`libtour/`](libtour/) |
| JSON を扱う：aeson でシリアライズ・デシリアライズ | [`jsonbook/`](jsonbook/)、[`langs/json/`](langs/json/)、[`langs/cargo/`](langs/cargo/) |
| テスト | [`loot/`](loot/)、[`langs/testing/`](langs/testing/)、[`langs/cargo/`](langs/cargo/) |
| CLI ツールを作る | [`mondex/`](mondex/)、[`langs/cli-tool/`](langs/cli-tool/)、[`langs/cargo/`](langs/cargo/) |
| サブコマンドのある CLI ツール | [`mondex/`](mondex/)、[`langs/cli-subcommands/`](langs/cli-subcommands/)、[`langs/cargo/`](langs/cargo/) |
| Monad 入門 | [`monadintro/`](monadintro/)、[`langs/monad-intro/`](langs/monad-intro/) |
| 第II部のふりかえり | [`review2/`](review2/) |

### 第III部 データベースと Web サービス

| 章 | コード |
|---|---|
| データベース入門：SQLite と sqlite-simple で CRUD | [`guilddb/`](guilddb/)、[`langs/database/`](langs/database/)、[`langs/cargo/`](langs/cargo/) |
| データベースを育てる：表の関係、マイグレーション、トランザクション | [`guildquest/`](guildquest/)、[`langs/db-schema/`](langs/db-schema/) |
| 型で SQL を守る：selda と ORM の世界 | [`guildtyped/`](guildtyped/)、[`dbcompare/`](dbcompare/)、[`langs/db-typed/`](langs/db-typed/)、[`langs/cargo/`](langs/cargo/) |
| データベースをテストする | [`guildtest/`](guildtest/)、[`langs/db-testing/`](langs/db-testing/) |
| PostgreSQL へ：サーバーの DB とコンテナ | [`guildpg/`](guildpg/)、[`langs/db-postgres/`](langs/db-postgres/) |
| hasql と rel8：PostgreSQL を型で使い倒す | [`guildrel8/`](guildrel8/)、[`langs/db-hasql/`](langs/db-hasql/) |
| DB に縛られない設計：SQLite と PostgreSQL を切り替える | [`guilddesign/`](guilddesign/)、[`langs/db-design/`](langs/db-design/) |
| Web API を作る：servant で、型が API の設計図になる | [`guildapi/`](guildapi/)、[`langs/web-api/`](langs/web-api/) |
| セキュリティ：型で守れるもの、守れないもの | [`guildsecure/`](guildsecure/)、[`langs/security/`](langs/security/) |
| Web アプリを作る：React の画面と Haskell の API を Docker で動かす | [`guildweb/`](guildweb/)、[`hsx/`](hsx/) |
| 第III部のふりかえり | [`review3/`](review3/) |

### 第IV部 速く書く・速く動かす

| 章 | コード |
|---|---|
| 遅延評価とパフォーマンスの基本 | [`laziness/`](laziness/)、[`langs/laziness/`](langs/laziness/) |
| データ構造の選び方 | [`datastructs/`](datastructs/)、[`langs/data-structures/`](langs/data-structures/) |
| 計測とチューニング | [`tuning/`](tuning/)、[`langs/measuring/`](langs/measuring/) |
| 競技プログラミング | [`compete/`](compete/)、[`langs/competitive/`](langs/competitive/) |
| 並行・並列 | [`concurrency/`](concurrency/)、[`langs/concurrency/`](langs/concurrency/) |
| 第IV部のふりかえり | [`review4/`](review4/) |

### 第V部 抽象の力

| 章 | コード |
|---|---|
| Functor・Applicative・Monad をもっと深く | [`deepmonad/`](deepmonad/)、[`langs/functor-applicative-monad/`](langs/functor-applicative-monad/) |
| Reader・Writer・State：よく使う3つのモナド | [`rws/`](rws/)、[`langs/reader-writer-state/`](langs/reader-writer-state/) |
| IO の正体：IO モナドもどきを作る | [`fakeio/`](fakeio/)、[`langs/fake-io/`](langs/fake-io/) |
| Foldable・Traversable と汎用的な部品 | [`foldtrav/`](foldtrav/)、[`langs/foldable-traversable/`](langs/foldable-traversable/) |
| lens：入れ子のデータを自在に操る | [`lensbook/`](lensbook/)、[`langs/lens/`](langs/lens/) |
| モナド変換子とアプリケーション設計 | [`appdesign/`](appdesign/)、[`langs/application-architecture/`](langs/application-architecture/) |
| GHC 拡張 | [`ghcext/`](ghcext/)、[`langs/ghc-extensions/`](langs/ghc-extensions/) |
| 小さな型安全 SQL を作る | [`minisql/`](minisql/)、[`langs/typed-sql/`](langs/typed-sql/) |
| 型レベルプログラミング入門 | [`typelevel/`](typelevel/)、[`langs/type-level/`](langs/type-level/) |
| 第V部のふりかえり | [`review5/`](review5/) |

### 第VI部 アルゴリズムを Haskell らしく

| 章 | コード |
|---|---|
| 仕様から始める：素朴な解を、式の変形で速くする | [`algo/`](algo/)、[`langs/algo-thinking/`](langs/algo-thinking/) |
| 探索：リストモナドで全部試す | [`algo/`](algo/)、[`langs/algo-search/`](langs/algo-search/) |
| 分割統治：分けて、解いて、まとめる | [`algo/`](algo/)、[`langs/algo-divide/`](langs/algo-divide/) |
| 動的計画法：遅延評価がメモを取る | [`algo/`](algo/)、[`langs/algo-dp/`](langs/algo-dp/) |
| 貪欲法と優先度つきキュー | [`algo/`](algo/)、[`langs/algo-greedy/`](langs/algo-greedy/) |
| グラフのアルゴリズム | [`algo/`](algo/)、[`langs/algo-graphs/`](langs/algo-graphs/) |
| 文字列のアルゴリズム | [`algo/`](algo/)、[`langs/algo-strings/`](langs/algo-strings/) |
| 再帰スキーム：fold と unfold を組み立てる | [`algo/`](algo/)、[`langs/algo-schemes/`](langs/algo-schemes/) |
| 純粋なデータ構造を作る | [`algo/`](algo/)、[`langs/algo-purely/`](langs/algo-purely/) |
| 第VI部のふりかえり | [`algo/`](algo/)、[`review-algo/`](review-algo/) |

### 第VII部 総合演習1：ミニ全文検索エンジンを育てる

| 章 | コード |
|---|---|
| 設計：型でドメインを描く | [`minisearch/`](minisearch/)、[`minisearch/01-design/`](minisearch/01-design/)、[`langs/project-design/`](langs/project-design/) |
| インデクサーを CLI ツールにする | [`minisearch/02-cli/`](minisearch/02-cli/)、[`langs/project-indexer-cli/`](langs/project-indexer-cli/) |
| 検索 Web API を公開する | [`minisearch/03-api/`](minisearch/03-api/)、[`langs/project-search-api/`](langs/project-search-api/) |
| 速くする | [`minisearch/04-fast/`](minisearch/04-fast/)、[`langs/project-performance/`](langs/project-performance/) |
| 並行化する | [`minisearch/05-concurrent/`](minisearch/05-concurrent/)、[`langs/project-concurrency/`](langs/project-concurrency/) |
| 設計を磨く | [`minisearch/06-architecture/`](minisearch/06-architecture/)、[`langs/project-architecture/`](langs/project-architecture/) |
| 第VII部のふりかえり | [`minisearch/06-architecture/`](minisearch/06-architecture/)、[`review6/`](review6/) |

### 第VIII部 総合演習2：プログラミング言語を作る

| 章 | コード |
|---|---|
| 言語を設計する | [`lam/01-design/`](lam/01-design/)、[`langs/interp-design/`](langs/interp-design/) |
| パーサーを書く | [`lam/02-parser/`](lam/02-parser/)、[`langs/interp-parser/`](langs/interp-parser/) |
| 評価器と REPL | [`lam/03-eval/`](lam/03-eval/)、[`langs/interp-evaluator/`](langs/interp-evaluator/) |
| 型検査器を作る | [`lam/04-types/`](lam/04-types/)、[`langs/interp-typechecker/`](langs/interp-typechecker/) |
| 言語を仕上げる | [`lam/05-polish/`](lam/05-polish/)、[`lam/02-parser/`](lam/02-parser/)、[`langs/interp-polish/`](langs/interp-polish/) |
| 第VIII部のふりかえり | [`lam/05-polish/`](lam/05-polish/)、[`review7/`](review7/) |

### 第IX部 総合演習3：Python の機械学習ライブラリを Haskell で作る

| 章 | コード |
|---|---|
| NumPy 風の配列ライブラリを作る | [`ml/`](ml/)、[`ml/01-numpy/`](ml/01-numpy/)、[`langs/ml-numerics/`](langs/ml-numerics/) |
| scikit-learn 風に回帰と分類を作る | [`ml/02-sklearn/`](ml/02-sklearn/)、[`langs/ml-regression/`](langs/ml-regression/) |
| PyTorch の autograd 風に自動微分を作る | [`ml/03-autograd/`](ml/03-autograd/)、[`langs/ml-autodiff/`](langs/ml-autodiff/) |
| PyTorch 風にニューラルネットワークを組む | [`ml/04-nn/`](ml/04-nn/)、[`langs/ml-neural-network/`](langs/ml-neural-network/) |
| 型で次元を守る | [`ml/05-typed/`](ml/05-typed/)、[`langs/ml-typed-tensors/`](langs/ml-typed-tensors/) |
| 第IX部のふりかえり | [`ml/05-typed/`](ml/05-typed/)、[`review8/`](review8/)、[`langs/part8-review/`](langs/part8-review/) |

### 第X部 総合演習4：型に厳しいインメモリデータベースを作る

| 章 | コード |
|---|---|
| 設計：値とコマンドを型で描く | [`minikv/01-design/`](minikv/01-design/)、[`langs/kv-design/`](langs/kv-design/) |
| プロトコルとパーサー | [`minikv/02-protocol/`](minikv/02-protocol/)、[`langs/kv-protocol/`](langs/kv-protocol/) |
| TCP サーバーにする | [`minikv/03-server/`](minikv/03-server/)、[`langs/kv-server/`](langs/kv-server/) |
| 期限切れと時計 | [`minikv/04-expiry/`](minikv/04-expiry/)、[`langs/kv-expiry/`](langs/kv-expiry/) |
| 永続化：落ちても戻れる | [`minikv/05-persist/`](minikv/05-persist/)、[`langs/kv-persistence/`](langs/kv-persistence/) |
| Pub/Sub とブロッキング | [`minikv/06-pubsub/`](minikv/06-pubsub/)、[`langs/kv-pubsub/`](langs/kv-pubsub/) |
| 型に厳しいクライアント | [`minikv/07-client/`](minikv/07-client/)、[`langs/kv-typed-client/`](langs/kv-typed-client/) |
| 第X部のふりかえり | [`minikv/07-client/`](minikv/07-client/)、[`review9/`](review9/) |

### 第XI部 プログラマーのための数学

| 章 | コード |
|---|---|
| 数の正体：整数・小数・あふれと丸め | [`math/`](math/)、[`langs/math-numbers/`](langs/math-numbers/) |
| 論理と集合：条件を正しく組み立てる | [`math/`](math/)、[`langs/math-logic-sets/`](langs/math-logic-sets/) |
| 帰納法と再帰：正しさを積み上げる | [`math/`](math/)、[`langs/math-induction/`](langs/math-induction/) |
| 指数と対数：増え方の感覚 | [`math/`](math/)、[`langs/math-growth/`](langs/math-growth/) |
| 数え上げと確率：場合の数と期待値 | [`math/`](math/)、[`langs/math-counting/`](langs/math-counting/) |
| 統計の基本：平均にだまされない | [`math/`](math/)、[`langs/math-statistics/`](langs/math-statistics/) |
| 剰余の世界：時計の算数からハッシュと暗号へ | [`math/`](math/)、[`langs/math-modular/`](langs/math-modular/) |
| グラフ：つながりを数える | [`math/`](math/)、[`langs/math-graphs/`](langs/math-graphs/) |
| ベクトルと行列：たくさんの数をまとめて動かす | [`math/`](math/)、[`langs/math-vectors/`](langs/math-vectors/) |
| 微分と勾配：坂道を下って答えを探す | [`math/`](math/)、[`langs/math-calculus/`](langs/math-calculus/) |
| 結合法則とモノイド：分けて計算してよい理由 | [`math/`](math/)、[`langs/math-algebra/`](langs/math-algebra/) |
| 第XI部のふりかえり | [`math/`](math/)、[`review10/`](review10/) |

### 第XII部 いまどきのプログラミングと Haskell

| 章 | コード |
|---|---|
| 言語の地図：Haskell の親戚と、新しい言語たち | [`modern/`](modern/) |
| 代数的データ型とパターンマッチは、どこにでもある | [`modern/`](modern/)、[`langs/modern-adt/`](langs/modern-adt/) |
| エフェクト：副作用に名前を付ける | [`modern/`](modern/)、[`langs/modern-effects/`](langs/modern-effects/) |
| 構造化された並行性：迷子のタスクを出さない | [`modern/`](modern/)、[`langs/modern-structured/`](langs/modern-structured/) |
| CRDT とローカルファースト：ぶつからない共同編集 | [`modern/`](modern/)、[`langs/modern-crdt/`](langs/modern-crdt/) |
| WebAssembly：Haskell をブラウザーで動かす | [`wasm/`](wasm/) |
| CI と再現できる環境：みんなの手元で同じに動く | [`modern/`](modern/)、[`ci/`](ci/) |
| AI と書くプログラミング：型とテストを手綱に | [`modern/`](modern/)、[`langs/modern-ai/`](langs/modern-ai/) |
| 第XII部のふりかえり | [`review12/`](review12/) |
<!-- chapter-index: end -->

## ライセンス

MIT License（`LICENSE` を参照）
