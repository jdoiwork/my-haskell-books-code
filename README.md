# モダン Haskell 入門〜中級：サンプルコード

[![ci](https://github.com/jdoiwork/my-haskell-books-code/actions/workflows/ci.yml/badge.svg)](https://github.com/jdoiwork/my-haskell-books-code/actions/workflows/ci.yml)

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
- `review-db-web`、`review-search`、`review-interpreter`、`review-ml`、`review-minikv`、`upsert`

`wasm` は、ghc-wasm-meta の `wasm32-wasi-ghc` で組み立てます（`wasm/build.sh`）。

GitHub Actions（`.github/workflows/ci.yml`）で、`cabal.project` のパッケージを `-Werror` でビルドし、テストを流しています（push のたびと、週に1回）。

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

### 第1部 走り出す

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
| 数の型と変換：Num・Integral・Fractional | [`numeric/`](numeric/)、[`langs/numeric-types/`](langs/numeric-types/) |
| deriving を使いこなす：Enum・Bounded・Read・Num | [`typeclasses/`](typeclasses/) |
| 型クラスを作る：自分だけの資格 | [`typeclasses/`](typeclasses/)、[`langs/custom-type-classes/`](langs/custom-type-classes/) |
| 動くプログラムを書く：IO と do でミニ RPG | [`minirpg/`](minirpg/)、[`minirpg/compare/`](minirpg/compare/)、[`langs/real-programs/`](langs/real-programs/) |
| ファイルと引数：プログラムを外の世界とつなぐ | [`minirpg/`](minirpg/)、[`cabal.project`](cabal.project)、[`book-prelude/`](book-prelude/) |
| 集計する：Map で殿堂の成績表を作る | [`minirpg/`](minirpg/) |
| 競技プログラミングの入出力：読んで、解いて、書く | [`cpintro/`](cpintro/)、[`langs/cp-io/`](langs/cp-io/) |
| 第1部のふりかえり：ベースキャンプで地図を広げる | [`review-first-steps/`](review-first-steps/) |

### 第2部 実戦の道具箱

| 章 | コード |
|---|---|
| cabal プロジェクトと開発環境 | [`guild/`](guild/)、[`cabal.project`](cabal.project) |
| エラー処理 | [`registry/`](registry/)、[`langs/error-handling/`](langs/error-handling/) |
| IO の例外をちゃんと扱う：bracket・finally・非同期例外 | [`treasury/`](treasury/)、[`langs/exceptions/`](langs/exceptions/) |
| デバッグとログ：プログラムの中をのぞく | [`debuglog/`](debuglog/) |
| 関数とドキュメントの探し方：Hoogle・Hackage・GHCi、そして AI | [`finding/`](finding/) |
| よく使うライブラリ | [`libtour/`](libtour/) |
| 日付と時刻：time で、瞬間と暦と長さを型で分ける | [`datetime/`](datetime/)、[`langs/datetime/`](langs/datetime/)、[`langs/cargo/`](langs/cargo/) |
| タイムゾーンと夏時間：土地の時計と、世界で1つの瞬間 | [`timezones/`](timezones/)、[`langs/timezones/`](langs/timezones/)、[`langs/cargo/`](langs/cargo/) |
| JSON を扱う：aeson でシリアライズ・デシリアライズ | [`jsonbook/`](jsonbook/)、[`langs/json/`](langs/json/)、[`langs/cargo/`](langs/cargo/) |
| テスト | [`loot/`](loot/)、[`langs/testing/`](langs/testing/)、[`langs/cargo/`](langs/cargo/) |
| CLI ツールを作る | [`mondex/`](mondex/)、[`langs/cli-tool/`](langs/cli-tool/)、[`langs/cargo/`](langs/cargo/) |
| サブコマンドのある CLI ツール | [`mondex/`](mondex/)、[`langs/cli-subcommands/`](langs/cli-subcommands/)、[`langs/cargo/`](langs/cargo/) |
| Monad 入門 | [`monadintro/`](monadintro/)、[`langs/monad-intro/`](langs/monad-intro/) |
| 第2部のふりかえり | [`review-toolbox/`](review-toolbox/) |

### 第3部 データベースと Web サービス

| 章 | コード |
|---|---|
| データベース入門：SQLite と sqlite-simple で CRUD | [`guilddb/`](guilddb/)、[`langs/database/`](langs/database/)、[`langs/cargo/`](langs/cargo/) |
| データベースを育てる：表の関係、マイグレーション、トランザクション | [`guildquest/`](guildquest/)、[`langs/db-schema/`](langs/db-schema/) |
| 型で SQL を守る：selda と ORM の世界 | [`guildtyped/`](guildtyped/)、[`dbcompare/`](dbcompare/)、[`langs/db-typed/`](langs/db-typed/)、[`langs/cargo/`](langs/cargo/) |
| データベースをテストする | [`guildtest/`](guildtest/)、[`guilddb/`](guilddb/)、[`cabal.project`](cabal.project)、[`langs/db-testing/`](langs/db-testing/) |
| PostgreSQL へ：サーバーの DB とコンテナ | [`guildpg/`](guildpg/)、[`langs/db-postgres/`](langs/db-postgres/) |
| hasql と rel8：PostgreSQL を型で使い倒す | [`guildrel8/`](guildrel8/)、[`langs/db-hasql/`](langs/db-hasql/) |
| DB に縛られない設計：SQLite と PostgreSQL を切り替える | [`guilddesign/`](guilddesign/)、[`langs/db-design/`](langs/db-design/) |
| Web API を作る：servant で、型が API の設計図になる | [`guildapi/`](guildapi/)、[`langs/web-api/`](langs/web-api/) |
| セキュリティ：型で守れるもの、守れないもの | [`guildsecure/`](guildsecure/)、[`langs/security/`](langs/security/) |
| Web アプリを作る：React の画面と Haskell の API を Docker で動かす | [`guildweb/`](guildweb/) |
| 第3部のふりかえり | [`review-db-web/`](review-db-web/) |

### 第4部 速く書く・速く動かす

| 章 | コード |
|---|---|
| 遅延評価とパフォーマンスの基本 | [`laziness/`](laziness/)、[`langs/laziness/`](langs/laziness/) |
| データ構造の選び方 | [`datastructs/`](datastructs/)、[`langs/data-structures/`](langs/data-structures/) |
| 計測とチューニング | [`tuning/`](tuning/)、[`langs/measuring/`](langs/measuring/) |
| 書き換えて速くする：ST・IORef・書き換えられる配列 | [`mutable-perf/`](mutable-perf/)、[`langs/mutable-performance/`](langs/mutable-performance/) |
| 競技プログラミング | [`compete/`](compete/)、[`langs/competitive/`](langs/competitive/) |
| 並行・並列 | [`concurrency/`](concurrency/)、[`langs/concurrency/`](langs/concurrency/) |
| 第4部のふりかえり | [`review-performance/`](review-performance/) |

### 第5部 抽象の力

| 章 | コード |
|---|---|
| Functor・Applicative・Monad をもっと深く | [`deepmonad/`](deepmonad/)、[`langs/functor-applicative-monad/`](langs/functor-applicative-monad/) |
| Reader・Writer・State：よく使う3つのモナド | [`rws/`](rws/)、[`langs/reader-writer-state/`](langs/reader-writer-state/) |
| IO の正体：IO モナドもどきを作る | [`fakeio/`](fakeio/)、[`langs/fake-io/`](langs/fake-io/) |
| Foldable・Traversable と汎用的な部品 | [`foldtrav/`](foldtrav/)、[`langs/foldable-traversable/`](langs/foldable-traversable/) |
| lens：入れ子のデータを自在に操る | [`lensbook/`](lensbook/)、[`langs/lens/`](langs/lens/) |
| モナド変換子とアプリケーション設計 | [`appdesign/`](appdesign/)、[`langs/application-architecture/`](langs/application-architecture/) |
| IO を持ち上げる：liftIO・MonadIO・MonadUnliftIO | [`lifting-io/`](lifting-io/)、[`langs/lifting-io/`](langs/lifting-io/) |
| 書き換えられる箱の使い分け：IORef・MVar・TVar・キュー | [`mutable-toolbox/`](mutable-toolbox/)、[`langs/mutable-toolbox/`](langs/mutable-toolbox/) |
| GHC 拡張 | [`ghcext/`](ghcext/)、[`hsx/`](hsx/)、[`langs/ghc-extensions/`](langs/ghc-extensions/) |
| 小さな型安全 SQL を作る | [`minisql/`](minisql/)、[`langs/typed-sql/`](langs/typed-sql/) |
| 型レベルプログラミング入門 | [`typelevel/`](typelevel/)、[`langs/type-level/`](langs/type-level/) |
| 第5部のふりかえり | [`review-abstraction/`](review-abstraction/) |

### 第6部 アルゴリズムを Haskell らしく

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
| 第6部のふりかえり | [`algo/`](algo/)、[`review-algorithms/`](review-algorithms/) |

### 第7部 総合演習1：ミニ全文検索エンジンを育てる

| 章 | コード |
|---|---|
| 設計：型でドメインを描く | [`minisearch/`](minisearch/)、[`minisearch/design/`](minisearch/design/)、[`langs/project-design/`](langs/project-design/) |
| インデクサーを CLI ツールにする | [`minisearch/cli/`](minisearch/cli/)、[`minisearch/design/`](minisearch/design/)、[`langs/project-indexer-cli/`](langs/project-indexer-cli/) |
| 検索 Web API を公開する | [`minisearch/api/`](minisearch/api/)、[`minisearch/cli/`](minisearch/cli/)、[`langs/project-search-api/`](langs/project-search-api/) |
| 速くする | [`minisearch/fast/`](minisearch/fast/)、[`minisearch/api/`](minisearch/api/)、[`langs/project-performance/`](langs/project-performance/) |
| 並列化する（parallel） | [`minisearch/parallel/`](minisearch/parallel/)、[`minisearch/fast/`](minisearch/fast/)、[`langs/project-concurrency/`](langs/project-concurrency/) |
| 並行化する（concurrent） | [`minisearch/live-text/`](minisearch/live-text/)、[`minisearch/live/`](minisearch/live/)、[`minisearch/parallel/`](minisearch/parallel/)、[`minisearch/live-intmap/`](minisearch/live-intmap/) |
| 設計を磨く | [`minisearch/architecture/`](minisearch/architecture/)、[`minisearch/live/`](minisearch/live/)、[`langs/project-architecture/`](langs/project-architecture/) |
| 第7部のふりかえり | [`minisearch/architecture/`](minisearch/architecture/)、[`review-search/`](review-search/) |

### 第8部 総合演習2：プログラミング言語を作る

| 章 | コード |
|---|---|
| 言語を設計する | [`lam/design/`](lam/design/)、[`langs/interp-design/`](langs/interp-design/) |
| パーサーを書く | [`lam/parser/`](lam/parser/)、[`langs/interp-parser/`](langs/interp-parser/) |
| 評価器と REPL | [`lam/eval/`](lam/eval/)、[`langs/interp-evaluator/`](langs/interp-evaluator/) |
| 型検査器を作る | [`lam/types/`](lam/types/)、[`langs/interp-typechecker/`](langs/interp-typechecker/) |
| 言語を仕上げる | [`lam/polish/`](lam/polish/)、[`lam/parser/`](lam/parser/)、[`langs/interp-polish/`](langs/interp-polish/) |
| 第8部のふりかえり | [`lam/polish/`](lam/polish/)、[`review-interpreter/`](review-interpreter/) |

### 第9部 総合演習3：Python の機械学習ライブラリを Haskell で作る

| 章 | コード |
|---|---|
| NumPy 風の配列ライブラリを作る | [`ml/`](ml/)、[`ml/numpy/`](ml/numpy/)、[`langs/ml-numerics/`](langs/ml-numerics/) |
| scikit-learn 風に回帰と分類を作る | [`ml/sklearn/`](ml/sklearn/)、[`langs/ml-regression/`](langs/ml-regression/) |
| PyTorch の autograd 風に自動微分を作る | [`ml/autograd/`](ml/autograd/)、[`langs/ml-autodiff/`](langs/ml-autodiff/) |
| PyTorch 風にニューラルネットワークを組む | [`ml/nn/`](ml/nn/)、[`ml/`](ml/)、[`langs/ml-neural-network/`](langs/ml-neural-network/) |
| 型で次元を守る | [`ml/typed/`](ml/typed/)、[`langs/ml-typed-tensors/`](langs/ml-typed-tensors/) |
| 速さを比べる：MiniTorch・Haskell のライブラリ・Python・Julia・R | [`ml/bench/`](ml/bench/)、[`ml/nn/`](ml/nn/)、[`ml/hasktorch/`](ml/hasktorch/)、[`langs/ml-benchmarks/`](langs/ml-benchmarks/) |
| 第9部のふりかえり | [`ml/typed/`](ml/typed/)、[`ml/bench/`](ml/bench/)、[`review-ml/`](review-ml/)、[`langs/review-ml/`](langs/review-ml/) |

### 第10部 総合演習4：型に厳しいインメモリデータベースを作る

| 章 | コード |
|---|---|
| 設計：値とコマンドを型で描く | [`minikv/01-design/`](minikv/01-design/)、[`langs/kv-design/`](langs/kv-design/) |
| プロトコルとパーサー | [`minikv/02-protocol/`](minikv/02-protocol/)、[`minikv/01-design/`](minikv/01-design/)、[`langs/kv-protocol/`](langs/kv-protocol/) |
| TCP サーバーにする | [`minikv/03-server/`](minikv/03-server/)、[`minikv/02-protocol/`](minikv/02-protocol/)、[`langs/kv-server/`](langs/kv-server/) |
| 期限切れと時計 | [`minikv/04-expiry/`](minikv/04-expiry/)、[`minikv/04-expiry/answers/pexpire/`](minikv/04-expiry/answers/pexpire/)、[`langs/kv-expiry/`](langs/kv-expiry/) |
| 永続化：落ちても戻れる | [`minikv/05-persist/`](minikv/05-persist/)、[`langs/kv-persistence/`](langs/kv-persistence/) |
| Pub/Sub とブロッキング | [`minikv/06-pubsub/`](minikv/06-pubsub/)、[`minikv/05-persist/`](minikv/05-persist/)、[`langs/kv-pubsub/`](langs/kv-pubsub/) |
| 型に厳しいクライアント | [`minikv/07-client/`](minikv/07-client/)、[`minikv/06-pubsub/`](minikv/06-pubsub/)、[`langs/kv-typed-client/`](langs/kv-typed-client/) |
| 第10部のふりかえり | [`minikv/07-client/`](minikv/07-client/)、[`review-minikv/`](review-minikv/) |

### 第11部 総合演習5：RabbitMQ 風のメッセージブローカーを作る

| 章 | コード |
|---|---|
| 設計：メッセージ・キュー・配り方を型で描く | [`minimq/01-design/`](minimq/01-design/)、[`langs/mq-design/`](langs/mq-design/) |
| 配り方の規則：全員に・宛先どおりに・模様で | [`minimq/02-routing/`](minimq/02-routing/)、[`langs/mq-routing/`](langs/mq-routing/) |
| 手順書きと TCP サーバー | [`minimq/03-server/`](minimq/03-server/)、[`langs/mq-server/`](langs/mq-server/) |
| 受け取りの確認と配り直し | [`minimq/04-ack/`](minimq/04-ack/)、[`langs/mq-ack/`](langs/mq-ack/) |
| 監督役：落ちても立ち直る | [`minimq/05-supervisor/`](minimq/05-supervisor/)、[`langs/mq-supervisor/`](langs/mq-supervisor/) |
| 永続化：届いていないメッセージを失わない | [`minimq/06-persist/`](minimq/06-persist/)、[`langs/mq-persistence/`](langs/mq-persistence/) |
| 型に厳しいクライアント：Erlang くん・Elixir くんと並べて | [`minimq/07-client/`](minimq/07-client/)、[`minimq/06-persist/`](minimq/06-persist/)、[`langs/mq-typed-client/`](langs/mq-typed-client/) |
| 第11部のふりかえり：手紙を一通も落とさない伝令所 | [`minimq/01-design/`](minimq/01-design/)、[`review-minimq/`](review-minimq/) |

### 第12部 総合演習6：読書メモのレビューサイトを作る

| 章 | コード |
|---|---|
| 設計：作品・目次・感想・評価を型で描く | [`tsundoku/design/`](tsundoku/design/)、[`langs/shelf-design/`](langs/shelf-design/) |
| 目次の木と進み具合：どこまで読んだ？ | [`tsundoku/progress/`](tsundoku/progress/)、[`tsundoku/design/`](tsundoku/design/)、[`langs/shelf-progress/`](langs/shelf-progress/) |
| ★を集める：件数の少ない★5にだまされないランキング | [`tsundoku/ratings/`](tsundoku/ratings/)、[`tsundoku/progress/`](tsundoku/progress/)、[`langs/shelf-ratings/`](langs/shelf-ratings/) |
| 窓口と記録：やれることを型にして SQLite にしまう | [`tsundoku/storage/`](tsundoku/storage/)、[`tsundoku/ratings/`](tsundoku/ratings/)、[`langs/shelf-storage/`](langs/shelf-storage/) |
| 自分で決める属性と状態：型の外側を型で守る | [`tsundoku/custom-fields/`](tsundoku/custom-fields/)、[`tsundoku/storage/`](tsundoku/storage/)、[`langs/shelf-custom-fields/`](langs/shelf-custom-fields/) |
| 本を取り込む：EPUB・PDF・ISBN から目次を作る | [`tsundoku/import/`](tsundoku/import/)、[`tsundoku/custom-fields/`](tsundoku/custom-fields/)、[`langs/shelf-import/`](langs/shelf-import/) |
| 中身を探す：SQLite FTS5 で全文検索 | [`tsundoku/search/`](tsundoku/search/)、[`tsundoku/import/`](tsundoku/import/)、[`langs/shelf-search/`](langs/shelf-search/) |
| 画面に依らない API：servant の型を正にする | [`tsundoku/api/`](tsundoku/api/)、[`tsundoku/search/`](tsundoku/search/)、[`langs/shelf-api/`](langs/shelf-api/) |
| 画面を取り替える1：lucid と htmx でサーバーが HTML を返す | [`tsundoku/htmx/`](tsundoku/htmx/)、[`tsundoku/api/`](tsundoku/api/)、[`langs/shelf-htmx/`](langs/shelf-htmx/) |
| 画面を取り替える2：React と brick で同じ API を使う | [`tsundoku/screens/`](tsundoku/screens/)、[`tsundoku/htmx/`](tsundoku/htmx/)、[`langs/shelf-react-brick/`](langs/shelf-react-brick/) |
| 本棚を手で整える：React で本を足し、目次を直す | [`tsundoku/editing/`](tsundoku/editing/)、[`tsundoku/screens/`](tsundoku/screens/)、[`langs/shelf-editing/`](langs/shelf-editing/) |
| AI と一緒に：MCP で目次を一括登録する | [`tsundoku/mcp/`](tsundoku/mcp/)、[`tsundoku/editing/`](tsundoku/editing/)、[`langs/shelf-mcp/`](langs/shelf-mcp/) |
| レビューサイトに仕上げる：本棚・タグ・ネタバレ・読書の統計 | [`tsundoku/review-site/`](tsundoku/review-site/)、[`tsundoku/mcp/`](tsundoku/mcp/)、[`langs/shelf-review-site/`](langs/shelf-review-site/) |
| 画面をおしゃれに：Mantine で本棚の画面を着替える | [`tsundoku/mantine/`](tsundoku/mantine/)、[`tsundoku/review-site/`](tsundoku/review-site/)、[`langs/shelf-mantine/`](langs/shelf-mantine/)、[`langs/cargo/`](langs/cargo/) |
| 配って使ってもらう：docker compose で、コマンド1つで本棚を | [`tsundoku/docker/`](tsundoku/docker/)、[`tsundoku/mantine/`](tsundoku/mantine/) |
| 第12部のふりかえり | [`tsundoku/docker/`](tsundoku/docker/)、[`review-tsundoku/`](review-tsundoku/) |

### 第13部 総合演習7：PCRE2 互換の正規表現エンジンを作る

| 章 | コード |
|---|---|
| 設計：使う側から決めて、パターンを型で描く | [`miniregex/design/`](miniregex/design/)、[`miniregex/dfa/`](miniregex/dfa/)、[`langs/regex-design/`](langs/regex-design/)、[`langs/cargo/`](langs/cargo/) |
| パターンを読む：パーサー | [`miniregex/parser/`](miniregex/parser/)、[`miniregex/design/`](miniregex/design/)、[`langs/regex-parser/`](langs/regex-parser/)、[`langs/cargo/`](langs/cargo/) |
| 最初のマッチャー：バックトラックで全部試す | [`miniregex/backtrack/`](miniregex/backtrack/)、[`miniregex/parser/`](miniregex/parser/)、[`langs/regex-backtrack/`](langs/regex-backtrack/)、[`langs/cargo/`](langs/cargo/) |
| グループとキャプチャ：使いやすい顔を仕上げる | [`miniregex/captures/`](miniregex/captures/)、[`miniregex/backtrack/`](miniregex/backtrack/)、[`langs/regex-captures/`](langs/regex-captures/)、[`langs/cargo/`](langs/cargo/) |
| 文字クラスと Unicode：フラグと大文字・小文字 | [`miniregex/unicode/`](miniregex/unicode/)、[`miniregex/captures/`](miniregex/captures/)、[`langs/regex-unicode/`](langs/regex-unicode/)、[`langs/cargo/`](langs/cargo/) |
| まわりを見る：先読み・後読み・アトミックグループ | [`miniregex/lookaround/`](miniregex/lookaround/)、[`miniregex/unicode/`](miniregex/unicode/)、[`langs/regex-lookaround/`](langs/regex-lookaround/)、[`langs/cargo/`](langs/cargo/) |
| 命令の列にコンパイルする：バックトラックの VM | [`miniregex/vm/`](miniregex/vm/)、[`miniregex/lookaround/`](miniregex/lookaround/)、[`langs/regex-vm/`](langs/regex-vm/) |
| 書き換えて速くする：ST で VM を書き直す | [`miniregex/vm-st/`](miniregex/vm-st/)、[`miniregex/vm/`](miniregex/vm/)、[`langs/cargo/`](langs/cargo/) |
| 自分を呼ぶ：再帰とサブルーチン | [`miniregex/recursion/`](miniregex/recursion/)、[`miniregex/vm-st/`](miniregex/vm-st/)、[`langs/regex-recursion/`](langs/regex-recursion/) |
| 止まらないパターンと戦う：線形時間の Pike VM | [`miniregex/linear/`](miniregex/linear/)、[`miniregex/recursion/`](miniregex/recursion/)、[`langs/regex-linear/`](langs/regex-linear/)、[`langs/cargo/`](langs/cargo/) |
| 先に探す：探す場所を絞る | [`miniregex/prefilter/`](miniregex/prefilter/)、[`miniregex/linear/`](miniregex/linear/)、[`miniregex/dfa/`](miniregex/dfa/)、[`langs/regex-prefilter/`](langs/regex-prefilter/) |
| しおりと命令を磨く：バイトの場所と1文字のくり返し | [`miniregex/cursor/`](miniregex/cursor/)、[`miniregex/prefilter/`](miniregex/prefilter/)、[`langs/regex-cursor/`](langs/regex-cursor/) |
| エンジンを乗り換える：バックトラックと Pike VM | [`miniregex/engines/`](miniregex/engines/)、[`miniregex/cursor/`](miniregex/cursor/)、[`langs/cargo/`](langs/cargo/) |
| 測って比べる：pcre2・Rust・Python とベンチマーク | [`miniregex/benchmarks/`](miniregex/benchmarks/)、[`miniregex/engines/`](miniregex/engines/)、[`langs/regex-benchmarks/`](langs/regex-benchmarks/)、[`langs/cargo/`](langs/cargo/) |
| よその国に学ぶ：Rust の regex は、なぜ速い？ | [`miniregex/lessons/`](miniregex/lessons/)、[`miniregex/benchmarks/`](miniregex/benchmarks/)、[`langs/cargo/`](langs/cargo/)、[`langs/regex-lessons/`](langs/regex-lessons/) |
| その場で作る DFA：lazy DFA で範囲を先に | [`miniregex/dfa/`](miniregex/dfa/)、[`miniregex/lessons/`](miniregex/lessons/)、[`langs/cargo/`](langs/cargo/) |
| Haskell の仲間に入る：regex-base と `=~` | [`miniregex/dfa/`](miniregex/dfa/)、[`miniregex/base/`](miniregex/base/)、[`langs/regex-base/`](langs/regex-base/) |
| 絵文字を探す：`\p{Emoji}` と `\X` | [`miniregex/emoji/`](miniregex/emoji/)、[`miniregex/base/`](miniregex/base/)、[`langs/regex-emoji/`](langs/regex-emoji/)、[`langs/cargo/`](langs/cargo/) |
| 道具に仕上げる：grep・準クォート・型付きのグループ | [`miniregex/polish/`](miniregex/polish/)、[`miniregex/emoji/`](miniregex/emoji/)、[`langs/regex-polish/`](langs/regex-polish/)、[`langs/cargo/`](langs/cargo/) |
| 第13部のふりかえり | [`miniregex/polish/`](miniregex/polish/)、[`review-miniregex/`](review-miniregex/) |

### 第14部 Lam を育てる：言語の機能

| 章 | コード |
|---|---|
| 文字列・タプル・リスト | [`lam/strings/`](lam/strings/)、[`lam/polish/`](lam/polish/)、[`langs/interp-strings/`](langs/interp-strings/) |
| 型を自分で定義する：data と case | [`lam/data/`](lam/data/)、[`lam/strings/`](lam/strings/)、[`langs/interp-data/`](langs/interp-data/) |
| 書きやすくする：パターンと糖衣構文 | [`lam/sugar/`](lam/sugar/)、[`lam/data/`](lam/data/)、[`langs/interp-sugar/`](langs/interp-sugar/) |
| 型を書く：型の注釈とレコード | [`lam/records/`](lam/records/)、[`lam/sugar/`](lam/sugar/)、[`langs/interp-records/`](langs/interp-records/) |
| 小数を足す：Elm 風の number | [`lam/records/`](lam/records/)、[`lam/float/`](lam/float/)、[`langs/interp-float/`](langs/interp-float/) |
| 型クラスを作る：class と instance | [`lam/classes/`](lam/classes/)、[`lam/float/`](lam/float/)、[`langs/interp-classes/`](langs/interp-classes/) |
| Map と JSON | [`lam/json/`](lam/json/)、[`lam/classes/`](lam/classes/)、[`langs/interp-map-json/`](langs/interp-map-json/) |
| 道具箱をそろえる：文字列・リスト・Map の標準ライブラリ | [`lam/stdlib/`](lam/stdlib/)、[`lam/json/`](lam/json/)、[`langs/interp-stdlib/`](langs/interp-stdlib/) |
| 文字列に値を埋めこむ：f-string 風の書式 | [`lam/format/`](lam/format/)、[`lam/stdlib/`](lam/stdlib/)、[`langs/interp-format/`](langs/interp-format/) |
| ファイルを分ける：モジュール | [`lam/modules/`](lam/modules/)、[`lam/format/`](lam/format/)、[`langs/interp-modules/`](langs/interp-modules/) |
| 第14部のふりかえり | [`lam/modules/`](lam/modules/)、[`review-lam-language/`](review-lam-language/) |

### 第15部 Lam でスクリプトを書く

| 章 | コード |
|---|---|
| 演算子を作る：infixl と as | [`lam/operators/`](lam/operators/)、[`lam/modules/`](lam/modules/)、[`langs/interp-operators/`](langs/interp-operators/) |
| 入出力：IO 型と do | [`lam/io/`](lam/io/)、[`lam/operators/`](lam/operators/)、[`langs/interp-io/`](langs/interp-io/) |
| 失敗を早めに返す：`?` と後始末 | [`lam/errors/`](lam/errors/)、[`lam/io/`](lam/io/)、[`langs/interp-errors/`](langs/interp-errors/) |
| スクリプトとして動かす | [`lam/script/`](lam/script/)、[`lam/errors/`](lam/errors/)、[`langs/interp-script/`](langs/interp-script/) |
| ファイルとコマンドと時計：スクリプトの道具 | [`lam/files/`](lam/files/)、[`lam/script/`](lam/script/)、[`langs/interp-files/`](langs/interp-files/) |
| 大きいファイルとバイト列：1行ずつ読む・ハッシュ・base64 | [`lam/streams/`](lam/streams/)、[`lam/files/`](lam/files/)、[`langs/interp-streams/`](langs/interp-streams/) |
| 文字列を探す・置き換える：正規表現 | [`lam/regex/`](lam/regex/)、[`lam/streams/`](lam/streams/)、[`lam/regex-mini/`](lam/regex-mini/)、[`lam/parsing/`](lam/parsing/)、[`miniregex/polish/`](miniregex/polish/)、[`langs/interp-regex/`](langs/interp-regex/)、[`langs/cargo/`](langs/cargo/) |
| 文字列を読み解く：パーサーを組み合わせる | [`lam/parsing/`](lam/parsing/)、[`lam/regex/`](lam/regex/)、[`langs/interp-parsing/`](langs/interp-parsing/)、[`langs/cargo/`](langs/cargo/) |
| データを読む：JSON の Decoder・CSV・TOML | [`lam/decode/`](lam/decode/)、[`lam/parsing/`](lam/parsing/)、[`langs/interp-decode/`](langs/interp-decode/)、[`langs/cargo/`](langs/cargo/) |
| 外の世界とつなぐ：HTTP と SQLite | [`lam/outside/`](lam/outside/)、[`lam/decode/`](lam/decode/)、[`langs/interp-outside/`](langs/interp-outside/) |
| 鍵をかける：権限のフラグと見張り | [`lam/guard/`](lam/guard/)、[`lam/outside/`](lam/outside/)、[`langs/interp-guard/`](langs/interp-guard/) |
| CLI を手軽に作る：型から作る Typer 風のオプション | [`lam/cli/`](lam/cli/)、[`lam/guard/`](lam/guard/)、[`langs/interp-cli/`](langs/interp-cli/)、[`langs/cargo/`](langs/cargo/) |
| シェルのように：`$"…"` のコマンドと端末の見た目 | [`lam/shell/`](lam/shell/)、[`lam/cli/`](lam/cli/)、[`langs/interp-shell/`](langs/interp-shell/)、[`langs/cargo/`](langs/cargo/) |
| ワンライナーで動かす：Perl のように | [`lam/oneliner/`](lam/oneliner/)、[`lam/shell/`](lam/shell/)、[`langs/interp-oneliner/`](langs/interp-oneliner/) |
| 第15部のふりかえり | [`lam/oneliner/`](lam/oneliner/)、[`review-lam-scripting/`](review-lam-scripting/) |

### 第16部 Lam を仕上げる

| 章 | コード |
|---|---|
| 並行に動かす：まとめて待つ | [`lam/concurrency/`](lam/concurrency/)、[`lam/oneliner/`](lam/oneliner/)、[`langs/interp-concurrency/`](langs/interp-concurrency/) |
| 仕事どうしで話す：チャネルとメッセージ | [`lam/channels/`](lam/channels/)、[`lam/concurrency/`](lam/concurrency/)、[`langs/interp-channels/`](langs/interp-channels/) |
| テストとデバッグ：lam test | [`lam/testing/`](lam/testing/)、[`lam/channels/`](lam/channels/)、[`langs/interp-testing/`](langs/interp-testing/)、[`langs/cargo/`](langs/cargo/) |

### 第17部 プログラマーのための数学

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
| 第17部のふりかえり | [`math/`](math/)、[`review-math/`](review-math/) |

### 第18部 いまどきのプログラミングと Haskell

| 章 | コード |
|---|---|
| 言語の地図：Haskell の親戚と、新しい言語たち | [`modern/`](modern/)、[`langs/modern-language-map/`](langs/modern-language-map/) |
| 代数的データ型とパターンマッチは、どこにでもある | [`modern/`](modern/)、[`langs/modern-adt/`](langs/modern-adt/) |
| エフェクト：副作用に名前を付ける | [`modern/`](modern/)、[`langs/modern-effects/`](langs/modern-effects/) |
| 構造化された並行性：迷子のタスクを出さない | [`modern/`](modern/)、[`langs/modern-structured/`](langs/modern-structured/) |
| CRDT とローカルファースト：ぶつからない共同編集 | [`modern/`](modern/)、[`langs/modern-crdt/`](langs/modern-crdt/) |
| WebAssembly：Haskell をブラウザーで動かす | [`wasm/`](wasm/)、[`modern/`](modern/) |
| CI と再現できる環境：みんなの手元で同じに動く | [`modern/`](modern/)、[`ci/`](ci/) |
| AI と書くプログラミング：型とテストを手綱に | [`modern/`](modern/)、[`langs/modern-ai/`](langs/modern-ai/) |
| MCP サーバーを育てる：チャットから依頼板を動かす | [`modern/`](modern/)、[`langs/modern-mcp/`](langs/modern-mcp/) |
| 第18部のふりかえり | [`review-modern/`](review-modern/) |
<!-- chapter-index: end -->

## ライセンス

MIT License（`LICENSE` を参照）
