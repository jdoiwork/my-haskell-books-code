# hsx（試作）

「GHC 拡張」の章に載せるかどうかを決めるための試作です。まだ本文には載せていません。

JSX のように、Haskell のコードの中に HTML を書けます。

```haskell
greeting :: Text -> Html
greeting name = [hsx|<p class="hello">Hello, {name}!</p>|]
```

## できること

- タグ、属性（`class="hero"`・`class={cls}`・値のない `checked`）、空要素（`<br>`、`<img/>`）
- `{name}` で値を埋め込む。`{show level}` のように、空白で区切った名前は関数の呼び出しになる
- 埋め込めるのは `Html`・`Text`・`String`・`Int`、そのリストと `Maybe`（型クラス `ToHtml`）
- 埋め込んだ文字と属性の値は、必ずエスケープする（`<script>` は `&lt;script&gt;`）。エスケープしないのは `raw` だけ
- 空白は JSX と同じ決まりで削る（改行のある文字は行ごとに削り、空の行は消える）
- **まちがいはコンパイルエラーになる**：閉じタグのまちがい・閉じ忘れは、ファイルの行と桁付きで（`broken/WrongClose.hs`）、`{nmae}` の打ちまちがいは GHC のいつものエラーで（`broken/Typo.hs`）

## できないこと（わざと小さくした）

- `{...}` の中に書けるのは、名前と、名前を並べた関数の呼び出しだけ。`{level + 1}` や `{"<"}` のような式は書けない（ちゃんとやるなら haskell-src-meta などで Haskell の式を読む）。`let` で名前を付けてから埋め込む
- `{map f xs}` は書けるが、`{for ...}` のような繰り返しの書き方はない（リストを埋め込めば並ぶ）
- コメント `<!-- -->` と `<!doctype>` は読めない（`raw "<!doctype html>"` を前に足す）
- 文字の中の `<` は書けない（タグの始まりと読む）

## ファイル

| ファイル | 行数 | 中身 |
| --- | --- | --- |
| `src/Hsx/Html.hs` | 約100 | `Html` 型（コンストラクターは隠す）、`escapeText`、`text`・`raw`、型クラス `ToHtml`・`ToAttr` |
| `src/Hsx/Parse.hs` | 約350 | テンプレートを木（`Node`）に読む手書きのパーサー（行と桁を数える）。Functor・Applicative・Monad の資格を自分で付けた小さな `Parser` |
| `src/Hsx/QQ.hs` | 約110 | 準クォート `hsx`。木を「決まった文字」と「穴」の列にして、`mconcat [raw "...", toHtml x, ...]` という式を作る（Template Haskell） |
| `app/GuildPage.hs` | | 例：ギルドの名簿のページ（`cabal run guild-page`） |
| `test/Spec.hs` | | 11個のテスト（パーサー、エスケープ、QuickCheck） |

## 本に載せるとしたら

- GHC 拡張の章で、`QuasiQuotes` と `TemplateHaskell` の例として、`QQ.hs` を中心に載せる（パーサーは「こういうものがある」と短く見せるだけでもよい）
- パーサーの書き方は第VII部（パーサーを書く）と重なるので、そちらに回してもよい
- セキュリティの章の「エスケープは型で守る」（`Html` のコンストラクターを隠す）につなげられる
