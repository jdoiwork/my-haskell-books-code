-- This file defines the module "Main". Only "main" is visible from outside.
-- このファイルは Main モジュール。外から見えるのは main だけ
module Main (main) where

-- Character tests and sorting
-- 文字の判定と並べ替え
import Data.Char (isAlpha)
import Data.List (sortOn)
import Data.Ord  (Down (..))

-- Dictionaries (Map).
-- "qualified as Map" means we use a prefix, like Map.toList.
-- 辞書（Map）。「qualified as Map」は、
-- Map.toList のように接頭辞を付けて使うという意味
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- Efficient text (Text) and its input and output
-- 効率のよい文字列（Text）とその入出力
import Data.Text    (Text)
import Data.Text    qualified as T
import Data.Text.IO qualified as TIO

-- 🚀 Read English text from standard input,
--    and show the 10 most frequent words.
-- 標準入力から英文を読み、よく出てくる単語のトップ10を表示する
-- "main :: IO ()" means main is an action that does input and output.
-- 「main :: IO ()」は、main が入出力をする処理であることを表す
main :: IO ()
main = do
  -- Read all of standard input and name it "input".
  -- 標準入力をすべて読み込み、input という名前を付ける
  input <- TIO.getContents

  -- Give names to the intermediate results.
  -- 途中の計算結果に名前を付ける
  let wordList = splitIntoWords input -- the list of words / 単語のリスト
      counts   = countWords wordList  -- word -> count / 単語 → 出現回数
      ranking  = topN 10 counts       -- the top 10 / 上位10件

  -- Print each entry of the ranking, one per line.
  -- ランキングの各要素を1行ずつ表示する
  mapM_ printEntry ranking

-- ✂️ Split text into lowercase words.
--    Anything that is not a letter separates words.
-- テキストを小文字の単語に分ける。英字以外の文字は単語の区切りとみなす
-- Type: takes a Text and returns a list of Text.
-- 型：Text を受け取り、Text のリストを返す
splitIntoWords :: Text -> [Text]
splitIntoWords text =
  -- Replace every non-letter with a space: "Hello, world!" -> "Hello  world "
  -- 英字以外をすべて空白にする："Hello, world!" → "Hello  world "
  let lettersOnly = T.map replaceNonLetter text
      -- Make everything lowercase: "Hello  world " -> "hello  world "
      -- すべて小文字にする："Hello  world " → "hello  world "
      lowered     = T.toLower lettersOnly
  -- Split by spaces: "hello  world " -> ["hello", "world"]
  -- 空白で区切る："hello  world " → ["hello", "world"]
  in  T.words lowered

-- 🔤 Replace a non-letter character with a space.
-- 英字でない文字を空白に置き換える
replaceNonLetter :: Char -> Char
replaceNonLetter c
  | isAlpha c = c   -- a letter stays as it is / 英字ならそのまま
  | otherwise = ' ' -- anything else becomes a space / それ以外は空白

-- 🔢 Count how many times each word appears.
-- 単語ごとの出現回数を数える
countWords :: [Text] -> Map Text Int
countWords wordList =
  -- Pair every word with 1: ["a", "b", "a"] -> [("a", 1), ("b", 1), ("a", 1)]
  -- 各単語を 1 と組にする：["a", "b", "a"] → [("a", 1), ("b", 1), ("a", 1)]
  let pairs = [ (w, 1) | w <- wordList ]
  -- Build a Map, adding up (+) the numbers for the same word:
  --   {"a": 2, "b": 1}
  -- 同じ単語の数を足し合わせ（+）ながら Map を作る：{"a": 2, "b": 1}
  in  Map.fromListWith (+) pairs

-- 🏆 Take the n most frequent words.
-- 出現回数の多い順に n 個取り出す
topN :: Int -> Map Text Int -> [(Text, Int)]
topN n counts =
  -- Turn the Map into a list of (word, count) pairs.
  -- Map を（単語, 回数）の組のリストに変える
  let pairs  = Map.toList counts
      -- Sort the pairs by count, largest first.
      -- 回数の多い順に並べ替える
      sorted = sortOn countDescending pairs
  -- Keep only the first n pairs.
  -- 先頭の n 個だけを残す
  in  take n sorted
  where
    -- The sort key: the count, in reverse order.
    -- 並べ替えのキー：回数を逆順にしたもの
    countDescending (_, count) = Down count

-- 🖨️ Print a word (padded to 12 characters) followed by its count.
-- 単語（12文字幅に左寄せ）と出現回数を表示する
printEntry :: (Text, Int) -> IO ()
printEntry (word, count) =
  -- "the" -> "the         ", then append the count as text: "the         6"
  -- "the" を12文字幅にし、回数を文字列にしてつなげる："the         6"
  TIO.putStrLn (T.justifyLeft 12 ' ' word <> T.pack (show count))
