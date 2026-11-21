-- ANCHOR: all
-- | MiniRegex and two regex libraries of Haskell, on the same jobs:
--   pcre2 (the C library of PCRE2 inside, without the JIT) and
--   regex-tdfa (written in Haskell; POSIX, so no \d or \w). Each job
--   counts the matches in the guild's book of sales.
-- MiniRegex と、Haskell の正規表現のライブラリ2つを、同じ仕事で：
-- pcre2（中に PCRE2 の C のライブラリ。JIT なし）と regex-tdfa（Haskell で
-- 書かれている。POSIX なので \d や \w がない）。どの仕事も、ギルドの
-- 売り上げの帳面の中の一致を数える
module Main (main) where

-- ANCHOR: imports
import Control.Exception (SomeException, evaluate, try)
import Data.Array ((!))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as R
import Test.Tasty.Bench
import Text.Regex.Pcre2 qualified as Pcre2
import Text.Regex.TDFA qualified as TDFA
import Text.Regex.TDFA.Text ()

-- ANCHOR: miniimports
import MiniRegex
import MiniRegex.Examples (eachText, fieldOf, guildBook, surely)
-- ANCHOR_END: miniimports
-- ANCHOR_END: imports

-- ANCHOR: jobs
-- 📋 A job: its name, the pattern for MiniRegex and pcre2, and the
--    pattern for regex-tdfa (POSIX classes instead of \d and \w).
-- 仕事：名前、MiniRegex と pcre2 のパターン、regex-tdfa のパターン
-- （\d と \w のかわりに POSIX の文字の集まり）
jobs :: [(String, Text, Text)]
jobs =
  [ ("dates", "\\d{4}-\\d{2}-\\d{2}", "[0-9]{4}-[0-9]{2}-[0-9]{2}")
  , ( "mail", "[\\w.+-]+@[\\w-]+\\.[\\w.-]+"
    , "[[:alnum:]_.+-]+@[[:alnum:]_-]+\\.[[:alnum:]_.-]+" )
  , ( "names", "Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go"
    , "Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go" )
  , ("caseless", "(?i)perl", "[Pp][Ee][Rr][Ll]")
  , ( "sales", "(\\w+) sold (\\d+) (\\w+)"
    , "([[:alnum:]_]+) sold ([0-9]+) ([[:alnum:]_]+)" )
  ]
-- ANCHOR_END: jobs

-- ANCHOR: main
-- ANCHOR: mainfn
-- ⏱️ Twenty thousand lines; each job with the three libraries, as
--    multiples of MiniRegex. Then the name field, with eight names, and
--    (from the chapter on learning from other lands) the texts looked
--    for one by one, in two thousand lines.
-- 2万行。仕事ごとに3つのライブラリを、MiniRegex の何倍かで。それから、
-- 名前が8個の名前の欄と、（よその国に学ぶ章から）文字列を1つずつ探す
-- 形を、2千行で
main :: IO ()
main = do
  let book = guildBook 20000
  _ <- evaluate (T.length book)
  defaultMain (map (job book) jobs ++ [field, oneByOne (guildBook 2000)])
-- ANCHOR_END: mainfn

job :: Text -> (String, Text, Text) -> Benchmark
job book (name, p, posix) = bgroup name
  [ bench "miniregex" (nf (countMini (surely p)) book)
  , vs (bench "pcre2" (nf (countPcre2 p) book))
  , vs (bench "tdfa"  (nf (countTdfa (TDFA.makeRegex posix)) book))
  , vs (bench "miniregex-lines" (nf (byLines (countMini (surely p))) book))
  , vs (bench "pcre2-lines" (nf (byLines (countPcre2 p)) book))
  ]
  where
    vs = bcompare ("$NF == \"miniregex\" && $(NF-1) == \"" ++ name ++ "\"")

-- 📃 The same count, one line at a time (no job looks across lines).
-- 同じ数え方を、1行ずつ（どの仕事も、行をまたいで探さない）
byLines :: (Text -> (Int, Int)) -> Text -> (Int, Int)
byLines count book = (sum (map fst cs), sum (map snd cs))
  where
    cs = map count (T.lines book)
-- ANCHOR_END: main

-- ANCHOR: count
-- 🔢 The count, and the sum of group 2 where there is one: the same
--    two numbers as the other languages print.
-- 件数と、2番のグループがあればその数の合計。ほかの言語が出すのと
-- 同じ2つの数
countMini :: Regex -> Text -> (Int, Int)
countMini re t = (length ms, sum (map second ms))
  where
    ms = findAll re t
    second m = maybe 0 number (group 2 m)

-- (partly applied, pcre2 compiles the pattern once)
-- （部分適用すると、pcre2 はパターンを1度だけコンパイルする）
countPcre2 :: Text -> Text -> (Int, Int)
countPcre2 p
  | "(\\d+)" `T.isInfixOf` p = withGroups
  | otherwise                = \t -> (length (matchAll t), 0)
  where
    matchAll = Pcre2.match p :: Text -> [Text]
    caps     = Pcre2.captures p :: Text -> [NE.NonEmpty Text]
    withGroups t =
      let cs = caps t
      in  (length cs, sum [number (c NE.!! 2) | c <- cs])

countTdfa :: TDFA.Regex -> Text -> (Int, Int)
countTdfa re t = (length ms, sum [second m | m <- ms])
  where
    ms = TDFA.matchAllText re t
    second m
      | length m > 2 = number (fst (m ! 2))
      | otherwise    = 0

number :: Text -> Int
number = either (const 0) fst . R.decimal
-- ANCHOR_END: count

-- ANCHOR: field
-- 🪪 The name field with eight names: pcre2 gives up at its limit of
--    ten million steps, and throws.
-- 名前が8個の名前の欄：pcre2 はステップ数の上限（1000万）で、あきらめて投げる
field :: Benchmark
field = bgroup "field"
  [ bench "miniregex" (nf (isMatch (surely names)) t)
  , bench "pcre2" (nfAppIO (gaveUp . Pcre2.matches names) t)
  , bench "tdfa" (nf (TDFA.matchTest posix) t)
  ]
  where
    t = fieldOf 8
    names = "^(\\w+\\s?)+$"
    posix = TDFA.makeRegex ("^([[:alnum:]_]+[[:space:]]?)+$" :: Text)
              :: TDFA.Regex
    gaveUp :: Bool -> IO (Either String Bool)
    gaveUp b = either (\e -> Left (take 20 (show (e :: SomeException))))
                      Right <$> try (evaluate b)
-- ANCHOR_END: field

-- ANCHOR: onebyone
-- 🐌 The texts of names and (?i)perl: with one run of break for the
--    first characters, and with each text looked for by itself.
-- names と (?i)perl の文字列：最初の文字を break の1回の走査で探すのと、
-- 文字列を1つずつ探すのと
oneByOne :: Text -> Benchmark
oneByOne book = bgroup "one-by-one"
  [ bgroup name
      [ bench "first-chars" (nf (countMini (surely p)) book)
      , bench "each-text"   (nf (countMini (eachText (surely p))) book)
      ]
  | (name, p) <- [ ("names", "Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go")
                 , ("caseless", "(?i)perl") ] ]
-- ANCHOR_END: onebyone
-- ANCHOR_END: all
