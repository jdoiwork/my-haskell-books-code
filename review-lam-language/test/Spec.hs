{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- 🧪 The report card of the workshop, on a made-up board: the lines of
--    Board.lam are made by the test, and the other Lam files are the
--    real ones in report/.
-- 工房の通信簿を、作りものの掲示板で：Board.lam の行はテストが作り、
-- ほかの Lam のファイルは report/ の本物
import Data.Functor.Identity (runIdentity)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import System.Directory (listDirectory)
import Test.Hspec
import Test.QuickCheck

import Lam.Module (Files (..), loadProgram)
import Lam.Prelude (preludeSession)
-- ANCHOR_END: head

-- ANCHOR: run
-- 📂 The Lam files of report/, with Board.lam put in from outside.
-- report/ の Lam のファイル。Board.lam は外から差し替える
reportFiles :: IO (Map.Map FilePath Text)
reportFiles = do
  here <- listDirectory "report"
  Map.fromList <$> sequence
    [ (\t -> (path, t)) <$> Utf8.readFile path
    | n <- here, ".lam" `T.isSuffixOf` T.pack n
    , let path = "report/" <> n ]

-- 🏃 Run a program that sits next to the files, as lam would show its
--    last expression ("... : type").
-- ファイルの隣に置いたプログラムを動かす。lam と同じく、最後の式を
-- 「… : 型」で見せる
runWith :: Map.Map FilePath Text -> Text -> Text
runWith files source =
  case runIdentity (loadProgram (Files (pure . (`Map.lookup` files)) [])
                      preludeSession preludeSession "report/probe.lam"
                      source) of
    Right (_, Just out) -> out
    Right (_, Nothing)  -> ""
    Left err            -> "error: " <> err

-- 📌 A board of our own: the lines are written into Board.lam.
-- 自分で作った掲示板。行を Board.lam に書きこむ
withBoard :: [Text] -> Map.Map FilePath Text -> Map.Map FilePath Text
withBoard lines' = Map.insert "report/Board.lam" board
  where
    board = T.unlines $
      [ "module Board exposing (tasks, log)"
      , "tasks = " <> lamList (map lamString tasks)
      , "log = " <> lamList (map lamString lines') ]
    lamList xs = "[" <> T.intercalate ", " xs <> "]"
    lamString s = "\"" <> T.concatMap escape s <> "\""
    escape '"'  = "\\\""
    escape '\\' = "\\\\"
    escape c    = T.singleton c
-- ANCHOR_END: run

-- ANCHOR: gen
-- 🎲 What one line of the board says.
-- 掲示板の1行が言うこと
data Line
  = Handed Text Text Int   -- ^ who, task, score (maybe out of 0..100)
  | NoScore Text Text      -- ^ the score is missing
  | NotJson Text Text Int  -- ^ written as "who, task, score"
  deriving Show

names, tasks :: [Text]
names = ["Python", "TS", "Rust", "Ruby", "Erlang"]
tasks = ["strings", "data", "float", "classes", "modules"]

instance Arbitrary Line where
  arbitrary = frequency
    [ (8, Handed <$> elements names <*> elements tasks
                 <*> chooseInt (-10, 130))
    , (1, NoScore <$> elements names <*> elements tasks)
    , (1, NotJson <$> elements names <*> elements tasks
                  <*> chooseInt (0, 100)) ]

-- 🖨️ A Line as the board would show it.
-- 掲示板に書くとおりの1行
render :: Line -> Text
render l = case l of
  Handed w t s -> object [("who", str w), ("task", str t), ("score", num s)]
  NoScore w t  -> object [("who", str w), ("task", str t)]
  NotJson w t s -> T.intercalate ", " [w, t, num s]
  where
    object kvs =
      "{" <> T.intercalate ", " [str k <> ": " <> v | (k, v) <- kvs] <> "}"
    str s = "\"" <> s <> "\""
    num = T.pack . show
-- ANCHOR_END: gen

-- ANCHOR: oracle
-- 🔮 The card again, counted in Haskell in another way: only the lines
--    with a score from 0 to 100 count. Who, the sorted scores, the
--    mark, and the tasks not handed in.
-- 通信簿を、Haskell で別のやり方で数え直す：0 から 100 の点数がある行だけ
-- 数える。名前、並べた点数、評価、出していない課題
oracle :: [Line] -> Text
oracle ls = lamShow [ (w, sort ss, markOf ss, missing w) | (w, ss) <- byWho ]
  where
    good = [ (w, t, s) | Handed w t s <- ls, 0 <= s, s <= 100 ]
    byWho = Map.toList (Map.fromListWith (flip (++))
              [ (w, [s]) | (w, _, s) <- good ])
    missing w = [ t | t <- sort tasks
                , t `notElem` [ t' | (w', t', _) <- good, w' == w ] ]
    markOf ss
      | avg >= 90 = "Excellent" :: Text
      | avg >= 75 = "Good"
      | avg >= 60 = "Pass"
      | otherwise = "Retry"
      where avg = fromIntegral (sum ss) / fromIntegral (length ss) :: Double
    lamShow rows = "[" <> T.intercalate ", " (map one rows) <> "]"
    one (w, ss, m, ms) =
      "(\"" <> w <> "\", " <> list (map (T.pack . show) ss) <> ", Just "
        <> m <> ", " <> list ["\"" <> t <> "\"" | t <- ms] <> ")"
    list xs = "[" <> T.intercalate ", " xs <> "]"

-- 🔍 The same four things, from the Lam program.
-- 同じ4つを、Lam のプログラムから
probe :: Text
probe = T.unlines
  [ "import Board"
  , "import Card (read, rows)"
  , "import Grade"
  , "card = rows Board.tasks (fst (read Board.log))"
  , "map (\\r -> (r.who, sort (map Grade.points r.scores), r.mark,"
    <> " Set.toList r.missing)) card" ]

probeType :: Text
probeType = "[(String, [Int], Maybe Mark, [String])]"
-- ANCHOR_END: oracle

-- ANCHOR: shuffle
-- 🔀 Shuffle the lines of the board: the average, the spread, the mark
--    and the tasks not handed in stay the same (the order of the
--    scores in a row may change, so they are not looked at).
-- 掲示板の行を混ぜる：平均・標準偏差・評価・出していない課題は同じまま
-- （1行の中の点数の順は変わりうるので、見ない）
sameInAnyOrder :: Map.Map FilePath Text -> Property
sameInAnyOrder files =
  forAll arbitrary $ \ls ->
  forAll (shuffle ls) $ \mixed ->
    numbers ls === numbers mixed
  where
    numbers ls = runWith (withBoard (map render ls) files) (T.unlines
      [ "import Board"
      , "import Card (read, rows)"
      , "card = rows Board.tasks (fst (read Board.log))"
      , "map (\\r -> (r.who, r.average, r.spread, r.mark, r.missing))"
        <> " card" ])

-- ANCHOR_END: shuffle

-- ANCHOR: draftSpread
-- 🐍 A board with only the five tasks of Python, with these scores.
-- Python くんの5つの課題だけの掲示板。点数はこのとおり
pythonBoard :: [Int] -> Map.Map FilePath Text -> Map.Map FilePath Text
pythonBoard scores =
  withBoard [render (Handed "Python" t s) | (t, s) <- zip tasks scores]

-- 📐 The spread of each row.
-- 1行ずつの標準偏差
spreadOf :: Map.Map FilePath Text -> Text
spreadOf fs = runWith fs (T.unlines
  [ "import Board"
  , "import Card (read, rows)"
  , "rows Board.tasks (fst (read Board.log)) |> map .spread" ])

-- 📝 The first draft of spread: the differences from the mean, squared
--    and added up in Float, in the order of the scores.
-- spread の最初の下書き：平均との差を2乗して、点数の順に Float で足す
draft :: Map.Map FilePath Text -> Map.Map FilePath Text
draft = Map.adjust (T.replace newSpread draftSpread) "report/Card.lam"
  where
    newSpread = T.unlines
      [ "spread xs ="
      , "  map sqrt (toFloat (n * squares - total * total)"
          <> " /? toFloat (n * n))"
      , "  where"
      , "    n = length xs"
      , "    total = sum xs"
      , "    squares = sum (map (\\x -> x * x) xs)" ]
    draftSpread = T.unlines
      [ "spread xs ="
      , "  case mean xs of"
      , "    | Nothing -> Nothing"
      , "    | Just m ->"
      , "        let d x = (toFloat x - m) * (toFloat x - m) in"
      , "        Just (sqrt (sum (map d xs) / toFloat (length xs)))" ]
-- ANCHOR_END: draftSpread

main :: IO ()
main = do
  files <- reportFiles
  hspec $ do
    -- ANCHOR: examples
    -- 「掲示板の通信簿」
    describe "the report card of the board" $ do
      -- 「だめな3行を、行の番号とわけと一緒に言う」
      it "tells the three wrong lines, with their numbers and why" $
        runWith files (T.unlines
          [ "import Board", "import Card (read)"
          , "snd (read Board.log)" ])
          `shouldBe` "[\"line 14: no score\", \
                     \\"line 17: 120 is not a score from 0 to 100\", \
                     \\"line 24: not JSON\"] : [String]"
      -- 「Score は score を通らないと作れない」
      it "makes a Score only through score" $
        runWith files "import Grade\nGrade.Score 120"
          `shouldBe` "error: type error: Grade.Score is hidden by the \
                     \module Grade (it is not in its exposing list)"
      -- 「よい行が1つもない人は、通信簿に載らない」
      it "leaves out one with no good line" $
        runWith (withBoard ["Ruby, float, 77"] files) (T.unlines
          [ "import Board", "import Card (read, rows)"
          , "rows Board.tasks (fst (read Board.log)) |> map .mark" ])
          `shouldBe` "[] : [Maybe Mark]"
    -- ANCHOR_END: examples
    -- ANCHOR: props
    -- 「どんな掲示板でも」
    describe "any board" $ do
      -- 「Haskell で数え直したものと同じ通信簿になる」
      it "makes the same card as counted again in Haskell" $
        property $ \ls ->
          runWith (withBoard (map render ls) files) probe
            `shouldBe` (oracle ls <> " : " <> probeType)
    -- ANCHOR_END: props
    -- ANCHOR: draft
    -- 「Float だけで数える下書きの標準偏差」
    describe "the draft spread, counted with Float only" $ do
      -- 「点数の高い順に貼り直すと、答えが変わってしまう」
      it "changes when the board is sorted by the scores" $ do
        spreadOf (pythonBoard [82, 75, 90, 88, 79] (draft files))
          `shouldBe` "[Just 5.564171097297422] : [Maybe Float]"
        spreadOf (pythonBoard [90, 88, 82, 79, 75] (draft files))
          `shouldBe` "[Just 5.5641710972974225] : [Maybe Float]"
      -- 「本物の spread は、どちらの順でも同じ」
      it "does not change in the real Card.lam" $ do
        spreadOf (pythonBoard [82, 75, 90, 88, 79] files)
          `shouldBe` spreadOf (pythonBoard [90, 88, 82, 79, 75] files)
    -- ANCHOR_END: draft
    -- ANCHOR: answer
    -- 「ステップ4の解答」
    describe "the answer of step 4" $
      -- 「行の順を入れ替えても、通信簿の数は変わらない」
      it "keeps the numbers of the card in any order of the lines" $
        sameInAnyOrder files
    -- ANCHOR_END: answer
-- ANCHOR_END: all
