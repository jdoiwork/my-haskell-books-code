-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Data.Aeson as A
import Data.Char (isAlpha, toLower)
import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Scientific (scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Control.Exception as E
import System.Timeout (timeout)

import Data.Either (isLeft, isRight)

import Lam.Module
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.State.Strict (modify, runState)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropExtension, replaceExtension, splitSearchPath,
                        takeDirectory, takeExtension, takeFileName, (</>))
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Random (mkStdGen)
import Lam.Format (groupDigits, padLeft, padRight, center, zeroPad)
import Lam.Builtins (Builtin (..), preludeConstructors)
import Lam.Stdlib (splitOn)
import Lam.Json (fromAeson, toAeson)
import Lam.Parser
import Lam.Prelude
import Lam.Pretty
import Lam.Program
import Lam.Repl
import Lam.Syntax
import Lam.Types
import Lam.World
import Lam.Eval (Request (..), Value (..), apply, renderValue)
import Lam.Answers (gunzipBuiltin)
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import Lam.Utf8 (Decoding (..), decodeWith, hexOf)
import qualified Crypto.Hash.SHA256 as Sha
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Directory (getTemporaryDirectory, removeFile)
-- ANCHOR_END: head

-- ANCHOR: run
-- 🔁 Type the lines into a REPL with the prelude, one by one.
-- プレリュード入りの REPL に、1行ずつ打ちこむ
runLines :: [Text] -> [Text]
runLines inputs = snd (foldl go (preludeSession, []) inputs)
  where
    go (s, outs) line =
      let (s', out) = replStep s line in (s', outs <> [out])

-- 🔁 One turn of the REPL, in an empty fake world: what it shows.
-- REPL の1回分を、空の偽物の世界で。見せるもの
replStep :: Session -> Text -> (Session, Text)
replStep s line = fst (runFake emptyWorld (step fakeWorld s line))

-- 🧮 The expression at the end of a loaded program, done in an empty
--    fake world: what it shows.
-- 読みこんだプログラムの最後の式を、空の偽物の世界でやる。見せるもの
finalText :: (Session, Maybe Expr) -> Either Text (Session, Maybe Text)
finalText (s, final) = case final of
  Nothing -> Right (s, Nothing)
  Just e  -> (\t -> (s, Just t))
               <$> shownBy (fst (runFake emptyWorld (evaluate fakeWorld s e)))

-- 🏁 An ending, as the text it shows (or the error).
-- 終わり方を、見せる文字（かエラー）に
shownBy :: Ending -> Either Text Text
shownBy ending = case ending of
  Finished t   -> Right t
  Errored t    -> Left t
  ExitedWith n -> Left ("[exit " <> T.pack (show n) <> "]")

-- 🏁 An ending of a program: Right () when it went to the end.
-- プログラムの終わり方。最後まで行けば Right ()
endedBy :: Ending -> Either Text ()
endedBy = fmap (const ()) . shownBy

-- ANCHOR: runLinesIn
-- 🎭 Type the lines into a REPL in a fake world: what each one shows,
--    and the world afterwards.
-- 偽物の世界の REPL に、1行ずつ打ちこむ。それぞれが見せるものと、
-- そのあとの世界
runLinesIn :: FakeWorld -> [Text] -> ([Text], FakeWorld)
runLinesIn world inputs = runFake world (go preludeSession inputs)
  where
    go _ [] = pure []
    go s (line : rest) = do
      (s', out) <- step fakeWorld s line
      (out :) <$> go s' rest

-- 🎭 Load a program and run it in a fake world, as lam file.lam does.
-- プログラムを読みこみ、偽物の世界で、lam file.lam と同じように動かす
runFile
  :: Files Identity -> FakeWorld -> FilePath -> Text
  -> (Either Text (), FakeWorld)
runFile files world file source =
  let (ending, world') = runEnding files world file source
  in (endedBy ending, world')

-- 🎭 The same, with how it ended (exit n too).
-- 同じだが、終わり方（exit n も）を返す
runEnding
  :: Files Identity -> FakeWorld -> FilePath -> Text -> (Ending, FakeWorld)
runEnding files world file source = runFake world $
  case runIdentity (loadProgram files preludeSession preludeSession
                      file source) of
    Left err              -> pure (Errored err)
    Right (loaded, final) -> runProgram fakeWorld loaded final
-- ANCHOR_END: runLinesIn
-- ANCHOR_END: run

-- ANCHOR: genText
-- 🎲 Any text, made often of the letters that need care:
--    " \ new line, tab, and letters outside ASCII.
-- どんな文字列でも。ただし、気をつけたい文字を多めに混ぜる
genText :: Gen Text
genText = T.pack <$> listOf (frequency
  [ (3, elements "\"\\\n\t")
  , (3, elements "ab λラ")
  , (1, arbitrary) ])
-- ANCHOR_END: genText

-- ANCHOR: golden
-- 📜 Run an example program, as `lam file.lam` would show it, but in a
--    fake world: the lines of the .in file (if there is one) are typed
--    in, and no file is read or written on the disk. The modules are
--    every .lam file under examples/, and LAM_PATH is the line of the
--    .env file (if there is one).
-- 例のプログラムを、lam file.lam と同じように、でも偽物の世界で動かす。
-- .in ファイル（あれば）の行をキーボードから打ったことにし、ディスクの
-- ファイルは読みも書きもしない。モジュールは examples/ の下の .lam
-- ファイル全部。LAM_PATH は .env ファイルの行（あれば）
runExample :: FilePath -> IO Text
runExample file = do
  source  <- Utf8.readFile file
  modules <- lamFiles "examples"
  env     <- linesOf (dropExtension file <> ".env")
  input   <- linesOf (dropExtension file <> ".in")
  shared  <- worldFilesOf (dropExtension file <> ".files")
  args    <- linesOf (dropExtension file <> ".args")
  -- ANCHOR: goldenFiles
  -- .times: a file (read from the disk) and when it was changed.
  -- .run: a command and its answer (the code, the file of its output).
  -- .times：ファイル（ディスクから読む）と、変えた時刻。
  -- .run：コマンドと、その答え（終了コード、出力のファイル）
  times   <- linesOf (dropExtension file <> ".times") >>= mapM stamp
  runs    <- linesOf (dropExtension file <> ".run") >>= mapM command
  -- ANCHOR_END: goldenFiles
  let path  = [ T.unpack (T.drop 1 v)
              | l <- env, let (k, v) = T.breakOn "=" l, k == "LAM_PATH" ]
      files = mapFiles (Map.fromList modules)
                (concatMap (splitSearchPath) path)
      world = emptyWorld { worldInput = input
                         , worldFiles = shared <> Map.fromList
                             [ (p, t) | (p, t, _) <- times ]
                         , worldTimes = Map.fromList
                             [ (p, ms) | (p, _, ms) <- times ]
                         , worldCommands = Map.fromList runs
                         , worldArgs = args
                         , worldEnv = Map.fromList
                             [("HOME", "/home/lam"), ("LANG", "C.UTF-8")] }
      (ending, done) = runEnding files world file source
  -- The standard error comes after the screen, and then how it ended:
  -- an error, or [exit n] for an exit other than 0.
  -- 標準エラーは画面の後ろに。そのあとに終わり方：エラーか、0 のほかの
  -- exit なら [exit n]
  pure (outputOf done <> errorsOf done <> case ending of
          Finished _   -> ""
          Errored t    -> t <> "\n"
          ExitedWith 0 -> ""
          ExitedWith n -> "[exit " <> T.pack (show n) <> "]\n")
  where
    linesOf f = do
      there <- doesFileExist f
      if there then T.lines <$> Utf8.readFile f else pure []
    -- The files of the fake world: each path in the .files list (from
    -- this project's folder), with what is in it on the disk.
    -- 偽物の世界のファイル：.files の一覧の道（このプロジェクトの
    -- フォルダーから）ごとに、ディスクの中身
    worldFilesOf f = do
      names <- linesOf f
      Map.fromList <$> mapM (\n -> (,) n <$> B.readFile (T.unpack n))
                            names
    -- "examples/files/logs/a.log 2026-11-01T09:10:00Z"
    stamp l = case T.words l of
      [p, iso] -> case (iso8601ParseM (T.unpack iso) :: Maybe UTCTime) of
        Just t -> do
          text <- B.readFile (T.unpack p)
          pure (p, text, floor (utcTimeToPOSIXSeconds t * 1000) :: Integer)
        Nothing -> fail ("a bad time in " <> file <> ": " <> T.unpack l)
      _ -> fail ("a bad line in " <> file <> ": " <> T.unpack l)
    -- "0 gitlog.txt git log --format=%an": the file is next to the .run.
    -- ファイルは .run ファイルの隣から
    command l = case T.words l of
      code : out : cmd -> do
        text <- Utf8.readFile (takeDirectory file </> T.unpack out)
        pure (cmd, (read (T.unpack code) :: Int, text, ""))
      _ -> fail ("a bad line in " <> file <> ": " <> T.unpack l)

-- ANCHOR: script
-- 🎭 Run a script in a fake world: how it ended, and the world after.
-- 偽物の世界でスクリプトを動かす：どう終わったかと、あとの世界
script :: FakeWorld -> Text -> (Ending, FakeWorld)
script world source =
  runEnding (mapFiles Map.empty []) world "script.lam" source
-- ANCHOR_END: script

-- ANCHOR: genWords
-- 🎲 Lines of words: a few words (so that some come again), with
--    capitals, signs and spaces between them.
-- 単語の行。単語は少しだけ（何度も出てくるように）。大文字・記号・
-- 空白を、あいだに混ぜる
genWords :: Gen [Text]
genWords = listOf line
  where
    line = T.concat <$> listOf piece
    piece = frequency
      [ (4, elements ["lam", "Lam", "haskell", "type", "ラムダ", "λ", "é"])
      , (3, elements [" ", ", ", ". ", "!", "  ", "1", "-"]) ]

-- 🏆 The top 10 in Haskell, as the first chapter counts them.
-- Haskell で数えたトップ10。最初の章と同じ数え方
haskellTop10 :: [Text] -> Text
haskellTop10 ls = T.concat
  [ w <> T.replicate (12 - T.length w) " " <> T.pack (show n) <> "\n"
  | (w, n) <- take 10 (sortOn (Down . snd) (Map.toList counts)) ]
  where
    text   = T.unlines ls
    ws     = T.words (T.map (\c -> if isAlpha c then toLower c else ' ') text)
    counts = Map.fromListWith (+) [ (w, 1 :: Int) | w <- ws ]
-- ANCHOR_END: genWords

-- 📂 Every .lam file under a folder: its path and its text.
-- フォルダーの下の .lam ファイル全部：道と中身
lamFiles :: FilePath -> IO [(FilePath, Text)]
lamFiles dir = do
  names <- listDirectory dir
  concat <$> mapM (\n -> do
    let path = dir </> n
    folder <- doesDirectoryExist path
    if folder then lamFiles path
    else if takeExtension n == ".lam"
      then (\t -> [(path, t)]) <$> Utf8.readFile path
      else pure []) names

-- 🗂️ Files from a Map (no disk), and the folders of LAM_PATH.
-- Map のファイル（ディスクなし）と、LAM_PATH のフォルダー
mapFiles :: Map.Map FilePath Text -> [FilePath] -> Files Identity
mapFiles table = Files (\f -> pure (Map.lookup f table))

-- 📜 Load a program that imports nothing of its own.
-- 自分の import がないプログラムを読みこむ
loadPlain :: Session -> FilePath -> Text -> Either Text (Session, Maybe Text)
loadPlain session file source =
  runIdentity (loadProgram noFiles preludeSession session file source)
    >>= finalText

-- 🥇 Golden test: the output must be the same as the .out file.
-- ゴールデンテスト：出力が .out ファイルと同じであること
golden :: String -> Spec
golden base =
  it ("examples/" <> base <> ".lam") $ do
    actual   <- runExample ("examples/" <> base <> ".lam")
    expected <- Utf8.readFile ("examples/" <> base <> ".out")
    actual `shouldBe` expected
-- ANCHOR_END: golden

-- ANCHOR: genPair
-- 🎲 A key and a value. The keys are few, so the same key comes often.
-- キーと値。キーは少しだけなので、同じキーがよく出る
genPair :: Gen (Text, Integer)
genPair = (,) <$> elements ["a", "b", "λ", "\"q\""] <*> choose (0, 9)

-- 🖨️ Pairs, as Lam writes them: [("a", 1), ("b", 2)]
-- 組のリストを、Lam の書き方で
lamPairs :: [(Text, Integer)] -> Text
lamPairs kvs = "[" <> T.intercalate ", "
  [ "(" <> renderString k <> ", " <> T.pack (show v) <> ")" | (k, v) <- kvs ]
  <> "]"
-- ANCHOR_END: genPair

-- ANCHOR: genAeson
-- 🎲 Any JSON value of aeson, not too big. The numbers are made of
--    two whole numbers, as aeson keeps them: 1999 and -2 is 19.99.
-- aeson の JSON の値（大きすぎないもの）。数は、aeson と同じく、
-- 2つの整数から作る
genAeson :: Gen A.Value
genAeson = sized go
  where
    go n = oneof $
      [ pure A.Null
      , A.Bool <$> arbitrary
      , A.Number <$> (scientific <$> arbitrary <*> choose (-30, 30))
      , A.String <$> genText ]
      <> if n <= 0 then [] else
           [ A.Array . V.fromList <$> few (go (n `div` 3))
           , A.object <$> few
               ((\k v -> (Key.fromText k, v))
                  <$> genText <*> go (n `div` 3)) ]
    -- 0 to 4 of them, so that the value does not grow too big.
    -- 0〜4個。値が大きくなりすぎないように
    few g = do
      k <- choose (0, 4)
      vectorOf k g
-- ANCHOR_END: genAeson

main :: IO ()
main = hspec $ do
  -- 「糖衣構文」
  describe "sugar" $
    -- ANCHOR: sugar
    -- 「引数がいくつもあっても、木は1引数の関数の重なり」
    it "turns many arguments into one-argument functions" $
      map (fmap render . parseExpr)
        [ "\\x y -> x + y"
        , "let add x y = x + y in add 1 2" ]
        `shouldBe` map Right
          [ "\\x -> \\y -> x + y"
          , "let add = \\x -> \\y -> x + y in add 1 2" ]
    -- ANCHOR_END: sugar

  -- 「プレリュード」
  describe "prelude" $
    -- ANCHOR: prelude
    -- 「プレリュードの関数に、型が付いている」
    it "gives the prelude functions their types" $
      map (\n -> renderScheme <$> Map.lookup n (sessionTypes preludeSession))
        ["id", "const", "compose", "flip", "not", "max", "gcd"]
        `shouldBe` map Just
          [ "a -> a", "a -> b -> a", "(a -> b) -> (c -> a) -> c -> b"
          , "(a -> b -> c) -> b -> a -> c", "Bool -> Bool"
          , "Comparable a => a -> a -> a", "Int -> Int -> Int" ]
    -- ANCHOR_END: prelude

  -- 「REPL」
  describe "step" $
    it "uses the prelude, and says which definition went wrong" $ do
      let inputs =
            [ "gcd 84 36", "compose not even 7"
            , "let add x y = x + y", "add 1 2"
            , "let bad = not 1", ":type twice twice" ]
          go (s, outs) line =
            let (s', out) = replStep s line in (s', outs <> [out])
      snd (foldl go (preludeSession, []) inputs)
        `shouldBe`
          [ "12 : Int", "true : Bool"
          , "add : Number a => a -> a -> a", "3 : Int"
          , "in the definition of bad: type error: \
            \Bool is not in the class Number"
          , "(a -> a) -> a -> a" ]

  -- 「文字列」
  describe "strings" $ do
    -- ANCHOR: stringRoundTrip
    -- 「どんな文字列でも、ソースコードに書いて読み直すと、同じ文字列」
    it "reads back any string it writes" $
      property $ forAll genText $ \s ->
        parseExpr (render (Lit (LString s))) == Right (Lit (LString s))
    -- ANCHOR_END: stringRoundTrip

    -- ANCHOR: stringExamples
    -- 「文字列をつなぎ、組み込みの関数で変える」
    it "joins strings and changes them with builtins" $
      runLines
        [ "\"Hello, \" ++ \"Lam\""
        , "toUpper \"lam\""
        , "length \"ラムダ\""
        , "\"say \\\"hi\\\"\\n\""
        , "\"a\" ++ 1" ]
        `shouldBe`
          [ "\"Hello, Lam\" : String"
          , "\"LAM\" : String"
          , "3 : Int"
          , "\"say \\\"hi\\\"\\n\" : String"
          , "type error: String is not in the class Number" ]
    -- ANCHOR_END: stringExamples

  -- 「タプルとリスト」
  describe "tuples and lists" $ do
    -- ANCHOR: collections
    -- 「タプルとリストに、型が付いて、計算できる」
    it "gives tuples and lists their types and values" $
      runLines
        [ "(\"Lam\", 42)"
        , "snd (\"Lam\", 42)"
        , ":type []"
        , "1 : 2 : [3]"
        , "map (\\x -> x * x) (range 1 5)"
        , "[1, 2] ++ [3]"
        , "length [1, 2, 3]"
        , "length 5"
        , "[1, true]"
        , "head []" ]
        `shouldBe`
          [ "(\"Lam\", 42) : (String, Int)"
          , "42 : Int"
          , "[a]"
          , "[1, 2, 3] : [Int]"
          , "[1, 4, 9, 16, 25] : [Int]"
          , "[1, 2, 3] : [Int]"
          , "3 : Int"
          , "type error: Int is not in the class HasLength"
          , "type error: Bool is not in the class Number"
          , "error: head: the list is empty" ]
    -- ANCHOR_END: collections

    -- ANCHOR: listPrelude
    -- 「リストの関数に、型が付いている」
    it "gives the list functions their types" $
      map (\n -> renderScheme <$> Map.lookup n (sessionTypes preludeSession))
        ["foldr", "map", "filter", "length", "range"]
        `shouldBe` map Just
          [ "Foldable f => (a -> b -> b) -> b -> f a -> b"
          , "Functor f => (a -> b) -> f a -> f b"
          , "(a -> Bool) -> [a] -> [a]"
          , "HasLength a => a -> Int"
          , "(Comparable a, Number a) => a -> a -> [a]" ]
    -- ANCHOR_END: listPrelude

    -- ANCHOR: listLikeHaskell
    -- 「Lam のリストの関数は、Haskell のものと同じ答えを出す」
    it "computes the same lists as Haskell" $
      -- Negative numbers, too: renderList writes -3 as it is.
      -- 負の数も。renderList は -3 をそのまま書く
      property $ forAll (listOf (choose (-9, 9))) $ \xs ->
        let source = "reverse (filter (\\x -> x > 4) "
                     <> renderList xs <> ")"
        in runLines [source]
             == [renderList (reverse (filter (> 4) xs)) <> " : [Int]"]
    -- ANCHOR_END: listLikeHaskell

  -- 「比べる」
  describe "comparing" $
    -- ANCHOR: compare
    -- 「Comparable の型なら比べられる。関数は、動かす前に断る」
    it "compares Comparable values, and refuses functions before running" $
      runLines
        [ "\"apple\" < \"banana\""
        , "[1, 2] < [1, 3]"
        , "(\"Lam\", 1) /= (\"Lam\", 2)"
        , "\"ab\" <= \"b\""
        , ":type \\x y -> x <= y"
        , "(\\x -> x) == (\\x -> x)"
        , "[toUpper] == []"
        , "max toUpper toLower" ]
        `shouldBe`
          [ "true : Bool", "true : Bool", "true : Bool"
          , "true : Bool"
          , "Comparable a => a -> a -> Bool"
          , "type error: a -> a is not in the class Comparable"
          , "type error: String -> String is not in the class Comparable"
          , "type error: String -> String is not in the class Comparable" ]
    -- ANCHOR_END: compare

  -- 「&& と ||」
  describe "&& and ||" $ do
    -- ANCHOR: shortcut
    -- 「&& と || は、要らないときは右側を計算しない」
    it "does not compute the right side when it is not needed" $
      runLines
        [ "and false (1 // 0 == 0)"
        , "false && 1 // 0 == 0"
        , "true || 1 // 0 == 0"
        , "true && 1 // 0 == 0" ]
        `shouldBe`
          [ "error: divided by zero"
          , "false : Bool"
          , "true : Bool"
          , "error: divided by zero" ]
    -- ANCHOR_END: shortcut

    -- ANCHOR: shortcutLikeHaskell
    -- 「&& と || の答えは、Haskell の && と || と同じ」
    it "gives the same answers as Haskell" $
      property $ \a b c ->
        let lam x = if x then "true" else "false"
            source = lam a <> " || " <> lam b <> " && " <> lam c
        in runLines [source] == [lam (a || b && c) <> " : Bool"]
    -- ANCHOR_END: shortcutLikeHaskell

  -- ANCHOR: examples
  -- 「例のプログラム（ゴールデンテスト）」
  describe "examples" $
    mapM_ golden
      [ "fib", "church", "broken", "answers", "strings", "words", "shapes"
      , "sugar"
      -- ANCHOR: recordGolden
      , "records/annotate", "records/report", "records/tuples"
      , "records/crew", "records/shapes", "records/scores"
      , "records/broken/unnoted", "records/broken/noted"
      , "records/broken/first", "records/broken/swap"
      , "records/broken/largest", "records/broken/len"
      , "records/broken/len-late", "records/broken/order"
      -- ANCHOR_END: recordGolden
      -- ANCHOR: floatGolden
      , "float/temps", "float/split", "float/prices"
      , "float/broken/mixed", "float/broken/called"
      -- ANCHOR_END: floatGolden
      -- ANCHOR: classGolden
      , "classes/before", "classes/describe", "classes/byhand"
      , "classes/containers", "classes/combine", "classes/defaults"
      , "classes/ranked", "classes/broken/missing", "classes/broken/unsaid"
      , "classes/broken/ambiguous", "classes/broken/ambiguous-fixed"
      , "classes/broken/super"
      -- ANCHOR_END: classGolden
      -- ANCHOR: jsonGolden
      , "json/grid", "json/orders", "json/tojson"
      -- ANCHOR_END: jsonGolden
      -- ANCHOR: stdlibGolden
      , "stdlib/signup", "stdlib/quiz", "stdlib/tally", "stdlib/team"
      , "stdlib/skipped"
      -- ANCHOR_END: stdlibGolden
      -- ANCHOR: formatGolden
      , "format/money" ]
      -- ANCHOR_END: formatGolden
  -- ANCHOR_END: examples

  -- 「data と case」
  describe "data and case" $ do
    -- ANCHOR: dataExamples
    -- 「型を宣言し、case で値を分ける」
    it "declares types, and takes values apart with case" $
      runLines
        [ "data Shape = Circle Int | Rect Int Int"
        , "Circle 2"
        , ":type Rect"
        , "let area s = case s of | Circle r -> 3 * r * r | Rect w h -> w * h"
        , "map area [Circle 2, Rect 3 4]"
        , "case (\"Rust\", 91, true) of | (n, score, _) -> score"
        , "case [1, 2, 3] of | [] -> 0 | x : rest -> x + length rest"
        , "case 5 of | 0 -> \"zero\" | 1 -> \"one\"" ]
        `shouldBe`
          [ "Circle : Int -> Shape\nRect : Int -> Int -> Shape"
          , "Circle 2 : Shape"
          , "Int -> Int -> Shape"
          , "area : Shape -> Int"
          , "[12, 12] : [Int]"
          , "91 : Int"
          , "3 : Int"
          , "error: no branch of case fits 5" ]
    -- ANCHOR_END: dataExamples

    -- ANCHOR: patternTypes
    -- 「パターンのまちがいは、動かす前に見つける」
    it "finds mistakes in patterns before running" $
      drop 1 (runLines
        [ "data Shape = Circle Int | Rect Int Int"
        , "case Circle 1 of | Rect w -> w"
        , "case Circle 1 of | Circel r -> r"
        , "case Circle 1 of | Circle \"r\" -> 0"
        , "case Circle 1 of | Circle r -> r | Rect w h -> \"wide\""
        , ":type \\m -> case m of | Nothing -> 0 | Just x -> x" ])
        `shouldBe`
          [ "type error: Rect needs 2 arguments, but the pattern has 1"
          , "type error: unknown constructor: Circel"
          , "type error: the types do not fit: String where Int is needed"
          , "type error: the types do not fit: String where Int is needed"
          , "Number a => Maybe a -> a" ]
    -- ANCHOR_END: patternTypes

    -- ANCHOR: dataErrors
    -- 「data の宣言のまちがい」
    it "finds mistakes in data declarations" $
      runLines
        [ "data T = C Strng"
        , "data T = C a"
        , "data T = C Maybe"
        , "data List a = Nil" ]
        `shouldBe`
          [ "in the data type T: type error: unknown type: Strng"
          , "in the data type T: type error: unknown type variable: a"
          , "in the data type T: type error: the type Maybe needs \
            \1 argument, but got 0"
          , "in the data type List: type error: List is a builtin type" ]
    -- ANCHOR_END: dataErrors

    -- ANCHOR: guards
    -- 「ガードが偽なら、次の枝へ進む」
    it "goes on to the next branch when a guard is false" $
      runLines
        [ "let size n = case n of | 0 -> \"zero\" | k if k < 10 -> \"small\" \
          \| _ -> \"big\""
        , "map size [0, 3, 42]"
        , "let x = 10 in case 50 of | x if x > 100 -> 0 | _ -> x" ]
        `shouldBe`
          [ "size : Int -> String"
          , "[\"zero\", \"small\", \"big\"] : [String]"
          , "10 : Int" ]
    -- ANCHOR_END: guards

    -- ANCHOR: guardSugar
    -- 「ガードは、case と if に開く」
    it "turns guards into case and if" $
      fmap render (parseExpr "case n of | k if k < 10 -> 1 | _ -> 2")
        `shouldBe` Right
          "let $it = n in \
          \let $next = \\$u -> case $it of | _ -> 2 in \
          \case $it of | k -> (if k < 10 then 1 else $next 0) | _ -> $next 0"
    -- ANCHOR_END: guardSugar

    -- ANCHOR: maybeResult
    -- 「プレリュードに Maybe と Result がある」
    it "has Maybe and Result in the prelude" $
      runLines
        [ "lookup \"TS\" [(\"Python\", 82), (\"TS\", 75)]"
        , "lookup \"Go\" [(\"Python\", 82)]"
        , "fromMaybe 0 (lookup \"Go\" [(\"Python\", 82)])"
        , "find even [1, 3, 4, 5]"
        , ":type Err"
        , "[Ok 1, Err \"oops\"]" ]
        `shouldBe`
          [ "Just 75 : Maybe Int"
          , "Nothing : Maybe Int"
          , "0 : Int"
          , "Just 4 : Maybe Int"
          , "a -> Result a b"
          , "[Ok 1, Err \"oops\"] : [Result String Int]" ]

    -- 「Maybe の関数に、型が付いている」
    it "gives the Maybe functions their types" $
      map (\n -> renderScheme <$> Map.lookup n (sessionTypes preludeSession))
        ["fromMaybe", "find", "lookup"]
        `shouldBe` map Just
          [ "a -> Maybe a -> a"
          , "Foldable f => (a -> Bool) -> f a -> Maybe a"
          , "Comparable a => a -> [(a, b)] -> Maybe b" ]
    -- ANCHOR_END: maybeResult

  -- 「自分の型を比べる」
  describe "comparing data" $ do
    -- ANCHOR: compareData
    -- 「Comparable かどうかは、中身から決まる」
    it "decides from the insides whether a type is Comparable" $
      runLines
        [ "data Shape = Circle Int | Rect Int Int"
        , "Circle 5 < Rect 1 1"
        , "Rect 1 2 < Rect 1 3"
        , "Nothing < Just 0"
        , ":type \\x y -> Just x == y"
        , "Just toUpper == Nothing"
        , "data Handler = Button (Int -> Int) | Off"
        , "Off == Off"
        , "data Tagged a = Tagged Int"
        , ":type \\x -> x == Tagged 1"
        , "data Tree a = Leaf | Node (Tree a) a (Tree a)"
        , "Node Leaf 1 Leaf < Node Leaf 2 Leaf"
        , ":type \\x y -> Node Leaf x Leaf == y" ]
        `shouldBe`
          [ "Circle : Int -> Shape\nRect : Int -> Int -> Shape"
          , "true : Bool"
          , "true : Bool"
          , "true : Bool"
          , "Comparable a => a -> Maybe a -> Bool"
          , "type error: String -> String is not in the class Comparable"
          , "Button : (Int -> Int) -> Handler\nOff : Handler"
          , "type error: Handler is not in the class Comparable \
            \(Button holds Int -> Int)"
          , "Tagged : Int -> Tagged a"
          , "Tagged a -> Bool"
          , "Leaf : Tree a\nNode : Tree a -> a -> Tree a -> Tree a"
          , "true : Bool"
          , "Comparable a => a -> Tree a -> Bool" ]
    -- ANCHOR_END: compareData

    -- ANCHOR: compareLikeHaskell
    -- 「Maybe の大小は、Haskell の deriving Ord と同じ」
    it "orders Maybe like Haskell's deriving Ord" $
      property $ forAll ((,) <$> genMaybe <*> genMaybe) $ \(a, b) ->
        let source = lamMaybe a <> " < " <> lamMaybe b
        in runLines [source] == [lamBool (a < b) <> " : Bool"]
    -- ANCHOR_END: compareLikeHaskell

  -- 「パターンを引数に」
  describe "patterns as arguments" $ do
    -- ANCHOR: lambdaSugar
    -- 「パターンの引数は、枝が1つの case に開く」
    it "opens a pattern argument into a case with one branch" $
      map (fmap render . parseExpr)
        [ "\\(a, b) -> a + b"
        , "\\x (Just y) -> x + y"
        , "let (q, r) = (17 // 5, mod 17 5) in q * 10 + r"
        , "let swap (a, b) = (b, a) in swap" ]
        `shouldBe` map Right
          [ "\\$arg -> case $arg of | (a, b) -> a + b"
          , "\\x -> \\$arg -> case $arg of | Just y -> x + y"
          , "case (17 // 5, mod 17 5) of | (q, r) -> q * 10 + r"
          , "let swap = \\$arg -> case $arg of | (a, b) -> (b, a) in swap" ]
    -- ANCHOR_END: lambdaSugar

    -- ANCHOR: patternArgs
    -- 「タプルや構成子を、引数でほどく」
    it "takes tuples and constructors apart in arguments" $
      runLines
        [ "map (\\(name, score) -> name) [(\"Python\", 82), (\"TS\", 75)]"
        , "let (q, r) = (17 // 5, mod 17 5) in (q, r)"
        , "let swap (a, b) = (b, a)"
        , "swap (1, \"one\")"
        , "let area (w, h) = w * h"
        , "(\\(Just x) -> x) Nothing"
        , "let (a, b) = (1, 2)" ]
        `shouldBe`
          [ "[\"Python\", \"TS\"] : [String]"
          , "(3, 2) : (Int, Int)"
          , "swap : (a, b) -> (b, a)"
          , "(\"one\", 1) : (String, Int)"
          , "area : Number a => (a, a) -> a"
          , "error: no branch of case fits Nothing"
          , "<repl>:1:20:\n  |\n1 | let (a, b) = (1, 2)\n\
            \  |                    ^\n\
            \unexpected end of input\nexpecting a value, an operator, \
            \or in (a let with a pattern needs in)\n" ]
    -- ANCHOR_END: patternArgs

    -- ANCHOR: letNotRecursive
    -- 「パターンの let は再帰しない（外の x を使う）」
    it "does not make a pattern let recursive" $
      runLines ["let x = 5 in let (x, y) = (x + 1, x + 2) in (x, y)"]
        `shouldBe` ["(6, 7) : (Int, Int)"]
    -- ANCHOR_END: letNotRecursive

  -- 「演算子を関数に」
  describe "sections" $ do
    -- ANCHOR: sectionSugar
    -- 「演算子とセクションは、ラムダに開く」
    it "opens operators and sections into lambdas" $
      map (fmap render . parseExpr)
        ["(+)", "(+ 1)", "(10 -)", "(- 1)", "(:)", "(x :)"]
        `shouldBe` map Right
          [ "\\$l -> \\$r -> $l + $r"
          , "\\$l -> $l + 1"
          , "\\$r -> 10 - $r"
          , "\\$l -> $l - 1"
          , "\\$l -> \\$r -> $l : $r"
          , "\\$r -> x : $r" ]
    -- ANCHOR_END: sectionSugar

    -- ANCHOR: sections
    -- 「セクションを使う。(-1) はセクションではなく、負の数」
    it "uses sections, and reads (-1) as a negative number" $
      runLines
        [ "map (* 2) [1, 2, 3]"
        , "(10 -) 3"
        , "(- 1) 10"
        , "foldr (+) 0 [1, 2, 3]"
        , ":type (++)"
        , "(-1)" ]
        `shouldBe`
          [ "[2, 4, 6] : [Int]"
          , "7 : Int"
          , "9 : Int"
          , "6 : Int"
          , "Semigroup a => a -> a -> a"
          , "-1 : Int" ]
    -- ANCHOR_END: sections

  -- 「リスト内包表記」
  describe "list comprehensions" $ do
    -- ANCHOR: comprehensionSugar
    -- 「内包表記は、ループ $go に開く」
    it "opens a comprehension into a loop $go" $
      fmap render (parseExpr "[x * 2 | x <- xs, x > 1]")
        `shouldBe` Right
          "let $go = \\$xs -> case $xs of \
          \| [] -> [] \
          \| x : $rest -> (if x > 1 then [x * 2] else []) ++ $go $rest \
          \in $go xs"
    -- ANCHOR_END: comprehensionSugar

    -- ANCHOR: comprehensions
    -- 「<- をいくつも、条件、let、外れうるパターン」
    it "has many <-, conditions, let, and patterns that may not fit" $
      runLines
        [ "[x * 2 | x <- [1, 2, 3, 4], x > 1]"
        , "[(x, y) | x <- [1, 2, 3], y <- [1, 2, 3], x < y]"
        , "[(x, y) | x <- [1, 2], y <- [x, 10]]"
        , "[y | x <- [1, 2, 3], let y = x * 10, y > 15]"
        , "[x | Just x <- [Just 1, Nothing, Just 3]]"
        , ":type \\xs -> [(x, y) | x <- xs, y <- xs]"
        , "[x | x <- 5]" ]
        `shouldBe`
          [ "[4, 6, 8] : [Int]"
          , "[(1, 2), (1, 3), (2, 3)] : [(Int, Int)]"
          , "[(1, 1), (1, 10), (2, 2), (2, 10)] : [(Int, Int)]"
          , "[20, 30] : [Int]"
          , "[1, 3] : [Int]"
          , "[a] -> [(a, a)]"
          , "type error: [a] is not in the class Number" ]
    -- ANCHOR_END: comprehensions

    -- ANCHOR: comprehensionLikeHaskell
    -- 「Haskell の内包表記と同じ答えになる」
    it "gives the same answers as Haskell's comprehensions" $
      property $ forAll (listOf (choose (0, 9))) $ \xs ->
        forAll (listOf (choose (0, 9))) $ \ys ->
          let source = "[x * 10 + y | x <- " <> renderList xs
                         <> ", x > 2, y <- " <> renderList ys
                         <> ", let s = x + y, mod s 2 == 0]"
              expected = [ x * 10 + y | x <- xs, x > 2, y <- ys
                                      , let s = x + y, s `mod` 2 == 0 ]
          in runLines [source] == [renderList expected <> " : [Int]"]
    -- ANCHOR_END: comprehensionLikeHaskell

  -- 「パイプラインと合成」
  describe "pipes and composition" $ do
    -- ANCHOR: pipeSugar
    -- 「|> と >> も、App と Lam に開く」
    it "opens |> and >> into App and Lam" $
      map (fmap render . parseExpr)
        ["xs |> map f |> sum", "sum <| map f <| xs", "f >> g", "g << f"]
        `shouldBe` map Right
          [ "sum (map f xs)"
          , "sum (map f xs)"
          , "\\$x -> g (f $x)"
          , "\\$x -> g (f $x)" ]
    -- ANCHOR_END: pipeSugar

    -- ANCHOR: pipes
    -- 「左から右へ流す、関数をつなぐ」
    it "flows from left to right, and joins functions" $
      runLines
        [ "[1, 2, 3] |> map (* 2) |> sum"
        , "((+ 1) >> (* 2)) 5"
        , "((+ 1) << (* 2)) 5"
        , ":type (>>)"
        , "[\"b\", \"a\"] |> map (toUpper >> chars) |> length"
        , "case 3 |> (+ 1) of | 4 -> \"four\" | _ -> \"other\"" ]
        `shouldBe`
          [ "12 : Int"
          , "12 : Int"
          , "11 : Int"
          , "(a -> b) -> (b -> c) -> a -> c"
          , "2 : Int"
          , "\"four\" : String" ]
    -- ANCHOR_END: pipes

  -- 「型の注釈とレコード」
  describe "annotations and records" $ do
    -- ANCHOR: annotationTest
    -- 「let のない定義、注釈、where を読む。ファイルの終わりの x : xs は式」
    it "reads definitions without let, annotations and where" $ do
      let decls src = fmap (\(ds, final) ->
            ( [ case d of
                  DefValue x e -> x <> " = " <> render e
                  DefSig x _   -> x <> " : ..."
                  _            -> "?"
              | d <- ds ]
            , fmap render final ))
            (runLamParser programParts "" src)
      decls "add : Int -> Int -> Int\nadd x y = x + y\n"
        `shouldBe` Right (["add : ...", "add = \\x -> \\y -> x + y"], Nothing)
      decls "f x = a * b\n  where\n    a = x + 1\n    b = 2\n"
        `shouldBe` Right
          (["f = \\x -> let a = x + 1 in let b = 2 in a * b"], Nothing)
      decls "xs = [2]\nx = 1\nx : xs\n"
        `shouldBe` Right (["xs = [2]", "x = 1"], Just "x : xs")
    -- ANCHOR_END: annotationTest

    -- ANCHOR: recordTypesTest
    -- 「レコードの型：読むだけなら、ほかのフィールドがあってよい」
    it "gives the types of records, with the rest of the fields" $
      runLines
        [ "{ name = \"Lam\", age = 3 }"
        , ":type \\r -> r.name"
        , ":type \\r -> (r.name, r.age)"
        , ":type \\r -> { r | age = r.age + 1 }"
        , ":type .name"
        , ":type .name >> toUpper"
        , ":type \\{ x, y } -> x + y"
        , "(\\r -> r.name) { name = \"Ruby\", version = \"4.0\" }"
        , "{ name = \"Lam\" }.age"
        , "{ name = \"Lam\" } == { name = \"Lam\" }"
        , "{ f = \\x -> x } == { f = \\x -> x }" ]
        `shouldBe`
          [ "{ age = 3, name = \"Lam\" } : { age : Int, name : String }"
          , "{ r | name : a } -> a"
          , "{ r | age : a, name : b } -> (b, a)"
          , "Number a => { r | age : a } -> { r | age : a }"
          , "{ r | name : a } -> a"
          , "{ r | name : String } -> String"
          , "Number a => { r | x : a, y : a } -> a"
          , "\"Ruby\" : String"
          , "type error: { name : String } has no field age"
          , "true : Bool"
          , "type error: a -> a is not in the class Comparable" ]
    -- ANCHOR_END: recordTypesTest

    -- 「本文の REPL のやり取り」
    it "answers as the chapter shows" $
      runLines
        [ "r = { name = \"Lam\", age = 3 }", "r.name", "{ r | age = 4 }", "r"
        , "r.title", "{ r | title = \"x\" }", "{ r | age = \"four\" }"
        , "label r = \"[\" ++ r.name ++ \"]\"", "label r"
        , "label { name = \"Ruby\", version = \"4.0\" }", "label { title = \"x\" }"
        , "total : [Int] -> Int; total xs = sum xs"
        , ":type max", "biggest : Int -> Int -> Int; biggest = max"
        , "{ name = \"Lam\" } 1"
        , "half : Int -> Int; double x = x * 2" ]
        `shouldBe`
          [ "r : Number a => { age : a, name : String }\n\
            \  -- usually : { age : Int, name : String }"
          , "\"Lam\" : String"
          , "{ age = 4, name = \"Lam\" } : { age : Int, name : String }"
          , "{ age = 3, name = \"Lam\" } : { age : Int, name : String }"
          , "type error: { age : a, name : String } has no field title"
          , "type error: { age : a, name : String } has no field title"
          , "type error: String is not in the class Number"
          , "label : { r | name : String } -> String"
          , "\"[Lam]\" : String"
          , "\"[Ruby]\" : String"
          , "type error: { title : String } has no field name"
          , "total : [Int] -> Int"
          , "Comparable a => a -> a -> a"
          , "biggest : Int -> Int -> Int"
          , "type error: the types do not fit: { name : String } \
            \where a -> b is needed"
          , "<repl>:1:36:\n  |\n1 | half : Int -> Int; double x = x * 2\n  |                                    ^\nthe annotation and the definition have different names\n" ]

    -- ANCHOR: updateProp
    -- 「一部を変えたレコード：変えたフィールドは新しい値、ほかはそのまま」
    it "changes only the fields it names" $
      property $ \a b n ->
        let r = "{ a = " <> num a <> ", b = " <> num b <> " }"
            changed = "{ r | a = " <> num n <> " }"
        in map valuePart (runLines
             [ inRecord r (changed <> ".a")
             , inRecord r (changed <> ".b")
             , inRecord r "r.a" ])
           === map (T.pack . show) [n, b, a]
    -- ANCHOR_END: updateProp

    -- ANCHOR: aliasTest
    -- 「別名は、中身の型と同じ」
    it "takes an alias as the type it stands for" $
      runLines
        [ "type alias User = { name : String, age : Int }"
        , "older : User -> User; older u = { u | age = u.age + 1 }"
        , "older { name = \"Lam\", age = 3 }"
        , "type alias Pair a = (a, a)"
        , "both : Pair Int -> Int; both (x, y) = x + y"
        , "type alias Bad = Bad"
        , "type alias Int = Bool" ]
        `shouldBe`
          [ "type alias User"
          , "older : { age : Int, name : String } -> { age : Int, name : String }"
          , "{ age = 4, name = \"Lam\" } : { age : Int, name : String }"
          , "type alias Pair"
          , "both : (Int, Int) -> Int"
          , "in the type alias Bad: type error: unknown type: Bad"
          , "in the type alias Int: type error: Int is a builtin type" ]
    -- ANCHOR_END: aliasTest

    -- 「注釈の下に、同じ名前の定義が要る」
    it "needs the definition right below an annotation" $
      fmap snd (loadPlain preludeSession "<test>" "half : Int -> Int\ndouble x = x * 2\n")
        `shouldBe` Left "error: the type annotation of half needs the definition of half right below it"

  -- 「キーワードで始まる名前」
  describe "names" $
    -- ANCHOR: keywordStart
    -- 「キーワードで始まる名前も読める（iffy、letter、cases、dataset、
    --   double、done）」
    it "reads names that start with a keyword" $
      runLines
        [ "let iffy = 1", "iffy + 1", "let letter = \"a\"", "letter"
        , "let cases = 2", "cases * 3", "let dataset = true", "dataset"
        , "double x = x * 2", "double 4", "done = true", "done" ]
        `shouldBe`
          [ "iffy : Number a => a\n  -- usually : Int", "2 : Int"
          , "letter : String", "\"a\" : String"
          , "cases : Number a => a\n  -- usually : Int", "6 : Int"
          , "dataset : Bool", "true : Bool"
          , "double : Number a => a -> a", "8 : Int"
          , "done : Bool", "true : Bool" ]
    -- ANCHOR_END: keywordStart

  -- 「小数」
  describe "floats" $ do
    -- ANCHOR: floatParse
    -- 「数のリテラルと負の数を読む。マイナスの前後の空白で見分ける」
    it "reads Floats and negative numbers, by the spaces around -" $
      map (fmap render . parseExpr)
        [ "1.5", "1e3", "2.5e-3", "-7", "-1.5", "f -7", "x - 1", "x-1"
        , "x -1", "-x", "-(a + b)", "a * -2", "[-1, -2]", "(-1)", "(- 1)"
        , "-r.age", "Float.isNaN x" ]
        `shouldBe` map Right
          [ "1.5", "1000.0", "0.0025", "-7", "-1.5", "f -7", "x - 1"
          , "x - 1", "x -1", "negate x", "negate (a + b)", "a * -2"
          , "[-1, -2]", "-1", "\\$l -> $l - 1", "negate r.age"
          , "Float.isNaN x" ]
    -- ANCHOR_END: floatParse

    -- ANCHOR: floatShow
    -- 「Float は、JavaScript と同じ形で見せる（node で確かめた）」
    it "shows a Float as JavaScript does" $
      map showFloat
        [ 1, 0.1 + 0.2, 1e21, 1e-7, 123456789012345680000, 0.000001
        , 1.5e-10, -2.5, 100, 1 / 3, 5e-324, 1.7976931348623157e308
        , 2 ^ (53 :: Int), 0 / 0, 1 / 0, -1 / 0 ]
        `shouldBe`
          [ "1", "0.30000000000000004", "1e+21", "1e-7"
          , "123456789012345680000", "0.000001", "1.5e-10", "-2.5", "100"
          , "0.3333333333333333", "5e-324", "1.7976931348623157e+308"
          , "9007199254740992", "NaN", "Infinity", "-Infinity" ]
    -- ANCHOR_END: floatShow

    -- ANCHOR: floatLiteralProp
    -- 「どんな Float のリテラルも、書いて読み直すと同じ Float」
    it "reads back any Float literal it writes" $
      property $ \x ->
        parseExpr (render (Lit (LFloat x))) == Right (Lit (LFloat x))
    -- ANCHOR_END: floatLiteralProp

    -- ANCHOR: floatLikeHaskell
    -- 「+ - * / の答えは、Haskell の Double と同じ」
    it "computes + - * / as Haskell's Double does" $
      property $ \a b ->
        let lit x = "(" <> floatLiteral x <> ")"
            run o = runLines [lit a <> " " <> o <> " " <> lit b]
        in map run ["+", "-", "*", "/"]
             === map (\x -> [showFloat x <> " : Float"])
                     [a + b, a - b, a * b, a / (b :: Double)]
    -- ANCHOR_END: floatLikeHaskell

    -- ANCHOR: intDivLikeHaskell
    -- 「// は Haskell の div、//? は 0 のときだけ Nothing」
    it "divides Ints as Haskell's div does" $
      property $ \a b ->
        runLines [num a <> " // " <> num b, num a <> " //? " <> num b]
          === if b == 0
                then ["error: divided by zero", "Nothing : Maybe Int"]
                else [ T.pack (show (a `div` b)) <> " : Int"
                     , "Just " <> num (a `div` b) <> " : Maybe Int" ]
    -- ANCHOR_END: intDivLikeHaskell

    -- ANCHOR: roundingProp
    -- 「丸めの3つのやり方は、半分のときだけちがう」
    it "rounds a half in three ways, and the rest alike" $
      property $ \(n :: Integer) (k :: Int) ->
        let x = fromInteger n + fromIntegral (k `mod` 10) / 10 :: Double
            lit = "(" <> floatLiteral x <> ")"
            answers = runLines
              [ "round " <> lit, "Float.roundTiesEven " <> lit
              , "Float.roundTiesUp " <> lit ]
            just m = "Just " <> num m <> " : Maybe Int"
            r = toRational x
            away = if r < 0 then negate (round' (negate r)) else round' r
            round' q = floor (q + 1 / 2) :: Integer
        in answers === [just away, just (round r), just (floor (r + 1 / 2))]
    -- ANCHOR_END: roundingProp

    -- ANCHOR: floatRepl
    -- 「本文の REPL のやり取り」
    it "answers the REPL lines of the chapter" $
      runLines
        [ "7 / 2", "1.5 + 2", "0.1 + 0.2", "1e3", "-7", "abs -7"
        , "(-1)", "(- 1) 10", "map (- 1) [10, 20]"
        , "x = 10", "x - 1", "x-1", "x -1", "-(x + 1)"
        , "double n = n * 2", "double 4", "double 1.5"
        , "length [1, 2] * 1.5", "toFloat (length [1, 2]) * 1.5"
        , "\"a\" + 1", "1 + 2", ":type 1 + 2", "n = 3"
        , "7 // 2", "-7 // 2", "7 // 0", "7 //? 0", "1 / 0", "0 / 0"
        , "7 /? 2", "round 2.5", "round -2.5", "Float.roundTiesEven 2.5"
        , "Float.roundTiesUp -2.5", "floor -2.7", "truncate -2.7"
        , "round (0 / 0)", "round 2.5 ?: 0", "Nothing ?: Just 3 ?: 0"
        , "Just 1 ?: 1 // 0", "Err \"no\" ?: 0", "Ok 2 ?: 0"
        , ":type \\m -> m ?: 0"
        , "nan = 0 / 0", "nan == nan", "Float.isNaN nan"
        , "showFloat (2 / 3)", "Float.fixed 3 (2 / 3)", "Float.fixed 1 -0.25"
        , "Float.fixed 0 2.5", "Float.fixed 2 7"
        , "next : a -> a; next x = x + 1"
        , "next : Number a => a -> a; next x = x + 1"
        , "3 == 3.0", "toFloat 3", "sqrt 2", "Float.pow 2 10" ]
        `shouldBe`
          [ "3.5 : Float", "3.5 : Float", "0.30000000000000004 : Float"
          , "1000 : Float", "-7 : Int", "7 : Int"
          , "-1 : Int", "9 : Int", "[9, 19] : [Int]"
          , "x : Number a => a\n  -- usually : Int", "9 : Int", "9 : Int"
          , "type error: a number and a function are mixed up \
            \(a -> b is not in the class Number): x -1 gives -1 to x \
            \(write x - 1), and -f x is (-f) x (write -(f x))"
          , "-11 : Int"
          , "double : Number a => a -> a", "8 : Int", "3 : Float"
          , "type error: the types do not fit: Float where Int is needed"
          , "3 : Float"
          , "type error: String is not in the class Number"
          , "3 : Int", "Number a => a\n  -- usually : Int"
          , "n : Number a => a\n  -- usually : Int"
          , "3 : Int", "-4 : Int", "error: divided by zero"
          , "Nothing : Maybe Int", "Infinity : Float", "NaN : Float"
          , "Just 3.5 : Maybe Float", "Just 3 : Maybe Int"
          , "Just (-3) : Maybe Int", "Just 2 : Maybe Int"
          , "Just (-2) : Maybe Int", "Just (-3) : Maybe Int"
          , "Just (-2) : Maybe Int", "Nothing : Maybe Int", "3 : Int"
          , "3 : Int", "1 : Int", "0 : Int", "2 : Int"
          , "(Fallible f, Number a) => f a -> a"
          , "nan : Float", "false : Bool", "true : Bool"
          , "\"0.6666666666666666\" : String", "\"0.667\" : String"
          , "\"-0.3\" : String", "\"3\" : String", "\"7.00\" : String"
          , "in the definition of next: type error: the \
            \annotation of next must say Number a (the definition needs it)"
          , "next : Number a => a -> a", "true : Bool", "3 : Float"
          , "1.4142135623730951 : Float", "1024 : Float" ]
    -- ANCHOR_END: floatRepl

    -- ANCHOR: splitRepl
    -- 「割り勘の例を :load して、share を試す」
    it "splits the bill in the example" $ do
      source <- Utf8.readFile "examples/float/split.lam"
      let loaded = case loadPlain preludeSession "split.lam" source of
            Right (s, _) -> s
            Left err     -> error (T.unpack err)
          go (s, outs) line =
            let (s', out) = replStep s line in (s', outs <> [out])
      snd (foldl go (loaded, []) ["share 4", "share 0", "exact 4"])
        `shouldBe`
          [ "Just (622, 2) : Maybe (Int, Int)", "Nothing : Maybe (Int, Int)"
          , "\"622.50: 623 622 623\" : String" ]
    -- ANCHOR_END: splitRepl

    -- ANCHOR: orElseErrors
    -- 「?: の左が Maybe でも Result でもなければ、そう教える」
    it "says what ?: needs on its left" $
      runLines ["3 ?: 0", "\"a\" ?: \"b\"", "[1] ?: 0", "round 2.5 ?: 0"]
        `shouldBe`
          [ "type error: ?: needs a Maybe or a Result on its left, but got \
            \a number (only a Maybe or a Result can be missing its value)"
          , "type error: ?: needs a Maybe or a Result on its left, but got \
            \String (only a Maybe or a Result can be missing its value)"
          , "type error: ?: needs a Maybe or a Result on its left, but got \
            \[a] (only a Maybe or a Result can be missing its value)"
          , "3 : Int" ]
    -- ANCHOR_END: orElseErrors

  -- 「複数行」
  -- 「クラスとインスタンス」
  describe "classes" $ do
    -- ANCHOR: classParse
    -- 「class の下に、メソッドの型と既定のメソッドを並べて読む」
    it "reads the methods and the defaults under a class" $
      fmap (\(decls, _) ->
              [ (classDeclName c, classVar c, map fst (classMethods c)
                , map fst (classDefaults c))
              | DefClass c <- decls ])
        (runLamParser programParts ""
           "class Describe a where\n\
           \  describe : a -> String\n\
           \  describeAll : [a] -> String\n\
           \  describeAll xs = join \", \" (map describe xs)\n\
           \describe\n")
        `shouldBe` Right
          [("Describe", "a", ["describe", "describeAll"], ["describeAll"])]
    -- ANCHOR_END: classParse

    -- ANCHOR: classRepl
    -- 「本文の REPL のやり取り（1行で書けるクラス）」
    it "answers the REPL lines of the chapter" $
      runLines
        [ "class Describe a where describe : a -> String"
        , "instance Describe Int where describe n = showInt n"
        , "describe 42", ":type describe", ":dicts describe 42"
        , "describe true"
        , "describeTwice x = describe x ++ \", \" ++ describe x"
        , ":dicts describeTwice 7"
        , ":dicts \\x -> describe x ++ describe x" ]
        `shouldBe`
          [ "describe : Describe a => a -> String"
          , "instance Describe Int"
          , "\"42\" : String", "Describe a => a -> String"
          , "describe $Describe$Int 42"
          , "type error: Bool is not in the class Describe"
          , "describeTwice : Describe a => a -> String"
          , "describeTwice $Describe$Int 7"
          , "\\$Describe_a -> \\x -> describe $Describe_a x \
            \++ describe $Describe_a x" ]
    -- ANCHOR_END: classRepl

    -- ANCHOR: classDicts
    -- 「例を :load して、辞書を入れた式を見る」
    it "shows where the dictionaries go" $ do
      containers <- replAfter "examples/classes/containers.lam"
        [":dicts describe (Just [3, 4])"]
      ranked <- replAfter "examples/classes/ranked.lam"
        [ ":dicts \\x -> describe x ++ showInt (rank x)"
        , ":type \\x -> describe x ++ showInt (rank x)" ]
      combine <- replAfter "examples/classes/combine.lam"
        [":dicts total [Yen 1]", ":dicts total [1, 2]", "total []"]
      (containers, ranked, combine) `shouldBe`
        ( [ "describe ($Describe$Maybe ($Describe$List $Describe$Int)) \
            \(Just [3, 4])" ]
        , [ "\\$Ranked_a -> \\x -> describe $Ranked_a.$Describe x \
            \++ showInt (rank $Ranked_a x)"
          , "Ranked a => a -> String" ]
        , [ "total $Combine$Money [Yen 1]", "total $Combine$Int [1, 2]"
          , "type error: cannot choose an instance of Combine: the type a \
            \is not known here (an annotation can tell it)" ] )
    -- ANCHOR_END: classDicts

    -- 「本文の、例を :load してからのやり取り」
    it "answers the REPL lines after loading the examples" $ do
      described <- replAfter "examples/classes/describe.lam"
        [ ":type describe", ":type twice", ":dicts describe (Yen 500)"
        , ":dicts twice (Yen 80)" ]
      contained <- replAfter "examples/classes/containers.lam"
        [":dicts describe [Just (Circle { radius = 1 }), Nothing]"]
      combined <- replAfter "examples/classes/combine.lam"
        [ ":type empty", ":dicts total [Yen 300, Yen 450]"
        , ":dicts combine empty (Minutes 5)", ":dicts total [1, 2, 3]"
        , "total [1, 2, 3]" ]
      (described, contained, combined) `shouldBe`
        ( [ "Describe a => a -> String", "Describe a => a -> String"
          , "describe $Describe$Money (Yen 500)"
          , "twice $Describe$Money (Yen 80)" ]
        , [ "describe ($Describe$List ($Describe$Maybe $Describe$Shape)) \
            \[Just (Circle { radius = 1 }), Nothing]" ]
        , [ "Combine a => a", "total $Combine$Money [Yen 300, Yen 450]"
          , "combine $Combine$Minutes (empty $Combine$Minutes) (Minutes 5)"
          , "total $Combine$Int [1, 2, 3]", "6 : Int" ] )

    -- ANCHOR: byHandProp
    -- 「Lam が入れる辞書は、手で書いた辞書と同じ答えを出す」
    it "gives the same answers as the dictionaries by hand" $
      property $ \(NonNegative r) (NonNegative n) -> ioProperty $ do
        let circle = "(Circle { radius = " <> num r <> " })"
            line   = "(twice " <> circle <> ", twice (Yen " <> num n <> "))"
            byHand = "(twice shapeDict " <> circle <> ", twice moneyDict (Yen "
                       <> num n <> "))"
        withClass <- replAfter "examples/classes/describe.lam" [line]
        withHand  <- replAfter "examples/classes/byhand.lam" [byHand]
        pure (withClass === withHand)
    -- ANCHOR_END: byHandProp

    -- ANCHOR: totalProp
    -- 「total は、Int なら Haskell の sum と同じ。お金も分も、中の数の合計」
    it "adds up as Haskell's sum does" $
      property $ \(NonEmpty xs) -> ioProperty $ do
        outs <- replAfter "examples/classes/combine.lam"
          [ "total " <> renderList xs
          , "total (map Yen " <> lamList xs <> ")" ]
        pure (outs === [ T.pack (show (sum xs)) <> " : Int"
                       , "Yen " <> num (sum xs) <> " : Money" ])
    -- ANCHOR_END: totalProp

    -- ANCHOR: classSelf
    -- 「注釈のない定義も、同じ辞書で自分を呼ぶ。中の let は、辞書の
    --   型変数を1つの型に決める」
    it "calls itself with the same dictionary, and keeps a local let \
       \to one type" $
      runLines
        [ "class Describe a where describe : a -> String"
        , "instance Describe Int where describe n = showInt n"
        , "describeEach xs = case xs of | [] -> \"\" \
          \| x : rest -> describe x ++ describeEach rest"
        , "describeEach [1, 2, 3]"
        , "let f x = describe x in (f 1, f 2)"
        , "let f x = describe x in (f 1, f \"a\")" ]
        `shouldBe`
          [ "describe : Describe a => a -> String", "instance Describe Int"
          , "describeEach : Describe a => [a] -> String", "\"123\" : String"
          , "(\"1\", \"2\") : (String, String)"
          , "type error: String is not in the class Number" ]
    -- ANCHOR_END: classSelf

    -- 「組み込みのクラスも、上位のクラスに書ける（辞書はない）」
    it "takes a builtin class above a declared one" $
      runLines
        [ "class Comparable a => Sorted a where key : a -> Int"
        , "instance Sorted Int where key n = n"
        , "smaller : Sorted a => a -> a -> a; smaller x y = if x < y then x else y"
        , "smaller 3 5", "data H = H (Int -> Int)"
        , "instance Sorted H where key h = 0" ]
        `shouldBe`
          [ "key : Sorted a => a -> Int", "instance Sorted Int"
          , "smaller : Sorted a => a -> a -> a", "3 : Int"
          , "H : (Int -> Int) -> H"
          , "in the instance Sorted H: type error: H is not in the class Comparable (H holds Int -> Int)" ]

    -- ANCHOR: classErrors
    -- 「クラスとインスタンスのまちがいを、そう教える」
    it "says what is wrong with a class or an instance" $
      runLines
        [ "class Describe a where describe : a -> String"
        , "instance Describe Int where describe n = showInt n"
        , "class Describe a where describe : a -> Int"
        , "instance Comparable Int"
        , "instance Describe Int where describe n = \"x\""
        , "instance Describe Bool where descrbe b = \"b\""
        , "instance Describe Bool"
        , "class Sized a where size : Int"
        , "instance Describe (Int -> Int) where describe f = \"f\""
        , "instance Describe (Maybe Int) where describe m = \"m\"" ]
        `shouldBe`
          [ "describe : Describe a => a -> String", "instance Describe Int"
          , "in the class Describe: type error:\
            \ the class Describe is already declared"
          , "in the instance Comparable Int: type error:\
            \ Comparable is a builtin class: the language decides its\
            \ members"
          , "in the instance Describe Int: type error:\
            \ there is already an instance Describe Int"
          , "in the instance Describe Bool: type error:\
            \ the class Describe has no method descrbe"
          , "in the instance Describe Bool: type error:\
            \ the method describe of Describe is not written (and has no\
            \ default)"
          , "in the class Sized: type error:\
            \ the type of the method size must have the class's a"
          , "in the instance Describe (Int -> Int): type error:\
            \ an instance is for a type name with different type\
            \ variables, like (Maybe a), not Int -> Int"
          , "in the instance Describe (Maybe Int): type error:\
            \ an instance is for a type name with different type\
            \ variables, like (Maybe a), not Maybe Int" ]
    -- ANCHOR_END: classErrors

  -- 「トレーニングの解答」
  -- 「Map」
  describe "maps" $ do
    -- ANCHOR: maps
    -- 「Map を作り、引き、比べる」
    it "makes, looks up and compares maps" $
      runLines
        [ "let scores = Map.insert \"TS\" 75 "
            <> "(Map.insert \"Python\" 82 Map.empty)"
        , "scores"
        , "Map.lookup \"TS\" scores"
        , "Map.lookup \"Go\" scores"
        , "{\"TS\": 75, \"Python\": 82} == scores"
        , ":type {}"
        , "{\"a\": 1} < {\"a\": 2}"
        , "{\"a\": toUpper} == {}"
        , "Map.insertWith (\\a b -> a + b) \"the\" 1 {\"the\": 2}"
        , "let name = \"TS\" in {name: 1, [name]: 2}"
        , "{[1]: 2}" ]
        `shouldBe`
          [ "scores : Number a => Map String a\n  -- usually : Map String Int"
          , "{\"Python\": 82, \"TS\": 75} : Map String Int"
          , "Just 75 : Maybe Int"
          , "Nothing : Maybe Int"
          , "true : Bool"
          , "KeyComparable a => Map a b"
          , "true : Bool"
          , "type error: String -> String is not in the class Comparable"
          , "{\"the\": 3} : Map String Int"
          , "{\"TS\": 2, \"name\": 1} : Map String Int"
          , "{1: 2} : Map Int Int" ]
    -- ANCHOR_END: maps

    -- ANCHOR: qualified
    -- 「Map. の付いた名前は、1つの名前。{ } は Map.fromList の糖衣構文」
    it "reads Map.insert as one name, and { } as sugar" $ do
      parseExpr "Map.insert \"a\" 1 m"
        `shouldBe` Right (App (App (App (Var "Map.insert")
                     (Lit (LString "a"))) (Lit (LInt 1))) (Var "m"))
      fmap render (parseExpr "{\"a\": 1, \"b\": x + 1}")
        `shouldBe` Right "Map.fromList [(\"a\", 1), (\"b\", x + 1)]"
      fmap render (parseExpr "{}") `shouldBe` Right "Map.fromList []"
      -- 「名前の形のキーは、" を省ける（変数ではなく、ただの文字列）」
      fmap render (parseExpr "{name: 1, if: 2, \"two words\": 3}")
        `shouldBe` Right
          "Map.fromList [(\"name\", 1), (\"if\", 2), (\"two words\", 3)]"
      -- 「[ ] で包んだキーは、式の値（変数 name の中身）」
      fmap render (parseExpr "{[name]: 1, [toUpper k]: 2}")
        `shouldBe` Right "Map.fromList [(name, 1), (toUpper k, 2)]"
    -- ANCHOR_END: qualified

    -- ANCHOR: mapLikeHaskell
    -- 「Map.fromList は、Haskell の Map.fromList と同じ答え」
    --  (the last of the same keys wins, and the keys come out in order)
    it "builds the same map as Haskell's Map.fromList" $
      property $ forAll (listOf1 genPair) $ \kvs ->
        let source = "Map.toList (Map.fromList " <> lamPairs kvs <> ")"
        in runLines [source]
             == [lamPairs (Map.toList (Map.fromList kvs))
                   <> " : [(String, Int)]"]
    -- ANCHOR_END: mapLikeHaskell

    -- ANCHOR: anyKeys
    -- 「キーは、Comparable な型なら何でもよい」
    it "takes keys of any Comparable type" $
      runLines
        [ "{1: \"one\", 2: \"two\"}"
        , "Map.lookup (1, 2) {[(1, 2)]: \"a\", [(0, 5)]: \"b\"}"
        , ":type Map.insert"
        , "{-1: 2, 1.5: 3}"
        , "Map.fromList [(Just 2, 1), (Nothing, 0)]"
        , "{[toUpper]: 1}"
        , "{1: 2, \"a\": 3}" ]
        `shouldBe`
          [ "{1: \"one\", 2: \"two\"} : Map Int String"
          , "Just \"a\" : Maybe String"
          , "KeyComparable a => a -> b -> Map a b -> Map a b"
          , "{-1: 2, 1.5: 3} : Map Float Int"
          , "{[Nothing]: 0, [Just 2]: 1} : Map (Maybe Int) Int"
          , "type error: String -> String is not in the class KeyComparable"
          , "type error: String is not in the class Number" ]
    -- ANCHOR_END: anyKeys

    -- ANCHOR: sets
    -- 「Set：どの値も1つずつ、順に並べて持つ」
    it "keeps each value once, in order, in a set" $
      runLines
        [ "Set.fromList [3, 1, 3, 2]"
        , "Set.member 2 (Set.fromList [1, 2])"
        , "Set.union (Set.fromList [1, 2]) (Set.fromList [2, 3])"
        , "Set.intersection (Set.fromList [1, 2]) (Set.fromList [2, 3])"
        , "Set.difference (Set.fromList [1, 2]) (Set.fromList [2, 3])"
        , "Set.toList (Set.insert (0, 0) Set.empty)"
        , "Just (Set.fromList [\"a\"])"
        , "Set.fromList [1, 2] == Set.fromList [2, 1, 2]"
        , ":type Set.fromList"
        , "Set.fromList [toUpper]" ]
        `shouldBe`
          [ "Set.fromList [1, 2, 3] : Set Int"
          , "true : Bool"
          , "Set.fromList [1, 2, 3] : Set Int"
          , "Set.fromList [2] : Set Int"
          , "Set.fromList [1] : Set Int"
          , "[(0, 0)] : [(Int, Int)]"
          , "Just (Set.fromList [\"a\"]) : Maybe (Set String)"
          , "true : Bool"
          , "KeyComparable a => [a] -> Set a"
          , "type error: String -> String is not in the class KeyComparable" ]

    -- 「Set.fromList は、Haskell の Set.fromList と同じ答え」
    it "builds the same set as Haskell's Set.fromList" $
      property $ \(NonEmpty (xs :: [Integer])) ->
        let shown ns = "[" <> T.intercalate ", " (map (T.pack . show) ns)
                         <> "]"
        in runLines ["Set.toList (Set.fromList " <> lamList xs <> ")"]
             === [shown (Set.toList (Set.fromList xs)) <> " : [Int]"]
    -- ANCHOR_END: sets

    -- ANCHOR: mapTypes
    -- 「型の注釈と data の宣言にも、Map と Set を書ける」
    it "writes Map and Set in annotations and data" $
      runLines
        [ "count : [String] -> Map String Int; count ws = "
            <> "foldl (\\m w -> Map.insertWith (+) w 1 m) {} ws"
        , "count [\"a\", \"b\", \"a\"]"
        , "data Board = Board (Set (Int, Int))"
        , "Board (Set.fromList [(1, 2)])"
        , "data T = C (Map Int)" ]
        `shouldBe`
          [ "count : [String] -> Map String Int"
          , "{\"a\": 2, \"b\": 1} : Map String Int"
          , "Board : Set (Int, Int) -> Board"
          , "Board (Set.fromList [(1, 2)]) : Board"
          , "in the data type T: type error: the type Map needs 2 \
            \arguments, but got 1" ]
    -- ANCHOR_END: mapTypes

    -- ANCHOR: nanKeys
    -- 「NaN のキーも、また引ける。== は IEEE どおり」
    it "finds a NaN key again" $
      runLines
        [ "nan = 0 / 0"
        , "m = Map.insert nan \"a\" Map.empty"
        , "Map.lookup nan m"
        , "Map.insert nan \"b\" m"
        , "Set.fromList [nan, nan, 1.5, -1 / 0]"
        , "Set.member (Just nan) (Set.fromList [Just nan])"
        , "nan == nan"
        , "f : Comparable k => k -> Map k Int; f k = Map.insert k 1 {}"
        , "g k = (k < k, Map.insert k 1 {})" ]
        `shouldBe`
          [ "nan : Float"
          , "m : Map Float String"
          , "Just \"a\" : Maybe String"
          , "{NaN: \"b\"} : Map Float String"
          , "Set.fromList [-Infinity, 1.5, NaN] : Set Float"
          , "true : Bool"
          , "false : Bool"
          , "f : Comparable a => a -> Map a Int"
          , "g : (Comparable a, Number b) => a -> (Bool, Map a b)" ]

    -- 「キーの比べ方は、NaN のほかは == と < と同じ」
    it "compares keys as == and < do, but for NaN" $
      property $ \(x :: Double) (y :: Double) ->
        let lit v = "(" <> floatLiteral v <> ")"
        in runLines
             [ "Set.toList (Set.fromList [" <> lit x <> ", " <> lit y <> "])"
                 <> " == (if " <> lit y <> " < " <> lit x
                 <> " then [" <> lit y <> ", " <> lit x <> "]"
                 <> " else if " <> lit x <> " == " <> lit y
                 <> " then [" <> lit x <> "]"
                 <> " else [" <> lit x <> ", " <> lit y <> "])" ]
             === ["true : Bool"]
    -- ANCHOR_END: nanKeys

    -- ANCHOR: usualTypes
    -- 「どの型にもなれる数のある値には、ふつうの型（Int）を添える。
    --   関数は1行のまま」
    it "shows the usual type of a value with numbers of any type" $
      runLines
        [ "treasures = {[(2, 0)]: \"herb\", [(1, 1)]: \"elixir\"}"
        , ":type treasures"
        , "double x = x * 2"
        , "names = {1: \"one\"}"
        , "words = {\"a\": \"b\"}" ]
        `shouldBe`
          [ "treasures : (Number a, KeyComparable a, Number b, \
            \KeyComparable b) => Map (a, b) String\n\
            \  -- usually : Map (Int, Int) String"
          , "(Number a, KeyComparable a, Number b, KeyComparable b) \
            \=> Map (a, b) String\n  -- usually : Map (Int, Int) String"
          , "double : Number a => a -> a"
          , "names : (Number a, KeyComparable a) => Map a String\n\
            \  -- usually : Map Int String"
          , "words : Map String String" ]
    -- ANCHOR_END: usualTypes

    -- ANCHOR: braceError
    -- 「どちらでもないときは、: と = のどちらが来るはずかを言う」
    it "says : or = is needed after a name in { }" $
      runLines ["{name}"]
        `shouldBe`
          [ "<repl>:1:6:\n  |\n1 | {name}\n  |      ^\nunexpected '}'\n\
            \expecting ':' (a Map) or '=' (a record)\n" ]
    -- ANCHOR_END: braceError

    -- 「レコードと Map の { } は、= と : で見分ける」
    it "tells a record from a map by = and :" $
      runLines
        [ "{ name = 1 }", "{ name: 1 }", "{}", "r = { name = 1 }"
        , "{ r | name = 2 }" ]
        `shouldBe`
          [ "{ name = 1 } : { name : Int }"
          , "{\"name\": 1} : Map String Int"
          , "{} : KeyComparable a => Map a b"
          , "r : Number a => { name : a }\n  -- usually : { name : Int }"
          , "{ name = 2 } : { name : Int }" ]

  -- 「JSON」
  describe "JSON" $ do
    -- ANCHOR: preludeCons
    -- 「Haskell が作る構成子の番号は、プレリュードの宣言と同じ」
    it "numbers the constructors as the prelude declares them" $
      [ (c, conIndex <$> Map.lookup c table)
      | let table = Map.unions
              [ constructors (moduleData m)
              | m <- Map.elems (sessionModules preludeSession) ]
      , (c, _) <- preludeConstructors ]
        `shouldBe` [ (c, Just i) | (c, i) <- preludeConstructors ]
    -- ANCHOR_END: preludeCons

    -- ANCHOR: parseJson
    -- 「JSON を読み、Lam の Json にする」
    it "reads JSON into Lam's Json" $
      runLines
        [ "Json.parse \"{\\\"name\\\": \\\"Lam\\\", "
            <> "\\\"tags\\\": [true, null]}\""
        , "Json.parse \"oops\""
        , "Json.parse \"[1] x\""
        , ":type Json.parse" ]
        `shouldBe`
          [ "Ok (JObj {\"name\": JStr \"Lam\", "
              <> "\"tags\": JArr [JBool true, JNull]}) : Result String Json"
          , "Err \"Failed reading: not a valid json value\""
              <> " : Result String Json"
          , "Err \"Failed reading: extra text after the JSON value\""
              <> " : Result String Json"
          , "String -> Result String Json" ]

    -- 「同じキーが2つあれば、後のほうが勝つ（Lam の { } と同じ）」
    it "keeps the last of the same keys, like { } in Lam" $
      runLines
        [ "Json.parse \"{\\\"a\\\": 1, \\\"a\\\": 2}\""
        , "{\"a\": JInt 1, \"a\": JInt 2}" ]
        `shouldBe`
          [ "Ok (JObj {\"a\": JInt 2}) : Result String Json"
          , "{\"a\": JInt 2} : Map String Json" ]
    -- ANCHOR_END: parseJson

    -- ANCHOR: numbers
    -- 「数は、整数なら JInt、小数なら JDec。どちらもぴったり」
    it "reads whole numbers as JInt, and fractions as JDec, exactly" $
      runLines
        [ "Json.parse \"[42, 19.99, -0.5, 1.0, 1e3, 12345678901234567890]\""
        , "Json.show (JArr [Json.mkDec 1999 2, Json.mkDec (0 - 5) 1, JInt 1000])" ]
        `shouldBe`
          [ "Ok (JArr [JInt 42, JDec 1999 2, JDec (-5) 1, JInt 1, JInt 1000, "
              <> "JInt 12345678901234567890]) : Result String Json"
          , "\"[19.99,-0.5,1000]\" : String" ]

    -- 「とても大きな指数でも、桁を並べない（2秒で終わる）」
    it "does not spell out the digits of a very large exponent" $ do
      let lines' = runLines
            [ "Json.parse \"1e1000000000\""
            , "Json.show (Json.mkDec 1 (0 - 1000000000))" ]
      done <- timeout 2000000 (E.evaluate (sum (map T.length lines')))
      fmap (const lines') done `shouldBe` Just
        [ "Ok (JDec 1 (-1000000000)) : Result String Json"
        , "\"1.0e1000000000\" : String" ]
    -- ANCHOR_END: numbers

    -- ANCHOR: floatJson
    -- 「JSON の数と Float を行き来する」
    it "turns JSON numbers into Floats, and back" $
      runLines
        [ "Json.toFloat (Json.mkDec 1999 2)", "Json.toFloat (JInt 3)"
        , "Json.toFloat (JStr \"3\")"
        , "Json.fromFloat 0.1", "Json.fromFloat 2.0"
        , "Json.fromFloat (0 / 0)", "Json.fromFloat (1 / 0)"
        , "Json.show (Json.fromFloat (0.1 + 0.2))" ]
        `shouldBe`
          [ "Just 19.99 : Maybe Float", "Just 3 : Maybe Float"
          , "Nothing : Maybe Float"
          , "JDec 1 1 : Json", "JInt 2 : Json"
          , "JNull : Json", "JNull : Json"
          , "\"0.30000000000000004\" : String" ]

    -- 「Json.fromFloat を Json.toFloat で戻すと、同じ Float」
    it "turns a Float into JSON and back" $
      property $ \x ->
        runLines ["Json.toFloat (Json.fromFloat (" <> floatLiteral x <> "))"]
          === ["Just " <> (if x < 0 then "(" <> showFloat x <> ")"
                           else showFloat x) <> " : Maybe Float"]
    -- ANCHOR_END: floatJson

    -- ANCHOR: mkDec
    -- 「Json.mkDec は、Json.parse と同じ形の小数を作る」
    it "makes a number in the same shape as Json.parse" $
      runLines
        [ "Json.decimal (Json.mkDec 19990 3)"
        , "Json.mkDec 19990 3", "Json.mkDec 1900 2", "Json.mkDec 0 5"
        , "Json.mkDec 1999 2 == Json.mkDec 19990 3" ]
        `shouldBe`
          [ "Just (1999, 2) : Maybe (Int, Int)"
          , "JDec 1999 2 : Json", "JInt 19 : Json", "JInt 0 : Json"
          , "true : Bool" ]

    -- 「どんな c と d でも、Haskell が書いた数 c × 10^(-d) を読んだものと同じ」
    -- （JDec は隠したので、Lam では書けない）
    it "makes the same Json as reading the number c * 10^(-d)" $
      property $ forAll ((,) <$> arbitrary <*> choose (-6, 8)) $
        \(c, d) ->
          let lam n = if n < 0 then "(0 - " <> T.pack (show (abs n)) <> ")"
                               else T.pack (show (n :: Integer))
              written = T.pack (show (scientific c (negate d)))
          in runLines
               [ "Json.mkDec " <> lam c <> " " <> lam (toInteger d)
                   <> " == (case Json.parse \"" <> written
                   <> "\" of | Ok j -> j | Err _ -> JNull)" ]
               === ["true : Bool"]
    -- ANCHOR_END: mkDec

    -- ANCHOR: roundTrip
    -- 「aeson の値を Lam の Json にして戻すと、もとの値」
    it "turns aeson's values into Lam's Json and back" $
      property $ forAll genAeson $ \j ->
        toAeson (fromAeson j) == Right j
    -- ANCHOR_END: roundTrip

    -- ANCHOR: jsonTools
    -- 「奥の値を、道すじで取り出す」
    it "takes a value from deep inside, by its path" $
      runLines
        [ "let doc = JObj {\"user\": "
            <> "JObj {\"name\": JStr \"Lam\", \"age\": JInt 3}}"
        , "Json.at [\"user\", \"name\"] doc"
        , "Maybe.andThen Json.int (Json.at [\"user\", \"age\"] doc)"
        , "Maybe.andThen Json.int (Json.at [\"user\", \"name\"] doc)"
        , "Json.at [\"user\", \"email\"] doc"
        , "Json.at [] (JInt 1)" ]
        `shouldBe`
          [ "doc : Json"
          , "Just (JStr \"Lam\") : Maybe Json"
          , "Just 3 : Maybe Int"
          , "Nothing : Maybe Int"
          , "Nothing : Maybe Json"
          , "Just (JInt 1) : Maybe Json" ]
    -- ANCHOR_END: jsonTools

  -- 「道具箱」
  describe "toolbox" $ do
    -- ANCHOR: toolboxStrings
    -- 「文字列の道具」
    it "trims, splits and replaces strings" $
      runLines
        [ "String.trim \"  Lam  \""
        , "String.split \",\" \"a,,b\""
        , "\" TS , ts@lam.test \" |> String.split \",\" |> map String.trim"
        , "String.startsWith \"#\" \"# note\""
        , "String.contains \"@\" \"ruby-at-lam.test\""
        , "String.replace \"-at-\" \"@\" \"ruby-at-lam.test\""
        , "String.repeat 3 \"ab\""
        , "String.split \"\" \"abc\"" ]
        `shouldBe`
          [ "\"Lam\" : String"
          , "[\"a\", \"\", \"b\"] : [String]"
          , "[\"TS\", \"ts@lam.test\"] : [String]"
          , "true : Bool"
          , "false : Bool"
          , "\"ruby@lam.test\" : String"
          , "\"ababab\" : String"
          , "[\"a\", \"b\", \"c\"] : [String]" ]
    -- 「区切りが空でなければ、Haskell と同じ。区切りでつなぎ直すと元に戻る」
    it "splits as Haskell's splitOn does, and join undoes it" $
      property $ forAll ((,) <$> elements ["", ",", ", ", "ab"] <*> genText) $
        \(sep, text) ->
          runLines [ "String.split " <> renderString sep <> " "
                       <> renderString text ]
          === [ "[" <> T.intercalate ", "
                         (map renderString (splitOn sep text))
                  <> "] : [String]" ]
          .&&. (T.null sep || T.intercalate sep (splitOn sep text) == text)
          .&&. (T.null sep || splitOn sep text == T.splitOn sep text)
    -- ANCHOR_END: toolboxStrings

    -- ANCHOR: toolboxSequence
    -- 「reverse・take・slice は、文字列にもリストにも」
    it "reverses, takes and slices strings and lists alike" $
      runLines
        [ "reverse \"abc\""
        , "reverse [1, 2, 3]"
        , "take 3 \"Haskell\""
        , "drop 1 [1, 2, 3]"
        , "slice 1 (-1) \"Lam!\""
        , "slice 1 (-1) [1, 2, 3, 4]"
        , "let palindrome s = s == reverse s"
        , "map palindrome [\"level\", \"lam\"]"
        , "palindrome [1, 2, 1]"
        , "reverse (Just 1)" ]
        `shouldBe`
          [ "\"cba\" : String"
          , "[3, 2, 1] : [Int]"
          , "\"Has\" : String"
          , "[2, 3] : [Int]"
          , "\"am\" : String"
          , "[2, 3] : [Int]"
          , "palindrome : (Comparable a, Sequence a) => a -> Bool"
          , "[true, false] : [Bool]"
          , "true : Bool"
          , "type error: Maybe a is not in the class Sequence" ]
    -- 「slice は Python の s[i:j] と同じ（負の数は後ろから）」
    it "slices as Python's s[i:j] does" $
      property $ \(i, j) -> forAll genText $ \text ->
        let len = T.length text
            place k = max 0 (min len (if k < 0 then len + k else k))
            python = T.drop (place i) (T.take (place j) text)
        in runLines [ "slice " <> num (toInteger (i :: Int)) <> " "
                        <> num (toInteger (j :: Int)) <> " "
                        <> renderString text ]
           === [ renderString python <> " : String" ]
    -- 「文字列の reverse は Haskell と同じ」
    it "reverses a string as Haskell does" $
      property $ forAll genText $ \text ->
        runLines ["reverse " <> renderString text]
          === [renderString (T.reverse text) <> " : String"]
    -- ANCHOR_END: toolboxSequence

    -- ANCHOR: toolboxListMax
    -- 「リストの版：空の maximum は Nothing」
    it "gives Nothing for the maximum of an empty list" $
      runLines [ "List.maximum [95, 88, 79]", "List.maximum []"
               , ":type List.maximum" ]
        `shouldBe` [ "Just 95 : Maybe Int"
                   , "Nothing : Comparable a => Maybe a"
                   , "Comparable a => [a] -> Maybe a" ]
    -- ANCHOR_END: toolboxListMax

    -- ANCHOR: toolboxKinds
    -- 「f a の型を書き、穴のある型だけを instance にする」
    it "writes f a in types, and checks the holes of an instance" $
      runLines
        [ ":type toList"
        , ":type foldr"
        , "instance Foldable Int where foldr f z x = z"
        , "class Box f where unbox : f a -> a"
        , "instance Box Maybe where unbox m = m ?: unbox m"
        , "instance Box (Maybe a) where unbox m = m"
        , "class Size a where size : a -> Int"
        , "instance Size Maybe where size m = 1"
        , "instance Size (Maybe a) where size m = 1"
        , "instance Size (a, b) where size p = 2" ]
        `shouldBe`
          [ "Foldable f => f a -> [a]"
          , "Foldable f => (a -> b -> b) -> b -> f a -> b"
          , "in the instance Foldable Int: type error: Foldable is for a type"
              <> " with a hole (one that takes 1 more type, like List or"
              <> " Maybe), not Int"
          , "unbox : Box f => f a -> a"
          , "instance Box Maybe"
          , "in the instance Box (Maybe a): type error: Box is for a type"
              <> " with a hole (one that takes 1 more type, like List or"
              <> " Maybe), not Maybe a"
          , "size : Size a => a -> Int"
          , "in the instance Size Maybe: type error: Size is for a type"
              <> " with no hole (one that takes 0 more types, like Int or"
              <> " (Maybe a)), not Maybe"
          , "instance Size (Maybe a)"
          , "instance Size (a, b)" ]
    -- ANCHOR_END: toolboxKinds

    -- ANCHOR: toolboxFoldable
    -- 「Foldable：リストにも Map にも Maybe にも Set にも」
    it "folds lists, maps, Maybes and sets alike" $
      runLines
        [ ":type any"
        , "any even [1, 3, 4]"
        , "sum {python: 82, ts: 75}"
        , "maximum {python: 82, ts: 75}"
        , "maximum []"
        , "sum (Just 3)"
        , "elem \"Lam\" (Just \"Lam\")"
        , "toList {b: 2, a: 1}"
        , "sum (Set.fromList [3, 1, 3])"
        , "sum 5"
        , "any even \"abc\"" ]
        `shouldBe`
          [ "Foldable f => (a -> Bool) -> f a -> Bool"
          , "true : Bool"
          , "157 : Int"
          , "Just 82 : Maybe Int"
          , "Nothing : Comparable a => Maybe a"
          , "3 : Int"
          , "true : Bool"
          , "[1, 2] : [Int]"
          , "4 : Int"
          , "type error: the types do not fit: Int where f a is needed"
          , "type error: the types do not fit: String where f Int is needed" ]
    -- 「maximum は、Haskell の maximum と同じ。空なら Nothing」
    it "finds the same maximum as Haskell, and Nothing for []" $
      property $ \xs ->
        map valuePart
          (runLines ["maximum " <> lamList xs, "minimum " <> lamList xs])
          === map valuePart
                [ lamMaybe (if null xs then Nothing else Just (maximum xs))
                , lamMaybe (if null xs then Nothing else Just (minimum xs)) ]
    -- 「Map の値は、キーの順にたたむ（引き算で順番まで確かめる）」
    it "folds the values of a map in the order of the keys" $
      property $ \kvs ->
        let m = Map.fromList [ (k :: Int, v :: Integer) | (k, v) <- kvs ]
            lit = "(Map.fromList [" <> T.intercalate ", "
                    [ "(" <> num (toInteger k) <> ", " <> num v <> ")"
                    | (k, v) <- Map.toList m ] <> "])"
        in map valuePart
             (runLines [ "sum " <> lit, "foldr (\\x acc -> x - acc) 0 " <> lit ])
           === [ T.pack (show (sum m)), T.pack (show (foldr (-) 0 m)) ]
    -- ANCHOR_END: toolboxFoldable

    -- ANCHOR: toolboxPending
    -- 「f がわからないうちの条件は待ち、最後まで決まらなければリストに」
    it "waits for f, and makes it a list when it is never known" $
      runLines
        [ "let total t = sum t"
        , "let average xs = sum xs / toFloat (length xs)"
        , "average [80, 90]"
        , "let grow t = map (\\x -> x + 1) t ++ [0]"
        , "(\\t -> length (map (\\x -> x) t)) (Just 1)" ]
        `shouldBe`
          [ "total : (Foldable f, Number a) => f a -> a"
          , "average : [Float] -> Float"
          , "85 : Float"
          , "grow : Number a => [a] -> [a]"
          , "type error: Maybe a is not in the class HasLength" ]
    -- ANCHOR_END: toolboxPending

    -- ANCHOR: toolboxFunctor
    -- 「Functor：map はリストにも Maybe にも Map にも Result にも」
    it "maps lists, Maybes, maps and Results alike" $
      runLines
        [ ":type map"
        , "map (\\n -> n * 2) [1, 2]"
        , "map (\\n -> n * 2) (Just 4)"
        , "map (\\n -> n * 2) {a: 1, b: 2}"
        , "map (\\n -> n * 2) (Ok 3)"
        , "map (\\n -> n * 2) (Err \"no\")"
        , "map toUpper \"abc\""
        , ":dicts map (\\n -> n * 2) (Just 4)" ]
        `shouldBe`
          [ "Functor f => (a -> b) -> f a -> f b"
          , "[2, 4] : [Int]"
          , "Just 8 : Maybe Int"
          , "{\"a\": 2, \"b\": 4} : Map String Int"
          , "Ok 6 : Result a Int"
          , "Err \"no\" : Result String Int"
          , "type error: the types do not fit: String where f String"
              <> " is needed"
          , "map $Functor$Maybe (\\n -> n * 2) (Just 4)" ]
    -- ANCHOR_END: toolboxFunctor

    -- ANCHOR: toolboxFallible
    -- 「?: は Fallible の toMaybe で開く。右は要るときだけ計算する」
    it "opens ?: with toMaybe, and computes the right only when needed" $
      runLines
        [ ":type \\m -> m ?: 0"
        , "Just 3 ?: (1 // 0)"
        , "Err \"no\" ?: 4"
        , "3 ?: 0"
        , "[1] ?: 0"
        , ":dicts Just 3 ?: 0" ]
        `shouldBe`
          [ "(Fallible f, Number a) => f a -> a"
          , "3 : Int"
          , "4 : Int"
          , "type error: ?: needs a Maybe or a Result on its left, but got"
              <> " a number (only a Maybe or a Result can be missing its"
              <> " value)"
          , "type error: ?: needs a Maybe or a Result on its left, but got"
              <> " [a] (only a Maybe or a Result can be missing its value)"
          , "case toMaybe $Fallible$Maybe (Just 3) of | Just $x -> $x"
              <> " | Nothing -> 0" ]
    -- ANCHOR_END: toolboxFallible

    -- ANCHOR: toolboxOwn
    -- 「自分の data の型も、Foldable・Functor・Fallible に入る」
    it "puts our own data types into the classes of the toolbox" $ do
      team <- replAfter "examples/stdlib/team.lam"
        [ "maximum (Team 1 3 2)", ":dicts sum (Team 1 2 3)" ]
      skipped <- replAfter "examples/stdlib/skipped.lam"
        [ ":type toMaybe", "Answered \"a\" ?: \"none\"" ]
      (team, skipped) `shouldBe`
        ( [ "Just 3 : Maybe Int", "sum $Foldable$Team (Team 1 2 3)" ]
        , [ "Fallible f => f a -> Maybe a", "\"a\" : String" ] )
    -- ANCHOR_END: toolboxOwn

    -- ANCHOR: toolboxLists
    -- 「リストの道具は、Haskell の zip・nub・partition・sortOn と同じ」
    it "zips, numbers, splits, sorts and makes unique as Haskell does" $
      property $ \xs ys ->
        let shown zs = "[" <> T.intercalate ", " (map (T.pack . show) zs)
                         <> "]"
        in map valuePart
             (runLines [ "zip " <> lamList xs <> " " <> lamList ys
                       , "unique " <> lamList xs
                       , "partition even " <> lamList xs
                       , "map fst (indexed " <> lamList xs <> ")"
                       , "sortOn negate " <> lamList xs ])
           === [ "[" <> T.intercalate ", "
                   [ "(" <> T.pack (show a) <> ", " <> T.pack (show b) <> ")"
                   | (a, b) <- zip xs ys ] <> "]"
               , shown (nub xs)
               , "(" <> shown (filter even xs) <> ", "
                   <> shown (filter odd xs) <> ")"
               , shown (map toInteger [0 .. length xs - 1])
               , shown (sortOn negate xs) ]
    -- 「sortOn は、同じキーの2つの順を変えない」
    it "keeps the order of the items with the same key" $
      property $ \(kvs :: [(Integer, Integer)]) ->
        let lit = "[" <> T.intercalate ", "
                    [ "(" <> num k <> ", " <> num v <> ")" | (k, v) <- kvs ]
                    <> "]"
            shownPairs ps = "[" <> T.intercalate ", "
              [ "(" <> T.pack (show k) <> ", " <> T.pack (show v) <> ")"
              | (k, v) <- ps ] <> "]"
        in map valuePart (runLines ["sortOn fst " <> lit])
             === [shownPairs (sortOn fst kvs)]
    -- ANCHOR_END: toolboxLists

    -- ANCHOR: toolboxMaps
    -- 「Map の道具は、Haskell の Map と同じ（unionWith は引数の順も）」
    it "unions, filters and updates maps as Haskell's Map does" $
      property $ \as bs k ->
        let a = Map.fromList [ (key :: Int, v :: Integer) | (key, v) <- as ]
            b = Map.fromList [ (key, v) | (key, v) <- bs ]
            lit m = "(Map.fromList [" <> T.intercalate ", "
                      [ "(" <> num (toInteger x) <> ", " <> num v <> ")"
                      | (x, v) <- Map.toList m ] <> "])"
            shown m = "{" <> T.intercalate ", "
                        [ T.pack (show x) <> ": " <> T.pack (show v)
                        | (x, v) <- Map.toList m ] <> "}"
            bump n = case n of
              Just v | v > 0 -> Just (v - 1)
              _              -> Nothing
        in map valuePart (runLines
             [ "Map.unionWith (\\x y -> x - y) " <> lit a <> " " <> lit b
             , "Map.filter (\\v -> v > 0) " <> lit a
             , "Map.update " <> num (toInteger (k :: Int))
                 <> " (\\n -> case n of | Just v -> if v > 0"
                 <> " then Just (v - 1) else Nothing | Nothing -> Nothing) "
                 <> lit a
             , "Map.union " <> lit a <> " " <> lit b
             , "Map.map (\\v -> v * 2) " <> lit a ])
           === map shown [ Map.unionWith (-) a b, Map.filter (> 0) a
                         , Map.alter bump k a, Map.union a b
                         , Map.map (* 2) a ]
    -- ANCHOR_END: toolboxMaps

    -- ANCHOR: toolboxPretty
    -- 「Json.pretty は、何行かに分けて2つずつ字下げする。読み戻すと同じ」
    it "writes JSON on many lines, and parse reads it back" $
      runLines
        [ "Json.pretty (JArr [JInt 1, JStr \"a\"])"
        , "Json.parse (Json.pretty (JObj {b: JNull, a: JBool true}))" ]
        `shouldBe`
          [ "\"[\\n  1,\\n  \\\"a\\\"\\n]\" : String"
          , "Ok (JObj {\"a\": JBool true, \"b\": JNull})"
              <> " : Result String Json" ]
    -- ANCHOR_END: toolboxPretty

    -- ANCHOR: quizRepl
    -- 「本文の、例を :load してからのやり取り」
    it "answers the REPL lines after loading the examples" $ do
      signup <- replAfter "examples/stdlib/signup.lam"
        [ "cells \"  Python , python@lam.test , Python\""
        , "fixMail \"ruby-at-lam.test\"", "map .mail entries" ]
      quiz <- replAfter "examples/stdlib/quiz.lam"
        [ "best [95, 88, 79]", "best []", "totals", "bestTotal", "allPoints"
        , "passedNames", "notYet", "map best book", "notice" ]
      tally <- replAfter "examples/stdlib/tally.lam" ["map showCounts days"]
      (signup, quiz, tally) `shouldBe`
        ( [ "[\"Python\", \"python@lam.test\", \"Python\"] : [String]"
          , "\"ruby@lam.test\" : String"
          , "[\"python@lam.test\", \"ts@lam.test\", \"rust@lam.test\","
              <> " \"ruby@lam.test\"] : [String]" ]
        , [ "\"95\" : String", "\"-\" : String"
          , "{\"Erlang\": 156, \"Python\": 247, \"Ruby\": 0, \"Rust\": 262,"
              <> " \"TS\": 159} : Map String Int"
          , "262 : Int", "824 : Int"
          , "[\"Python\", \"Rust\", \"TS\"] : [String]"
          , "[\"Erlang\", \"Ruby\"] : [String]"
          , "{\"Erlang\": \"61\", \"Python\": \"90\", \"Ruby\": \"-\","
              <> " \"Rust\": \"95\", \"TS\": \"91\"} : Map String String"
          , "\"{\\n  \\\"Erlang\\\": 156,\\n  \\\"Python\\\": 247,\\n  \\\"Ruby"
              <> "\\\": 0,\\n  \\\"Rust\\\": 262,\\n  \\\"TS\\\": 159\\n}\" : String" ]
        , [ "[\"Haskell 1, Lam 2, Python 1\", \"Go 1, Haskell 2, Lam 1,"
              <> " Rust 1\"] : [String]" ] )
    -- ANCHOR_END: quizRepl

  -- 「値を埋めこむ書式」
  describe "f-strings" $ do
    -- ANCHOR: formatSugar
    -- 「f"..." は、++ と関数の呼び出しに開く」
    -- (The names it calls are hidden ones, like $fmt.display: see the
    -- tests of the modules. Here, without $fmt.)
    -- （呼ぶ名前は $fmt.display のような隠した名前。ここでは $fmt. を外す）
    it "opens f\"...\" into ++ and calls" $
      map (fmap (T.replace "$fmt." "" . render) . parseExpr)
        [ "f\"Hello, {name}!\""
        , "f\"{n:>8,} {n:05}\""
        , "f\"{x:.1} {x:>9,.2}\""
        , "f\"{x=} {x = :^6}\""
        , "f\"{{}} {f\"{x}\"}\""
        , "f\"\"" ]
        `shouldBe` map Right
          [ "\"Hello, \" ++ display name ++ \"!\""
          , "padLeft 8 (groupDigits (showNumber n)) ++ \" \""
              <> " ++ zeroPad 5 (showNumber n)"
          , "Float.fixed 1 x ++ \" \" ++ padLeft 9 (groupDigits"
              <> " (Float.fixed 2 x))"
          , "\"x=\" ++ show x ++ \" x = \" ++ center 6 (display x)"
          , "\"{} \" ++ display (display x)"
          , "\"\"" ]
    -- ANCHOR_END: formatSugar

    -- ANCHOR: formatValues
    -- 「値を埋めこみ、桁をそろえる（Python と同じ答え）」
    it "puts values in, and lines them up like Python" $
      runLines
        [ "name = \"Lam\""
        , "f\"[{name:<6}] [{name:>6}] [{name:^6}] [{name:^5}]\""
        , "f\"{1234567:,} {-1234:,} {42:05} {-42:05} {123456:03}\""
        , "f\"{name=} {[name]} {Just (1, true)} {{\""
        , "f\"{name:>2}|{\"日本\":>4}|\""
        , "f\"{2.5:.2} {-0.5:.1} {1234567.891:,.2} {3.14159:08.3}\"" ]
        `shouldBe`
          [ "name : String"
          , "\"[Lam   ] [   Lam] [ Lam  ] [ Lam ]\" : String"
          , "\"1,234,567 -1,234 00042 -0042 123456\" : String"
          , "\"name=\\\"Lam\\\" [\\\"Lam\\\"] Just (1, true) {\" : String"
          , "\"Lam|  日本|\" : String"
          , "\"2.50 -0.5 1,234,567.89 0003.142\" : String" ]
    -- ANCHOR_END: formatValues

    -- ANCHOR: formatErrors
    -- 「書式のまちがいは、動かす前にパーサーが見つける」
    it "finds the mistakes of a format before running" $
      map (fmap (const ()) . parseExpr)
        [ "f\"{x:>}\"", "f\"{x:8}\"", "f\"{x:>05}\"", "f\"{x:05,}\""
        , "f\"{x:>8d}\"", "f\"{x:.}\"", "f\"{x:.1f}\"", "f\"{x : xs}\""
        , "f\"{\\y -> y}\"", "f\"a } b\"" ]
        `shouldSatisfy` all isLeft
    -- ANCHOR_END: formatErrors

    -- 「エラーの文が、まちがいの場所を言う」
    it "says what is wrong with a format, and where" $
      runLines ["f\"{x:>}\"", "f\"{x:8}\"", "f\"{x:.2f}\"", "f\"a } b\""]
        `shouldBe`
          [ "<repl>:1:7:\n  |\n1 | f\"{x:>}\"\n  |       ^\n\
            \a width must follow < > or ^\n"
          , "<repl>:1:6:\n  |\n1 | f\"{x:8}\"\n  |      ^\n\
            \a width needs < > or ^ before it (or 0, for a number)\n"
          , "<repl>:1:8:\n  |\n1 | f\"{x:.2f}\"\n  |        ^\n\
            \a format has no type letter like f (write {x:.1}, not {x:.1f})\n"
          , "<repl>:1:5:\n  |\n1 | f\"a } b\"\n  |     ^\n\
            \a single } must be written }}\n" ]

    -- ANCHOR: formatTypes
    -- 「埋めこめない値と、数にしか使えない書式は、型のエラー」
    it "gives a type error for values that cannot be shown" $
      runLines
        [ ":type show"
        , ":type display"
        , "f\"{\\n -> n}\""
        , "f\"{(\\n -> n)}\""
        , "f\"{\"Lam\":,}\""
        , "f\"{\"Lam\":.1}\""
        , "data Handler = Button (Int -> Int)"
        , "show (Button (\\n -> n))"
        , "instance Display Handler"
        , "data Box a = Box a"
        , "f\"{Box 1}\""
        , "show (Box {a: [1]})" ]
        `shouldBe`
          [ "Showable a => a -> String"
          , "Display a => a -> String"
          , "<repl>:1:4:\n  |\n1 | f\"{\\n -> n}\"\n  |    ^\n\
            \inside { }, put \\, let, if and case in ( )\n"
          , "type error: a -> a is not in the class Display"
          , "type error: String is not in the class Number"
          , "type error: String is not in the class Number"
          , "Button : (Int -> Int) -> Handler"
          , "type error: Handler is not in the class Showable \
            \(Button holds Int -> Int)"
          , "in the instance Display Handler: type error: Handler is not \
            \in the class Showable (Button holds Int -> Int)"
          , "Box : a -> Box a"
          , "type error: Box a is not in the class Display"
          , "\"Box {\\\"a\\\": [1]}\" : String" ]
    -- ANCHOR_END: formatTypes

    -- ANCHOR: formatDisplay
    -- 「Display：自分の型は1行で入れるか、見せ方を自分で決める」
    it "shows our own types and JSON with Display" $
      runLines
        [ "display \"Lam\"", "show \"Lam\""
        , "f\"{Json.mkDec 1999 2} {JArr [JInt 1, JNull]}\""
        , "data Money = Yen Int"
        , "instance Display Money"
        , "f\"{Yen 300}\""
        , "data Tag = Tag String"
        , "instance Display Tag where display (Tag s) = \"#\" ++ s"
        , "f\"{Tag \"lam\"} {[Tag \"a\"]}\"" ]
        `shouldBe`
          [ "\"Lam\" : String", "\"\\\"Lam\\\"\" : String"
          , "\"19.99 [1,null]\" : String"
          , "Yen : Int -> Money"
          , "instance Display Money"
          , "\"Yen 300\" : String"
          , "Tag : String -> Tag"
          , "instance Display Tag"
          , "\"#lam [Tag \\\"a\\\"]\" : String" ]
    -- ANCHOR_END: formatDisplay

    -- 「f と文字列のあいだに空白があれば、いままでどおり関数の呼び出し」
    it "still calls a function f when a space comes before the string" $
      runLines ["f s = s ++ \"!\"", "f \"hi\"", "f\"hi\""]
        `shouldBe`
          ["f : String -> String", "\"hi!\" : String", "\"hi\" : String"]

    -- ANCHOR: formatPrint
    -- 「:print は、文字列をそのまま書く。レシートも、そろって見える」
    it "writes the receipt with :print" $ do
      out <- replAfter "examples/format/receipt.lam"
        [":print receipt", ":print 3"]
      out `shouldBe`
        [ T.intercalate "\n"
            [ "             RECEIPT              "
            , "sticker     4 x    300 =     1,200"
            , "mug         1 x  1,200 =     1,200"
            , "tshirt      3 x  2,500 =     7,500"
            , "----------------------------------"
            , "total                        9,900"
            , "per item                   1,237.5" ]
        , "type error: :print writes a String or a [String], not Int" ]
    -- ANCHOR_END: formatPrint

    -- ANCHOR: padProps
    -- 「そろえた文字は、幅より短くならず、もとの文字を右・左・真ん中に持つ」
    it "pads to the width, keeping the text on its side" $
      forAll ((,) <$> choose (0, 12) <*> genText) $ \(w, s) ->
        let n       = max (fromIntegral w) (T.length s)
            blank k = T.replicate k " "
            d       = n - T.length s
        in  padLeft w s  === blank d <> s
       .&&. padRight w s === s <> blank d
       .&&. center w s   === blank (d `div` 2) <> s <> blank (d - d `div` 2)

    -- 「カンマを外すと、もとの数。0 で埋めた数を読むと、もとの数」
    it "groups digits and pads with zeros without changing the number" $
      forAll ((,) <$> arbitrary <*> choose (0, 12)) $ \(n, w) ->
        let text    = T.pack (show (n :: Integer))
            grouped = groupDigits text
            parts   = T.splitOn "," (T.dropWhile (== '-') grouped)
            padded  = zeroPad w text
        in  read (T.unpack (T.filter (/= ',') grouped)) === n
       .&&. all ((== 3) . T.length) (drop 1 parts)
       .&&. all ((`elem` [1, 2, 3]) . T.length) (take 1 parts)
       .&&. read (T.unpack (T.filter (/= '-') padded)) === abs n
       .&&. T.isPrefixOf "-" padded === (n < 0)
       .&&. T.length padded === max (fromIntegral w) (T.length text)
    -- ANCHOR_END: padProps


  -- 「モジュール」
  describe "modules" $ do
    -- ANCHOR: moduleGolden
    mapM_ (golden . ("modules/" <>))
      [ "receipt/today", "tools/week", "price/menu", "price/broken"
      , "shapes/gallery", "shapes/both", "clash/report", "clash/fixed"
      , "ring/shop" ]
    -- ANCHOR_END: moduleGolden

    -- ANCHOR: parseModule
    -- 「module と import の行を読む」
    it "reads the module line and the import lines" $
      fmap (\(Program h is _ _) -> (h, is))
        (runLamParser program "m.lam" (T.unlines
          [ "module Shapes exposing (Shape (..), area, Describe (describe))"
          , "import Json"
          , "import Json as J (get, Json (JInt, JStr))" ]))
        `shouldBe` Right
          ( Just (Header (Loc "m.lam" 1 1) "Shapes" (ExposeSome
              [ ExposeType "Shape" AllConstructors, ExposeValue "area"
              , ExposeType "Describe" (TheseConstructors ["describe"]) ]))
          , [ Import (Loc "m.lam" 2 1) "Json" Nothing Nothing
            , Import (Loc "m.lam" 3 1) "Json" (Just "J")
                (Just (ExposeSome
                  [ ExposeValue "get"
                  , ExposeType "Json" (TheseConstructors ["JInt", "JStr"])
                  ])) ] )
    -- ANCHOR_END: parseModule

    -- ANCHOR: importNames
    -- 「import した名前の付き方：前にモジュールの名前、as なら別名だけ、
    --   かっこの一覧なら名前なしでも」
    it "names what it imports: with the module, the alias, or bare" $ do
      withShapes "import Shapes\nShapes.area (Shapes.square 3)"
        `shouldBe` Right "9 : Int"
      withShapes "import Shapes as S\nS.area (S.Circle 2)"
        `shouldBe` Right "12 : Int"
      -- Unlike Haskell, as takes the place of the name: Shapes. is gone.
      -- Haskell とちがい、as は名前の代わり。Shapes. は使えない
      withShapes "import Shapes as S\nShapes.area (S.Circle 2)"
        `shouldBe` Left "main.lam:2:1: type error: unknown name: Shapes.area"
      withShapes "import Shapes (area, Shape (..))\narea (Rect 2 5)"
        `shouldBe` Right "10 : Int"
      withShapes "import Shapes (area)\nsquare 3"
        `shouldBe` Left "main.lam:2:1: type error: unknown name: square"
      withShapes (T.unlines [ "import Shapes as S (area)"
                            , "(area (S.Circle 1), S.area (S.square 2))" ])
        `shouldBe` Right "(3, 4) : (Int, Int)"
      -- A ( at column 1 is the expression at the end, not the list.
      -- 1桁目の ( は、一覧ではなく、最後の式
      withShapes "import Shapes\n(Shapes.area (Shapes.square 2), 1)"
        `shouldBe` Right "(4, 1) : (Int, Int)"
    -- ANCHOR_END: importNames

    -- ANCHOR: hiding
    -- 「見せていない名前は、ほかのファイルからは見えない」
    it "keeps the names a module does not expose" $ do
      withShapes "import Shapes\nShapes.secret"
        `shouldBe` Left "type error: unknown name: Shapes.secret"
      withShapes "import Shapes (secret)\n1"
        `shouldBe` Left
          "main.lam:1:1: error: the module Shapes does not expose secret"
      -- A constructor hidden by Json: written bare or with Json.
      -- Json が隠した構成子。名前なしでも、Json. を付けても
      runLines
        [ "JDec 1 2", "Json.JDec 1 2", "case JInt 1 of | JDec c d -> c" ]
        `shouldBe` map (\c -> "type error: " <> c
                          <> " is hidden by the module Json"
                          <> " (it is not in its exposing list)")
          [ "JDec", "Json.JDec", "JDec" ]
      -- The values still show as JDec: only writing it is not allowed.
      -- 値の見せ方は JDec のまま。書くことだけができない
      runLines [ "Json.mkDec 1999 2", "Json.decimal (Json.mkDec 19990 3)" ]
        `shouldBe` [ "JDec 1999 2 : Json"
                   , "Just (1999, 2) : Maybe (Int, Int)" ]
    -- ANCHOR_END: hiding

    -- ANCHOR: patterns
    -- 「別名の付いた構成子も、パターンに書ける」
    it "matches constructors written with the module's name" $ do
      withShapes (T.unlines
        [ "import Shapes as S"
        , "name s = case s of"
        , "  | S.Circle _ -> \"circle\""
        , "  | S.Rect _ _ -> \"rect\""
        , "(name (S.Circle 1), name (S.square 2))" ])
        `shouldBe` Right "(\"circle\", \"rect\") : (String, String)"
      runLines [ "case Json.mkDec 1 1 of | Json.JInt n -> n | _ -> -1" ]
        `shouldBe` [ "-1 : Int" ]
    -- ANCHOR_END: patterns

    -- ANCHOR: classTests
    -- 「クラスとメソッドを見せる。インスタンスは、どこへでも付いていく」
    it "shows a class with its methods, and takes the instances along" $ do
      withShapes "import Shapes\nShapes.describe (Shapes.Circle 2)"
        `shouldBe` Right "\"circle 2\" : String"
      withShapes "import Shapes (Describe (..))\ndescribe (Shapes.Rect 1 2)"
        `shouldBe` Right "\"rect 1 2\" : String"
      -- Our own type in the class of Shapes.
      -- Shapes のクラスに、自分の型を入れる
      withShapes (T.unlines
        [ "import Shapes (Describe (..))"
        , "data Dot = Dot"
        , "instance Describe Dot where"
        , "  describe d = \"a dot\""
        , "describe Dot" ])
        `shouldBe` Right "\"a dot\" : String"
      -- Describe without (..): the class, but not its method.
      -- (..) のない Describe：クラスだけで、メソッドは見えない
      runModules
        [ ("Shapes.lam", T.replace "Describe (..)" "Describe" shapes) ]
        "import Shapes\nShapes.describe (Shapes.Circle 2)"
        `shouldBe` Left
          "main.lam:2:1: type error: Shapes.describe is hidden by the \
          \module Shapes (it is not in its exposing list)"
      -- The instance comes with the type, with no word in the list.
      -- インスタンスは、一覧に何も書かなくても、型と一緒に来る
      withShapes "import Shapes (Shape (..))\nf\"{Circle 2}\""
        `shouldBe` Right "\"(circle 2)\" : String"
    -- ANCHOR_END: classTests

    -- ANCHOR: instanceClash
    -- 「同じインスタンスが2つのモジュールから来たら、エラーにする」
    it "stops when two modules declare the same instance" $ do
      let shown m how = T.unlines
            [ "module " <> m <> " exposing (..)"
            , "import Box (Box (..))"
            , "instance Display Box where"
            , "  display b = " <> how
            , "tag = 1" ]
          go = runModules
            [ ("Box.lam", "module Box exposing (Box (..))\ndata Box = Box")
            , ("A.lam", shown "A" "\"a\""), ("B.lam", shown "B" "\"b\"") ]
      go "import A\nimport B\n1" `shouldBe` Left
        "main.lam:2:1: error: the instance Display Box comes from both \
        \A and B"
      go "import A\nimport Box\nf\"{Box.Box}\""
        `shouldBe` Right "\"a\" : String"
    -- ANCHOR_END: instanceClash

    -- ANCHOR: fmtNames
    -- 「f"..." は隠した名前を呼ぶので、import や自分の定義に左右されない」
    it "calls hidden names from f\"...\", whatever is imported" $ do
      fmap render (parseExpr "f\"{x:>5}\"")
        `shouldBe` Right "$fmt.padLeft 5 ($fmt.display x)"
      -- Our own display, padLeft and show do not change f"...".
      -- 自分の display・padLeft・show は、f"..." を変えない
      runModules []
        "display x = \"own\"\n\
        \padLeft n s = \"own\"\n\
        \show = 0\n\
        \(f\"{12:>4}|{[1]=}\", display 1)"
        `shouldBe` Right "(\"  12|[1]=[1]\", \"own\") : (String, String)"
    -- ANCHOR_END: fmtNames

    -- ANCHOR: standardTests
    -- 「標準のモジュール：別名を付けられ、中の組み込みも見せる」
    it "makes the standard library into modules" $ do
      out <- replImporting []
        [ "import Map as M"
        , "M.insertWith (+) \"a\" 1 (M.fromList [(\"a\", 2)])"
        , "import Float as F (isNaN)"
        , "(F.pow 2.0 3.0, isNaN (0.0 / 0.0))"
        , "Prelude.map (\\x -> x + 1) [1, 2]"
        , "List.sortBy (\\a b -> a > b) [1, 3, 2]" ]
      out `shouldBe`
          [ "", "{\"a\": 3} : Map String Int", ""
          , "(8, true) : (Float, Bool)"
          , "[2, 3] : [Int]", "[3, 2, 1] : [Int]" ]
      Map.keys (sessionModules preludeSession) `shouldBe`
        [ "Base64", "Bytes", "File", "Float", "Hash", "IO", "Json", "List"
        , "Map", "Maybe", "Path", "Prelude", "Process", "Random", "Result"
        , "Set", "String", "Time", "Uuid" ]
    -- ANCHOR_END: standardTests

    -- ANCHOR: finding
    -- 「探す順：まず隣、それから LAM_PATH のフォルダーを順に」
    it "looks next to the file first, then in LAM_PATH" $ do
      let next  = "module Greet exposing (hi)\nhi = \"next to it\""
          inLib = "module Greet exposing (hi)\nhi = \"in lib\""
          go files = fmap (fromMaybe "" . snd) $ (>>= finalText) $
            runIdentity $
            loadProgram (mapFiles (Map.fromList files) ["lib", "more"])
              preludeSession preludeSession "app/main.lam"
              "import Greet\nGreet.hi"
      go [("app/Greet.lam", next), ("lib/Greet.lam", inLib)]
        `shouldBe` Right "\"next to it\" : String"
      go [("more/Greet.lam", next), ("lib/Greet.lam", inLib)]
        `shouldBe` Right "\"in lib\" : String"
      go [] `shouldBe` Left ("app/main.lam:1:1: error: no module Greet "
        <> "(looked for app/Greet.lam, lib/Greet.lam, more/Greet.lam)")
    -- ANCHOR_END: finding

    -- ANCHOR: once
    -- 「2つのファイルが import しても、モジュールのファイルは1度だけ読む」
    it "reads a module file only once, even if two files import it" $ do
      let files = Map.fromList
            [ ("A.lam", "module A exposing (a)\nimport C\na = C.c + 1")
            , ("B.lam", "module B exposing (b)\nimport C\nb = C.c + 2")
            , ("C.lam", "module C exposing (c)\nc = 10") ]
          logging = Files
            { readSource = \path -> do
                modify (path :)
                pure (Map.lookup path files)
            , searchPath = [] }
          (loaded, readLog) = runState
            (loadProgram logging preludeSession preludeSession "main.lam"
               "import A\nimport B\nA.a + B.b") []
      fmap snd (loaded >>= finalText) `shouldBe` Right (Just "23 : Int")
      reverse readLog `shouldBe` ["A.lam", "C.lam", "B.lam"]
    -- ANCHOR_END: once

    -- ANCHOR: moduleErrors
    -- 「モジュールのファイルのまちがいを、場所と一緒に教える」
    it "tells what is wrong with a module file, and where" $ do
      let with m = runModules [("M.lam", m)]
      with "x = 1" "import M\n1" `shouldBe` Left
        ("M.lam:1:1: error: a module starts with the line  "
         <> "module M exposing (...)")
      with "module N exposing (x)\nx = 1" "import M\n1" `shouldBe`
        Left "M.lam:1:1: error: the file M.lam must be the module M, not N"
      with "module M exposing (y)\nx = 1" "import M\n1" `shouldBe`
        Left ("M.lam:1:1: error: the module M exposes y, "
              <> "but does not define it")
      with "module M exposing (x)\nx = 1\nx" "import M\n1" `shouldBe`
        Left ("M.lam: error: a module cannot end with an expression"
              <> " (only a program can)")
      with "module M exposing (x)\nx = 1 // 0" "import M\n1" `shouldBe`
        Left "M.lam:2:7: in the definition of x: error: divided by zero"
    -- ANCHOR_END: moduleErrors

    -- ANCHOR: clashTests
    -- 「2つのモジュールの同じ名前・同じ型は、エラーにする」
    it "stops when two modules give the same name or the same type" $ do
      let box m n = T.unlines
            [ "module " <> m <> " exposing (Box (..), size)"
            , "data Box = Box Int", "size = " <> n ]
          go = runModules [("A.lam", box "A" "1"), ("B.lam", box "B" "2")]
      go "import A (size)\nimport B (size)\n1"
        `shouldBe` Left
          "main.lam:2:1: error: the name size comes from both A and B"
      go "import A\nimport B\n1" `shouldBe` Left
        "main.lam:2:1: error: the type Box comes from both A and B"
      -- Only the names in front differ: A.size and B.size are fine.
      -- 前の名前だけがちがうなら、A.size と B.size は大丈夫
      go "import A as X\nX.size" `shouldBe` Right "1 : Int"
      runModules [] "data Maybe a = Nope\n1" `shouldBe` Left
        ("in the data type Maybe: type error: "
         <> "the type Maybe belongs to the module Maybe")
      runModules [] "class Display a where\n  display : a -> String\n1"
        `shouldBe` Left ("in the class Display: type error: "
                         <> "the class Display belongs to the module Prelude")
    -- ANCHOR_END: clashTests

    -- ANCHOR: ringTest
    -- 「import の輪は、ぐるぐる回らずにエラーにする」
    it "finds imports that go around in a ring" $
      runModules
        [ ("A.lam", "module A exposing (a)\nimport B\na = 1")
        , ("B.lam", "module B exposing (b)\nimport A\nb = 2") ]
        "import A\n1"
        `shouldBe` Left ("B.lam:2:1: error: the imports go around in a "
                         <> "ring: main.lam -> A.lam -> B.lam -> A.lam")
    -- ANCHOR_END: ringTest

    -- ANCHOR: replImport
    -- 「REPL の import と :browse」
    it "imports and browses in the REPL" $ do
      out <- replImporting [("Shapes.lam", shapes)]
        [ "import Shapes as S (area)", "area (S.square 3)", ":browse Shapes"
        , "import Nope" ]
      out `shouldBe`
        [ "", "9 : Int"
        , T.intercalate "\n"
            [ "Shapes.Circle : Int -> Shape"
            , "Shapes.Rect : Int -> Int -> Shape"
            , "Shapes.area : Shape -> Int"
            , "Shapes.describe : Describe a => a -> String"
            , "Shapes.square : Int -> Shape"
            , "(instances: Describe Shape, Display Shape)" ]
        , "<repl>:1:1: error: no module Nope (looked for Nope.lam)" ]
    -- ANCHOR_END: replImport

    -- ANCHOR: exposedProp
    -- 「exposing に書いた名前だけが、ほかのファイルから見える」
    it "shows exactly the names in the exposing list" $
      property $ forAll (sublistOf ["one", "two", "three"]) $ \shown ->
        let header =
              "module Nums exposing (" <> T.intercalate ", " shown <> ")"
            nums = T.unlines [ header, "one = 1", "two = 2", "three = 3" ]
            seen n = runModules [("Nums.lam", nums)]
                       ("import Nums\nNums." <> n)
                       /= Left ("type error: unknown name: Nums." <> n)
        in not (null shown) ==>
             [ n | n <- ["one", "two", "three"], seen n ] === shown
    -- ANCHOR_END: exposedProp

  -- 「IO と do」
  describe "IO and do" $ do
    -- ANCHOR: ioTypes
    -- 「IO の値の型。IO は比べられない」
    it "gives IO values their types" $
      runLines
        [ ":type putStrLn", ":type getLine", ":type readFile"
        , ":type IO.putStrLn"
        , "putStrLn \"hi\" == putStrLn \"hi\"" ]
        `shouldBe`
          [ "String -> IO ()", "IO (Maybe String)"
          , "String -> IO (Result String String)", "String -> IO ()"
          , "type error: IO () is not in the class Comparable" ]
    -- ANCHOR_END: ioTypes

    -- ANCHOR: monadTypes
    -- 「pure と ap は Applicative、bind は Monad のメソッド」
    it "makes pure and ap the methods of Applicative, and bind of Monad" $
      runLines
        [ ":type pure", ":type ap", ":type bind"
        , ":type forEach", ":type mapM" ]
        `shouldBe`
          [ "Applicative f => a -> f a"
          , "Applicative f => f (a -> b) -> f a -> f b"
          , "Monad f => f a -> (a -> f b) -> f b"
          , "Monad f => [a] -> (a -> f b) -> f ()"
          , "Monad f => (a -> f b) -> [a] -> f [b]" ]
    -- ANCHOR_END: monadTypes

    -- ANCHOR: doSugar
    -- 「do は、隠した名前の bind と関数に開く」
    it "opens do into the hidden bind and functions" $
      map (fmap render . parseExpr)
        [ "do x <- getLine; putStrLn x"
        , "do putStr \"a\"; let b = \"b\"; putStrLn b"
        , "do\n  (a, b) <- pure (1, 2)\n  pure (a + b)" ]
        `shouldBe` map Right
          [ "$do.bind getLine (\\x -> putStrLn x)"
          , "$do.bind (putStr \"a\") (\\$_ -> let b = \"b\" in putStrLn b)"
          , "$do.bind (pure (1, 2)) (\\$arg -> case $arg of \
            \| (a, b) -> pure (a + b))" ]
    -- ANCHOR_END: doSugar

    -- ANCHOR: doErrors
    -- 「do の最後の行は式。; で1行に並べてもよい」
    it "needs an expression at the end of do" $
      runLines
        [ "do x <- pure 1", ":type do x <- pure 1; pure (x + 1)" ]
        `shouldBe`
          [ "<repl>:1:15:\n  |\n1 | do x <- pure 1\n  |               ^\n\
            \the last line of do must be an expression\n"
          , "(Monad f, Number a) => f a\n  -- usually : Monad f => f Int" ]
    -- ANCHOR_END: doErrors

    -- ANCHOR: doMonads
    -- 「do は、Maybe でも Result でもリストでも使える」
    it "uses do for Maybe, Result and lists too" $
      runLines
        [ "do x <- Just 1; y <- Just 2; pure (x + y)"
        , "do x <- Just 1; y <- Nothing; pure (x + y)"
        , "do x <- Ok 1; y <- Err \"no y\"; pure (x + y)"
        , "do x <- [1, 2]; y <- [\"a\", \"b\"]; pure (x, y)"
        , "mapM (\\x -> if x > 0 then Just x else Nothing) [3, 1, 2]" ]
        `shouldBe`
          [ "Just 3 : Maybe Int", "Nothing : Maybe Int"
          , "Err \"no y\" : Result String Int"
          , "[(1, \"a\"), (1, \"b\"), (2, \"a\"), (2, \"b\")] : [(Int, String)]"
          , "Just [3, 1, 2] : Maybe [Int]" ]
    -- ANCHOR_END: doMonads

    -- ANCHOR: applicativeOps
    -- 「<$> と <*> で、箱の中の値を1つの関数に渡す」
    it "gives the values in boxes to one function with <$> and <*>" $
      runLines
        [ ":type (<$>)", ":type (<*>)"
        , "(+) <$> Just 1 <*> Just 2"
        , "(+) <$> Just 1 <*> Nothing"
        , "(+) <$> Ok 1 <*> Err \"no b\""
        , "(+) <$> [1, 2] <*> [10, 20]"
        , "(+) <$> Just 1 <*> Just 2 == Just 3" ]
        `shouldBe`
          [ "Functor f => (a -> b) -> f a -> f b"
          , "Applicative f => f (a -> b) -> f a -> f b"
          , "Just 3 : Maybe Int", "Nothing : Maybe Int"
          , "Err \"no b\" : Result String Int"
          , "[11, 21, 12, 22] : [Int]"
          , "true : Bool" ]
    -- ANCHOR_END: applicativeOps

    -- ANCHOR: adoSugar
    -- 「ado は、隠した名前の map と ap に開く」
    it "opens ado into the hidden map and ap" $
      map (fmap render . parseExpr)
        [ "ado x <- Just 1; y <- Just 2; pure (x + y)"
        , "ado putStr \"a\"; s <- getLine; pure s"
        , "ado pure 3" ]
        `shouldBe` map Right
          [ "$do.ap ($do.map (\\x -> \\y -> x + y) (Just 1)) (Just 2)"
          , "$do.ap ($do.map (\\$_ -> \\s -> s) (putStr \"a\")) getLine"
          , "$do.pure 3" ]
    -- ANCHOR_END: adoSugar

    -- ANCHOR: adoErrors
    -- 「ado の行は、上の行の名前を使えない。最後は pure」
    it "says why an ado cannot be opened" $
      runLines
        [ "ado x <- Just 1; y <- Just x; pure y"
        , "ado x <- Just 1; x" ]
        `shouldBe`
          [ "<repl>:1:37:\n  |\n1 | ado x <- Just 1; y <- Just x; pure y\n\
            \  |                                     ^\n\
            \in an ado, a line cannot use x from a line above (use do)\n"
          , "<repl>:1:19:\n  |\n1 | ado x <- Just 1; x\n\
            \  |                   ^\n\
            \the last line of an ado must be pure, then an expression\n" ]
    -- ANCHOR_END: adoErrors

    -- ANCHOR: adoTypes
    -- 「ado は Applicative だけ、do は Monad を求める」
    it "needs only an Applicative for an ado" $
      runLinesIn emptyWorld { worldInput = ["Lam", "3"] }
        [ "both a b = ado x <- a; y <- b; pure (x, y)"
        , "bothDo a b = do x <- a; y <- b; pure (x, y)"
        , "both (Just 1) (Just \"one\")"
        , "both getLine getLine" ]
        `shouldBe`
          ( [ "both : Applicative f => f a -> f b -> f (a, b)"
            , "bothDo : Monad f => f a -> f b -> f (a, b)"
            , "Just (1, \"one\") : Maybe (Int, String)"
            , "(Just \"Lam\", Just \"3\") : (Maybe String, Maybe String)" ]
          , emptyWorld )
    -- ANCHOR_END: adoTypes

    -- ANCHOR: makingIsNotDoing
    -- 「作るだけでは、何も起きない。やるたびに起きる」
    it "does nothing when an action is made, and does it each time" $
      runLinesIn emptyWorld
        [ "hi = putStrLn \"hi\""
        , "[hi, hi, hi]"
        , "do hi; hi" ]
        `shouldBe`
          ( [ "hi : IO ()"
            , "[<IO action>, <IO action>, <IO action>] : [IO ()]"
            , "" ]
          , emptyWorld { worldOutput = ["hi\n", "hi\n"] } )
    -- ANCHOR_END: makingIsNotDoing

    -- ANCHOR: replIO
    -- 「REPL は、IO の式をその場でやる。pure 1 も IO として」
    it "does an IO in the REPL, and takes pure 1 as an IO" $
      runLinesIn emptyWorld
        [ "pure 1", "pure (1, \"one\")", "map (\\x -> x + 1) (pure 3)"
        , "forEach [\"a\", \"b\"] (\\s -> putStrLn (toUpper s))" ]
        `shouldBe`
          ( [ "1 : Int", "(1, \"one\") : (Int, String)", "4 : Int", "" ]
          , emptyWorld { worldOutput = ["B\n", "A\n"] } )
    -- ANCHOR_END: replIO

    -- ANCHOR: hiddenBind
    -- 「自分で bind を定義しても、do は Monad の bind を呼ぶ」
    it "calls the bind of Monad, even with a bind of your own" $
      runLinesIn emptyWorld
        [ "hi = putStrLn \"hi\"", "bind m f = 42", "do hi; hi" ]
        `shouldBe`
          ( [ "hi : IO ()", "bind : Number c => a -> b -> c", "" ]
          , emptyWorld { worldOutput = ["hi\n", "hi\n"] } )
    -- ANCHOR_END: hiddenBind

    -- ANCHOR: fakeFiles
    -- 「偽物の世界のファイルを、読んで書く」
    it "reads and writes the files of a fake world" $ do
      let world = emptyWorld
            { worldFiles = textFiles [("memo.txt", "λ memo\n")] }
          (outs, world') = runLinesIn world
            [ "readFile \"memo.txt\""
            , "readFile \"nothing.txt\""
            , "writeFile \"new.txt\" \"hello\"" ]
      outs `shouldBe`
        [ "Ok \"λ memo\\n\" : Result String String"
        , "Err \"nothing.txt: no such file\" : Result String String"
        , "Ok () : Result String ()" ]
      worldFiles world' `shouldBe`
        textFiles [("memo.txt", "λ memo\n"), ("new.txt", "hello")]
    -- ANCHOR_END: fakeFiles

    -- ANCHOR: echo
    -- 「読んだ行を、そのまま全部書き出す（でたらめな行で）」
    it "echoes every line it reads" $
      forAll (listOf (T.filter (/= '\n') <$> genText)) $ \ls ->
        let echo = "echo = do line <- getLine; case line of \
                   \| Nothing -> pure () | Just s -> do putStrLn s; echo"
            (_, world') = runLinesIn emptyWorld { worldInput = ls }
                            [echo, "echo"]
        in outputOf world' === T.concat [ l <> "\n" | l <- ls ]
    -- ANCHOR_END: echo

    -- ANCHOR: shopFake
    -- 「shop.lam は、偽物の世界に receipt.txt を書く。注文が
    --   壊れていれば、そう言う」
    it "writes receipt.txt into a fake world, or says what is wrong" $ do
      source  <- Utf8.readFile "examples/io/shop.lam"
      receipt <- Utf8.readFile "examples/modules/receipt/Receipt.lam"
      orders  <- Utf8.readFile "examples/io/orders.json"
      let modules = mapFiles
            (Map.fromList [("lib/Receipt.lam", receipt)]) ["lib"]
          run json = runFile modules emptyWorld
            { worldFiles = textFiles [("examples/io/orders.json", json)] }
            "shop.lam" source
          (result, world) = run orders
      result `shouldBe` Right ()
      fmap (take 2 . T.lines . decodeUtf8)
          (Map.lookup "receipt.txt" (worldFiles world))
        `shouldBe` Just [ "             RECEIPT              "
                        , "sticker     4 x    300 =     1,200" ]
      map (outputOf . snd . run)
        ["{\"sticker\": 4}", "[{\"item\": \"mug\"}]", "[1,"]
        `shouldBe`
          [ "no receipt: the orders must be a list\n"
          , "no receipt: each order needs an item and a count\n"
          , "no receipt: json list value: not enough input\n" ]
    -- ANCHOR_END: shopFake

    -- ANCHOR: checkMain
    -- 「main は IO () でなければならない」
    it "says when main is not IO ()" $
      map (\src -> fst (runFile (mapFiles Map.empty []) emptyWorld "m" src))
        ["main = 1", "main = putStrLn", "main = pure 3"]
        `shouldBe` map (Left . ("in main: type error: " <>))
          [ "IO () is not in the class Number"
          , "main must be IO (), [String] -> IO (), or [String] -> \
            \Map String String -> IO () (or IO (Result e ()) in place of \
            \IO ()), but it is String -> IO ()"
          , "() is not in the class Number" ]
    -- ANCHOR_END: checkMain

    -- ANCHOR: ioGolden
    mapM_ (golden . ("io/" <>)) ["hello", "shop", "ado"]
    -- ANCHOR_END: ioGolden

  -- 「失敗を早めに返す：? と後始末」
  describe "errors" $ do
    -- ANCHOR: trySugar
    -- 「? の行は、branch で開く case に。失敗なら、すぐに pure で返す」
    it "opens a line with ? into a case on branch" $
      map (fmap render . parseExpr)
        [ "do t <- readFile p ?; putStrLn t"
        , "do writeFile p t ?; pure (Ok ())" ]
        `shouldBe` map Right
          [ "$do.bind (readFile p) (\\$try -> case $try.branch $try of \
            \| Ok t -> putStrLn t | Err $failed -> $do.pure $failed)"
          , "$do.bind (writeFile p t) (\\$try -> case $try.branch $try of \
            \| Ok _ -> pure (Ok ()) | Err $failed -> $do.pure $failed)" ]
    -- ANCHOR_END: trySugar
    -- ANCHOR: deferSugar
    -- 「defer の行は、残りの行を $do.finally で包む」
    it "opens defer into the hidden finally around the rest" $
      fmap render (parseExpr "do defer putStrLn \"bye\"; putStrLn \"hi\"")
        `shouldBe` Right
          "$do.finally ($do.bind ($do.pure ()) (\\$_ -> putStrLn \"hi\")) \
          \(putStrLn \"bye\")"
    -- ANCHOR_END: deferSugar
    -- ANCHOR: tryErrors
    -- 「最後の行に ? や defer は書けない。?: と /? は、これまでどおり」
    it "says where ? and defer cannot go, and keeps ?: and /?" $
      runLines
        [ "do putStrLn \"a\" ?"
        , "do putStrLn \"a\"; defer putStrLn \"b\""
        , "do x <- pure (Nothing ?: 1); pure (x + 1)"
        , "do x <- pure (7 /? 0); pure x"
        , "deferred = 3" ]
        `shouldBe`
          [ "<repl>:1:18:\n  |\n1 | do putStrLn \"a\" ?\n  |                  ^\n\
            \the last line of do is the answer itself: write it without ?\n"
          , "<repl>:1:36:\n  |\n1 | do putStrLn \"a\"; defer putStrLn \"b\"\n\
            \  |                                    ^\n\
            \the last line of do must be an expression\n"
          , "2 : Int"
          , "Nothing : Maybe Float"
          , "deferred : Number a => a\n  -- usually : Int" ]
    -- ANCHOR_END: tryErrors
    -- ANCHOR: tryTypes
    -- 「? を使う do の型。Maybe と Result は混ぜられない」
    it "gives the do with ? the type of the container" $
      runLines
        [ ":type \\p -> do t <- readFile p ?; pure (Ok (length t))"
        , ":type do n <- getLine ?; pure (Just (length n))"
        , ":type branch"
        , ":type Result.mapError"
        , ":type do n <- getLine ?; t <- readFile n ?; pure (Ok t)" ]
        `shouldBe`
          [ "String -> IO (Result String Int)"
          , "IO (Maybe Int)"
          , "Fallible f => f a -> Result (f b) a"
          , "(a -> b) -> Result a c -> Result b c"
          , "type error: the types do not fit: \
            \Result String String where Maybe String is needed" ]
    -- ANCHOR_END: tryTypes
    -- ANCHOR: tryStops
    -- 「? は、最初の失敗で止まる。そのあとの行は、やらない」
    it "stops at the first failure, and does not do the lines after it" $
      runLinesIn emptyWorld
            { worldFiles = textFiles [("a.txt", "apple")] }
        [ "do a <- readFile \"a.txt\" ?; putStrLn a; \
          \b <- readFile \"b.txt\" ?; putStrLn b; pure (Ok ())"
        , "do x <- Just (Just 1) ?; y <- Just Nothing ?; pure (Just (x + y))" ]
        `shouldBe`
          ( [ "Err \"b.txt: no such file\" : Result String ()"
            , "Just Nothing : Maybe (Maybe Int)" ]
          , emptyWorld { worldFiles = textFiles [("a.txt", "apple")]
                       , worldOutput = ["apple\n"] } )
    -- ANCHOR_END: tryStops
    -- ANCHOR: tryProperty
    -- 「? を並べた do は、Haskell の sequence と同じ答え。画面に出るのは、
    --   最初の Err の手前まで」
    it "gives the answer of sequence, and does the lines up to the first Err" $
      forAll (listOf genResult) $ \rs ->
        let line i r = named "x" i <> " <- pure (" <> lamResult r <> ") ?; \
                       \putStrLn \"" <> num (toInteger i) <> "\""
            xs = [ named "x" i | i <- [1 .. length rs] ]
            source = "do " <> T.concat [ line i r <> "; "
                                       | (i, r) <- zip [1 ..] rs ]
                       <> "pure (Ok [" <> T.intercalate ", " xs <> "])"
            (outs, world) = runLinesIn emptyWorld [source]
            done = length (takeWhile isRight rs)
        in (map valuePart outs, outputOf world)
             === ( [lamResultList (sequence rs)]
                 , T.concat [ num i <> "\n" | i <- [1 .. toInteger done] ] )
    -- ANCHOR_END: tryProperty
    -- ANCHOR: stopTests
    -- 「error と todo は、どんな型にもなれて、止まる」
    it "stops with error and todo, which can be of any type" $
      runLines
        [ ":type error"
        , "if 1 > 2 then error \"never\" else 3"
        , "1 + error \"boom\""
        , "length [todo \"a\", \"b\"]"
        , "f x = todo \"f\""
        , "f 3" ]
        `shouldBe`
          [ "String -> a", "3 : Int", "error: boom"
          , "error: not written yet: a", "f : a -> b"
          , "error: not written yet: f" ]
    -- ANCHOR_END: stopTests
    -- ANCHOR: finallyTests
    -- 「後始末は、止まっても、? で返っても、やる。defer は後ろから」
    it "cleans up after a stop and after ?, the last defer first" $
      runLinesIn emptyWorld
        [ "IO.finally (putStrLn \"body\") (putStrLn \"cleanup\")"
        , "IO.finally (do pure (); pure (1 // 0)) (putStrLn \"cleanup\")"
        , "do defer putStrLn \"1\"; defer putStrLn \"2\"; putStrLn \"body\""
        , "do defer putStrLn \"bye\"; t <- readFile \"none\" ?; pure (Ok t)"
        , "do defer putStrLn \"bye\"; x <- pure (1 // 0); pure x" ]
        `shouldBe`
          ( [ "", "error: divided by zero", ""
            , "Err \"none: no such file\" : Result String String"
            , "error: divided by zero" ]
          , emptyWorld { worldOutput = reverse
              [ "body\n", "cleanup\n", "cleanup\n", "body\n", "2\n", "1\n"
              , "bye\n", "bye\n" ] } )
    -- 「後始末も止まったら、そちらを伝える（Haskell と同じ）」
    it "tells the stop of the cleanup, when both stop" $
      runLines [ "IO.finally (do pure (); error \"body\") (error \"cleanup\")" ]
        `shouldBe` [ "error: cleanup" ]
    -- ANCHOR_END: finallyTests
    -- ANCHOR: deferMany
    -- 「defer を3つ。後のものから動き、後始末が止まっても外側は動く」
    it "runs many defers, the last first, even when a cleanup stops" $
      runLinesIn emptyWorld
        [ "bye n = putStrLn f\"bye {n}\""
        , "do defer bye 1; defer bye 2; defer bye 3; putStrLn \"body\""
        , "do defer bye 1; defer (do pure (); pure (1 // 0)); \
          \putStrLn \"body\""
        , "do defer bye 1; defer (pure (1 // 0)); putStrLn \"body\"" ]
        `shouldBe`
          ( [ "bye : Display a => a -> IO ()", "", "error: divided by zero"
            , "error: divided by zero" ]
          , emptyWorld { worldOutput = reverse
              [ "body\n", "bye 3\n", "bye 2\n", "bye 1\n"
              , "body\n", "bye 1\n", "bye 1\n" ] } )
    -- ANCHOR_END: deferMany
    -- ANCHOR: bracketTry
    -- 「bracket は返すのを忘れない。try は、やる途中で止まったものだけを
    --   つかまえる」
    it "gives back with bracket, and catches stops while doing with try" $
      runLinesIn emptyWorld
        [ "IO.bracket (pure 3) (\\r -> putStrLn f\"back {r}\") \
          \(\\r -> pure (r * 2))"
        , "IO.bracket (pure 3) (\\r -> putStrLn f\"back {r}\") \
          \(\\r -> pure (r // 0))"
        , "IO.try (do pure (); pure (1 // 0))"
        , "IO.try (pure 5)"
        , "IO.try (pure (1 // 0))" ]
        `shouldBe`
          ( [ "6 : Int", "error: divided by zero"
            , "Err \"divided by zero\" : Result String Int"
            , "Ok 5 : Result String Int"
            , "error: divided by zero" ]
          , emptyWorld { worldOutput = ["back 3\n", "back 3\n"] } )
    -- ANCHOR_END: bracketTry
    -- ANCHOR: mainResult
    -- 「main が Err を返したら、その文で止まる」
    it "stops with the words of an Err from main" $
      map (\src -> fst (runFile (mapFiles Map.empty []) emptyWorld "m" src))
        [ "main : IO (Result String ())\nmain = pure (Err \"no orders\")"
        , "main = pure (Err 404)"
        , "main = pure (Ok ())"
        , "main = pure (Ok 3)" ]
        `shouldBe`
          [ Left "error: no orders", Left "error: 404", Right ()
          , Left "in main: type error: \
                 \the types do not fit: Result a b where () is needed" ]
    -- ANCHOR_END: mainResult
    -- ANCHOR: shopErrors
    -- 「注文が壊れていても、ファイルがなくても、お店は閉まる」
    it "closes the shop after any failure" $ do
      source  <- Utf8.readFile "examples/errors/shop.lam"
      receipt <- Utf8.readFile "examples/modules/receipt/Receipt.lam"
      let modules = mapFiles
            (Map.fromList [("lib/Receipt.lam", receipt)]) ["lib"]
          run files = runFile modules emptyWorld
            { worldFiles = textFiles files } "shop.lam" source
          orders json = [("examples/errors/orders.json", json)]
          tell (result, world) = (result, outputOf world)
      map (tell . run)
        [orders "{\"sticker\": 4}", orders "[1,", []]
        `shouldBe`
          [ ( Left "error: the orders must be a list"
            , "the shop opens\nthe shop closes\n" )
          , ( Left "error: json list value: not enough input"
            , "the shop opens\nthe shop closes\n" )
          , ( Left "error: examples/errors/orders.json: no such file"
            , "the shop opens\nthe shop closes\n" ) ]
    -- ANCHOR_END: shopErrors
    -- ANCHOR: errorsGolden
    mapM_ (golden . ("errors/" <>)) ["shop", "register", "todo"]
    -- ANCHOR_END: errorsGolden

  describe "script" $ do
    -- ANCHOR: argsTests
    -- 「引数と環境変数を、3つのやり方で受け取る」
    it "takes the arguments and the environment, in three ways" $ do
      let world = emptyWorld
            { worldArgs = ["Lam", "Haskell"]
            , worldEnv  = Map.fromList [("LAM_USER", "Lisp")] }
          run src = outputOf (snd (script world src))
      map run
        [ "main = do\n  args <- getArgs\n  putStrLn f\"{args}\""
        , "main args = putStrLn f\"{length args} names\""
        , "main args env = putStrLn (Map.lookup \"LAM_USER\" env ?: \"?\")"
        , "main = do\n  user <- lookupEnv \"NOPE\"\n  putStrLn f\"{user}\"" ]
        `shouldBe`
          [ "[\"Lam\", \"Haskell\"]\n", "2 names\n", "Lisp\n", "Nothing\n" ]
    -- ANCHOR_END: argsTests
    -- ANCHOR: mainShapes
    -- 「引数のある main も、IO (Result e ()) を返せる。形に合わない main は、
    --   許される形を教える」
    it "lets main with arguments give a Result, and tells the shapes" $
      map (fst . script emptyWorld { worldArgs = ["x", "y"] })
        [ "main : [String] -> IO (Result String ())\n\
          \main args = pure (Err (join \",\" args))"
        , "main args env = pure (Ok ())"
        , "main args = pure 3" ]
        `shouldBe`
          [ Errored "error: x,y"
          , Finished ""
          , Errored "in main: type error: main must be IO (), [String] -> \
                    \IO (), or [String] -> Map String String -> IO () (or \
                    \IO (Result e ()) in place of IO ()), but it is \
                    \a -> f b" ]
    -- ANCHOR_END: mainShapes
    -- ANCHOR: exitTests
    -- 「exit で止まり、残りはやらない。標準出力と標準エラーは別々」
    it "stops at exit, and does not do the rest" $ do
      let (ending, world) = script emptyWorld (T.unlines
            [ "main = do"
            , "  putStrLn \"before\""
            , "  ePutStrLn \"oops\""
            , "  exit 3"
            , "  putStrLn \"after\"" ])
      ending `shouldBe` ExitedWith 3
      outputOf world `shouldBe` "before\n"
      errorsOf world `shouldBe` "oops\n"
    -- ANCHOR_END: exitTests
    -- ANCHOR: exitCleanup
    -- 「exit でも後始末はやる。IO.try は exit をつかまえない。後始末も
    --   exit したら、そちらが勝つ」
    it "does the cleanups on exit, and try does not catch it" $
      map (\src -> let (e, w) = script emptyWorld src in (e, outputOf w))
        [ "main = do\n  defer putStrLn \"closed\"\n  exit 4"
        , "main = IO.finally (exit 5) (putStrLn \"finally\")"
        , "main = IO.bracket (pure 1) (\\_ -> putStrLn \"back\") \
          \(\\_ -> exit 6)"
        , "main = do\n  r <- IO.try (exit 7)\n  putStrLn \"caught\""
        , "main = IO.finally (exit 8) (exit 9)" ]
        `shouldBe`
          [ (ExitedWith 4, "closed\n"), (ExitedWith 5, "finally\n")
          , (ExitedWith 6, "back\n"), (ExitedWith 7, "")
          , (ExitedWith 9, "") ]
    -- ANCHOR_END: exitCleanup
    -- ANCHOR: placeTests
    -- 「エラーは、いちばん内側の場所を指す。error と todo も。#! の行が
    --   あっても、行の番号はずれない。REPL は場所を見せない」
    it "points at the innermost place, and keeps the lines after #!" $ do
      map (fst . script emptyWorld)
        [ "half x = x // 0\nmain = do\n  putStrLn \"go\"\n\
          \  putStrLn f\"{half 3}\""
        , "#!/usr/bin/env lam\nmain = do\n  putStrLn \"go\"\n\
          \  error \"boom\""
        , "main = do\n  putStrLn \"go\"\n  todo \"the rest\""
        , "main = do\n  putStrLn (1 + 2)" ]
        `shouldBe`
          [ Errored "script.lam:1:12: error: divided by zero"
          , Errored "script.lam:4:3: error: boom"
          , Errored "script.lam:3:3: error: not written yet: the rest"
          , Errored "script.lam:2:3: in the definition of main: \
                    \type error: String is not in the class Number" ]
      runLines ["10 // 0", "error \"boom\""]
        `shouldBe` ["error: divided by zero", "error: boom"]
    -- ANCHOR_END: placeTests
    -- ANCHOR: wordTests
    -- 「行と単語に切る。文字を見分ける。文字列を数に」
    it "cuts lines and words, tells letters, and reads numbers" $
      runLines
        [ "lines \"a\\nb c\\n\\nd\""
        , "words \"  a  b\\tc\\n\""
        , "map String.isLetter [\"a\", \"é\", \"λ\", \"1\", \",\", \"\"]"
        , "map String.toInt [\"42\", \"-7\", \"4x\", \"\", \"-\"]" ]
        `shouldBe`
          [ "[\"a\", \"b c\", \"\", \"d\"] : [String]"
          , "[\"a\", \"b\", \"c\"] : [String]"
          , "[true, true, true, false, false, false] : [Bool]"
          , "[Just 42, Just (-7), Nothing, Nothing, Nothing] : [Maybe Int]" ]
    -- ANCHOR_END: wordTests
    -- ANCHOR: wordfreqProp
    -- 「wordfreq.lam は、Haskell で数えたトップ10と同じものを見せる」
    it "shows the same top 10 as Haskell does" $
      forAll genWords $ \ls -> ioProperty $ do
        source <- Utf8.readFile "examples/script/wordfreq.lam"
        let world = emptyWorld { worldInput = ls }
        pure (outputOf (snd (script world source)) === haskellTop10 ls)
    -- ANCHOR_END: wordfreqProp
    -- ANCHOR: scriptGolden
    mapM_ (golden . ("script/" <>))
      [ "greet", "echo", "shop", "shout", "wc", "sum", "closing"
      , "average", "wordfreq" ]
    -- ANCHOR_END: scriptGolden

  describe "files, commands and clocks" $ do
    -- ANCHOR: pathTest
    -- 「Path の道具は、Haskell の filepath と同じ答え」
    it "joins and cuts paths as Haskell's filepath does" $
      property $ forAll (elements ["logs", "a/b", "a.tar.gz", "x/y.log", "."])
        $ \p -> forAll (elements ["a.log", "b", "c.txt"]) $ \q ->
          map valuePart (runLines
            [ "Path.join " <> renderString p <> " " <> renderString q
            , "Path.basename " <> renderString p
            , "Path.dirname " <> renderString p
            , "Path.extension " <> renderString p
            , "Path.withExtension \".bak\" " <> renderString p ])
            === map (renderString . T.pack)
                  [ T.unpack p </> T.unpack q, takeFileName (T.unpack p)
                  , takeDirectory (T.unpack p), takeExtension (T.unpack p)
                  , replaceExtension (T.unpack p) ".bak" ]
    -- ANCHOR_END: pathTest

    -- ANCHOR: folderTest
    -- 「偽物の世界のフォルダー：作る、見る、名前を変える、消す」
    it "makes, lists, renames and removes in the fake world" $ do
      let world = emptyWorld { worldFiles = textFiles
            [("logs/a.log", "a"), ("logs/b.txt", "b")] }
          (out, world') = runLinesIn world
            [ "File.list \"logs\"", "File.list \"./logs/../logs\""
            , "File.glob \"logs/*.log\"", "File.makeDir \"old/2026\""
            , "File.rename \"logs/a.log\" \"old/2026/a.log\""
            , "File.list \"old\"", "File.remove \"logs\""
            , "File.remove \"logs/b.txt\"", "File.remove \"logs\""
            , "File.isDir \"logs\"", "File.exists \"old/2026/a.log\""
            , "File.list \"nope\"" ]
      map valuePart out `shouldBe`
        [ "Ok [\"a.log\", \"b.txt\"]", "Ok [\"a.log\", \"b.txt\"]"
        , "Ok [\"logs/a.log\"]", "Ok ()", "Ok ()", "Ok [\"2026\"]"
        , "Err \"logs: the folder is not empty\"", "Ok ()", "Ok ()"
        , "false", "true"
        , "Err \"nope: no such folder\"" ]
      Map.keys (worldFiles world') `shouldBe` ["old/2026/a.log"]
    -- ANCHOR_END: folderTest

    -- ANCHOR: infoTest
    -- 「File.info はレコード。大きさは UTF-8 のバイト」
    it "gives the size in UTF-8 bytes, and when it was changed" $
      map valuePart (fst (runLinesIn emptyWorld
        { worldFiles = textFiles [("a.txt", "ラムダ\n")]
        , worldTimes = Map.fromList [("a.txt", 0)] }
        [ "File.info \"a.txt\"", "File.info \"b.txt\"" ]))
        `shouldBe`
          [ "Ok { modified = 1970-01-01T00:00:00Z, size = 10 }"
          , "Err \"b.txt: no such file\"" ]
    -- ANCHOR_END: infoTest

    -- ANCHOR: cleanupDryRun
    -- 「--dry-run の片付け：何も消さずに、消すはずのものを知らせる」
    it "removes nothing with --dry-run, and tells what it would" $ do
      source <- Utf8.readFile "examples/files/cleanup.lam"
      logs   <- mapM (\(p, t) -> (\x -> (p, (x, t))) <$> Utf8.readFile (T.unpack p))
                  [ ("examples/files/logs/app-2026-11-01.log", 1793524200000)
                  , ("examples/files/logs/app-2026-11-17.log", 1794881400000) ]
      let world = emptyWorld
            { worldFiles = textFiles [ (p, x) | (p, (x, _)) <- logs ]
            , worldTimes = Map.fromList [ (p, t) | (p, (_, t)) <- logs ]
            , worldArgs  = ["--dry-run", "examples/files/logs"] }
          (ending, done) = script world source
      ending `shouldBe` Finished ""
      Map.keys (worldFiles done) `shouldBe` map fst logs
      outputOf done `shouldBe` T.unlines
        [ "would remove examples/files/logs/app-2026-11-01.log"
            <> " (15 days, 66 bytes)"
        , "1 of 2 files: 7 days or more" ]
    -- ANCHOR_END: cleanupDryRun

    -- ANCHOR: processTest
    -- 「Process：3つの終わり方と、output」
    it "tells the three endings of a command apart" $ do
      let world = emptyWorld { worldCommands = Map.fromList
            [ (["ls"], (0, "a\n", "")), (["ls", "x"], (1, "", "no x\n")) ] }
      map valuePart (fst (runLinesIn world
        [ "Process.run \"ls\" []", "Process.run \"ls\" [\"x\"]"
        , "Process.run \"nope\" []"
        , "Process.output (Failed { code = 1, out = \"\", err = \"no x\\n\" })" ]))
        `shouldBe`
          [ "Succeeded { err = \"\", out = \"a\\n\" }"
          , "Failed { code = 1, err = \"no x\\n\", out = \"\" }"
          , "CannotStart \"nope: no such command\""
          , "Err \"exit 1: no x\"" ]
    -- ANCHOR_END: processTest

    -- ANCHOR: timeTest
    -- 「Time：ISO 8601 で書いて読み戻すと同じ。差は切り捨て」
    it "reads back the ISO 8601 it writes" $
      property $ forAll (chooseInteger (0, 4102444800000)) $ \ms ->
        map valuePart (runLines
          [ "Time.fromIso (Time.toIso (Time.fromMillis "
              <> T.pack (show ms) <> ")) == Ok (Time.fromMillis "
              <> T.pack (show ms) <> ")" ]) === ["true"]
    it "reads a zone, and cuts the days down" $
      map valuePart (runLines
        [ "Time.fromIso \"2026-11-17T12:00:00+09:00\""
        , "Time.fromIso \"2026-11-17\" |> Result.map Time.toIso"
        , "Time.fromIso \"yesterday\""
        , "Time.diffDays (Time.fromMillis 172799999) (Time.fromMillis 0)"
        , "Time.format Time.utc (Time.fromMillis 0)"
        , "f\"{Time.fromMillis 1500} in {Time.utc}\"" ])
        `shouldBe`
          [ "Ok 2026-11-17T03:00:00Z", "Ok \"2026-11-17T00:00:00Z\""
          , "Err \"not an ISO 8601 time: yesterday\"", "1"
          , "\"1970-01-01 00:00:00 +00:00\""
          , "\"1970-01-01T00:00:01.500Z in +00:00\"" ]
    -- ANCHOR_END: timeTest

    -- ANCHOR: randomTest
    -- 「Random：範囲の中で、同じ種なら同じ答え。shuffle は並べ替えるだけ」
    it "throws dice in the range, the same for the same seed" $
      property $ \seed (NonNegative lo) (Positive w) ->
        let world = emptyWorld { worldRandom = mkStdGen seed }
            lines' = [ "Random.int " <> num lo <> " " <> num (lo + w)
                     , "Random.shuffle [1, 2, 3, 4, 5, 6, 7]" ]
            out = map valuePart (fst (runLinesIn world lines'))
            (n, shuffled) = case out of
              [a, b] -> ( read (T.unpack a) :: Integer
                        , T.splitOn ", " (T.dropEnd 1 (T.drop 1 b)) )
              _      -> (-1, [])
        in n >= lo .&&. n <= lo + w
             .&&. sortOn id shuffled === map (T.pack . show) [1 .. 7 :: Int]
             .&&. out === map valuePart (fst (runLinesIn world lines'))
    -- ANCHOR_END: randomTest

    -- ANCHOR: filesGolden
    mapM_ (golden . ("files/" <>)) ["clock", "cleanup", "authors"]
    -- ANCHOR_END: filesGolden

  describe "big files and bytes" $ do
    -- ANCHOR: replLines
    -- 「本文の REPL の行に、本文のとおりに答える」
    it "answers the REPL lines of the chapter" $ do
      files <- mapM (\p -> (,) p <$> B.readFile (T.unpack p))
        [ "examples/streams/access.log", "examples/streams/bad.log"
        , "examples/streams/photos/lam.png" ]
      let world = emptyWorld { worldFiles = Map.fromList files }
          -- The SHA-256 of "abc" (the example of FIPS 180-2).
          -- "abc" の SHA-256（FIPS 180-2 の例）
          abc = "\"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f2"
                  <> "0015ad\" : String"
      fst (runLinesIn world
        [ "File.withFile \"examples/streams/access.log\" File.readLine"
        , "File.foldLines (\\n _ -> n + 1) 0 \"examples/streams/access.log\""
        , "Bytes.fromString \"ラムダ\""
        , "(length \"ラムダ\", length (Bytes.fromString \"ラムダ\"))"
        , "Bytes.toString (Bytes.fromList [99, 97, 102, 233])"
        , "Bytes.decode Replace (Bytes.fromList [99, 97, 102, 233])"
        , "Bytes.decode Ignore (Bytes.fromList [99, 97, 102, 233])"
        , "Bytes.decode Strict (Bytes.fromList [99, 97, 102, 233])"
        , "readFile \"examples/streams/bad.log\""
        , "File.foldLines (\\n _ -> n + 1) 0 \"examples/streams/bad.log\""
        , "File.foldLinesWith Replace (\\n _ -> n + 1) 0 "
            <> "\"examples/streams/bad.log\""
        , "Hash.sha256 (Bytes.fromString \"abc\") |> Bytes.toHex"
        , "let half = Hash.add (Bytes.fromString \"a\") Hash.start"
        , "Hash.add (Bytes.fromString \"bc\") half |> Hash.finish |> Bytes.toHex"
        , "Hash.file \"examples/streams/photos/lam.png\""
        , "Base64.encode (Bytes.fromString \"Lam!\")"
        , "Base64.decode \"TGFtIQ==\" |> Result.andThen Bytes.toString"
        , "Base64.decode \"TGFt!Q==\""
        , "Uuid.v4" ])
        `shouldBe`
          [ "Ok (Just \"2026-11-20T00:00:01Z GET /books/12 200 143ms\")"
              <> " : Result String (Maybe String)"
          , "Ok 8 : Result String Int"
          , "<9 bytes: e383a9e383a0e38380> : Bytes"
          , "(3, 9) : (Int, Int)"
          , "Err \"not UTF-8 from byte 3\" : Result String String"
          , "\"caf\65533\" : String"
          , "\"caf\" : String"
          , "error: Bytes.decode: not UTF-8 from byte 3"
          , "Err \"examples/streams/bad.log: not UTF-8 from byte 74\""
              <> " : Result String String"
          , "error: examples/streams/bad.log: line 2: not UTF-8 from byte 29"
          , "Ok 3 : Result String Int"
          , abc
          , "half : Hasher"
          , abc
          , "Ok <32 bytes: 4dcf4746ed529a95d38323c95f9e3ee6…>"
              <> " : Result String Bytes"
          , "\"TGFtIQ==\" : String"
          , "Ok \"Lam!\" : Result String String"
          , "Err \"not base64: invalid character at offset: 4\""
              <> " : Result String Bytes"
          , "\"72318d2a-0390-4070-a1ab-385bfeb0c5d6\" : String" ]
    -- ANCHOR_END: replLines

    -- ANCHOR: handleTest
    -- 「ハンドルは1行ずつ読み、閉じたら読めない。止まっても閉じる」
    it "reads line by line, closes, and closes even after a stop" $ do
      let world = emptyWorld
            { worldFiles = textFiles [("a.txt", "one\r\ntwo\nthree")] }
          (out, done) = runLinesIn world
            [ withA "mapM (\\_ -> File.readLine h) [1, 2, 3, 4]"
            , withA "do File.close h; File.readLine h"
            , withA "do _ <- File.readLine h; error \"boom\""
            , withA "mapM (\\_ -> File.readChunk 4 h) [1, 2, 3, 4, 5]"
            , "File.open \"none.txt\"" ]
          withA body = "File.withFile \"a.txt\" (\\h -> " <> body <> ")"
      map valuePart out `shouldBe`
        [ "Ok [Just \"one\", Just \"two\", Just \"three\", Nothing]"
        , "error: handle 2 is closed"
        , "error: boom"
        , "Ok [Just <4 bytes: 6f6e650d>, Just <4 bytes: 0a74776f>, "
            <> "Just <4 bytes: 0a746872>, Just <2 bytes: 6565>, Nothing]"
        , "Err \"none.txt: no such file\"" ]
      worldHandles done `shouldBe` Map.empty
    -- ANCHOR_END: handleTest

    -- ANCHOR: bytesProps
    -- 「文字 → バイト列 → 文字 で元どおり。Replace と Ignore は止まらない」
    it "turns text into bytes and back, and Replace and Ignore never stop" $
      property $ \(t, ws) ->
        let text = T.pack t
            raw  = B.pack ws
        in decodeWith Strict (encodeUtf8 text) === Right text
             .&&. isRight (decodeWith Replace raw)
             .&&. isRight (decodeWith Ignore raw)
             .&&. (isRight (decodeWith Strict raw)
                     === (decodeWith Strict raw == decodeWith Replace raw))

    -- 「少しずつのハッシュは、一度のハッシュと同じ。base64 も元に戻る」
    it "hashes a little at a time as at once, and base64 comes back" $
      property $ \(ws, cut) ->
        let list = "[" <> T.intercalate ", " (map (T.pack . show) ws) <> "]"
            n    = T.pack (show (cut `mod` (length ws + 1)))
            bs   = "(Bytes.fromList " <> list <> ")"
            end  = T.pack (show (length ws))
        in map valuePart (runLines
             [ "Hash.sha256 " <> bs <> " |> Bytes.toHex"
             , "Hash.start |> Hash.add (Bytes.slice 0 " <> n <> " " <> bs
                 <> ") |> Hash.add (Bytes.slice " <> n <> " " <> end <> " " <> bs
                 <> ") |> Hash.finish |> Bytes.toHex"
             , "Base64.decode (Base64.encode " <> bs
                 <> ") |> Result.map Bytes.toList" ])
           === [ T.pack (show (hexOf (Sha.hash (B.pack ws))))
               , T.pack (show (hexOf (Sha.hash (B.pack ws))))
               , "Ok " <> list ]
    -- ANCHOR_END: bytesProps

    -- ANCHOR: uuidTest
    -- 「UUID の7つ目は 4 で、9つ目は 8・9・a・b で始まる」
    it "makes UUIDs of version 4 and variant 1" $
      property $ \seed ->
        let world = emptyWorld { worldRandom = mkStdGen seed }
            out   = T.concat (map valuePart (fst (runLinesIn world ["Uuid.v4"])))
            parts = T.splitOn "-" (T.dropEnd 1 (T.drop 1 out))
        in map T.length parts === [8, 4, 4, 4, 12]
             .&&. T.take 1 (parts !! 2) === "4"
             .&&. T.take 1 (parts !! 3) `elem` ["8", "9", "a", "b"]
    -- ANCHOR_END: uuidTest

    -- ANCHOR: bigFile
    -- 「大きなファイル（テストの中で作る）を、本物の世界で1行ずつ数える」
    it "counts a big file of the real world, one line at a time" $ do
      dir <- getTemporaryDirectory
      let path = dir </> "lam-streams-big.log"
          statuses = cycle ["200", "200", "304", "404", "200", "500"]
          lines' = [ "t GET / " <> s <> " 1ms" | s <- take 200000 statuses ]
      B.writeFile path (encodeUtf8 (T.unlines lines'))
      source <- Utf8.readFile "examples/streams/count.lam"
      dice   <- newDice (Just 618)
      desk   <- newDesk
      screen <- newIORef ""
      -- The real world, but the screen and the arguments are ours.
      -- 本物の世界。ただし画面と引数は、こちらで持つ
      let handle request = case request of
            PutStr t -> VTuple [] <$ modifyIORef' screen (<> t)
            GetArgs  -> pure (VList [VString (T.pack path)])
            _        -> realWorld dice desk request
      loaded <- loadProgram (diskFiles []) preludeSession preludeSession
                  "count.lam" source
      ending <- case loaded of
        Left err              -> pure (Errored err)
        Right (done, final)   -> runProgram handle done final
      removeFile path
      ending `shouldBe` Finished ""
      readIORef screen `shouldReturn`
        "200 100,001\n304 33,333\n404 33,333\n500 33,333\n"
    -- ANCHOR_END: bigFile

    -- ANCHOR: streamsGolden
    mapM_ (golden . ("streams/" <>))
      ["count", "slurp", "errors", "dupes", "inline"]
    -- ANCHOR_END: streamsGolden

  -- 「トレーニングの解答」
  describe "answers" $ do
    mapM_ (golden . ("streams/answers/" <>)) ["wc", "head", "verify"]
    -- ANCHOR: answerGunzip
    -- 「gzip で縮めたものを、元に戻す。壊れたものは Err」
    it "makes gzip bytes whole again, and says Err for broken ones" $
      property $ \ws ->
        let raw  = B.pack ws
            gz   = BL.toStrict (GZip.compress (BL.fromStrict raw))
            run b = either (const "stopped") renderValue
                      (apply (builtinValue gunzipBuiltin) (VBytes b))
        in run gz === renderValue (VCon "Ok" 0 [VBytes raw])
             .&&. T.isPrefixOf "Err \"not gzip: " (run (B.drop 1 gz))
    -- ANCHOR_END: answerGunzip

  describe "program" $
    -- ANCHOR: layout
    -- 「1桁目から始まる行は、新しい定義（か、最後の式）」
    it "starts a new definition at column 1" $
      fmap (\(decls, final) ->
              ([ x | DefValue x _ <- decls ], fmap render final))
        (runLamParser programParts "" "let a = 1\nlet b =\n  a + 1\nb\n")
        `shouldBe` Right (["a", "b"], Just "b")
    -- ANCHOR_END: layout

-- 📜 A program as its declarations and its last expression.
-- プログラムを、宣言と最後の式の組に
programParts :: Parser ([Decl], Maybe Expr)
programParts = (\p -> (programDecls p, programFinal p)) <$> program

-- ANCHOR: runModules
-- 🗂️ Load main.lam with these files (the modules) around it: what
--    the expression at the end shows, or the error.
-- この（モジュールの）ファイルを置いて、main.lam を読みこむ。最後の式が
-- 見せるものか、エラー
runModules :: [(FilePath, Text)] -> Text -> Either Text Text
runModules files source =
  fmap (fromMaybe "" . snd) $ (>>= finalText) $ runIdentity $
    loadProgram (mapFiles (Map.fromList files) []) preludeSession
      preludeSession "main.lam" source

-- 🗂️ The same, with Shapes.lam.
-- 同じく、Shapes.lam を置いて
withShapes :: Text -> Either Text Text
withShapes = runModules [("Shapes.lam", shapes)]

-- 🔷 A module with a type, a class, two instances, and a secret.
-- 型、クラス、インスタンス2つ、それに秘密のあるモジュール
shapes :: Text
shapes = T.unlines
  [ "module Shapes exposing (Shape (..), Describe (..), area, square)"
  , "data Shape = Circle Int | Rect Int Int"
  , "class Describe a where"
  , "  describe : a -> String"
  , "instance Describe Shape where"
  , "  describe s = case s of | Circle r -> f\"circle {r}\""
  , "    | Rect w h -> f\"rect {w} {h}\""
  , "instance Display Shape where"
  , "  display s = f\"({describe s})\""
  , "area s = case s of | Circle r -> 3 * r * r | Rect w h -> w * h"
  , "square n = Rect n n"
  , "secret = 42" ]
-- ANCHOR_END: runModules

-- ⌨️ REPL lines, where an import line looks in these files.
-- REPL の行。import の行は、このファイルから探す
replImporting :: [(FilePath, Text)] -> [Text] -> IO [Text]
replImporting files inputs = pure (snd (foldl go (preludeSession, []) inputs))
  where
    go (s, outs) line = case parseInput line of
      Right (ImportLine imp) ->
        case runIdentity (importAt (mapFiles (Map.fromList files) [])
                            preludeSession s imp) of
          Left err -> (s, outs <> [err])
          Right s' -> (s', outs <> [""])
      _ -> let (s', out) = replStep s line in (s', outs <> [out])

-- ANCHOR: replAfter
-- 📂 :load a file, then answer some REPL lines.
-- ファイルを :load してから、REPL の行に答える
replAfter :: FilePath -> [Text] -> IO [Text]
replAfter file inputs = do
  source <- Utf8.readFile file
  let loaded = case loadPlain preludeSession file source of
        Right (s, _) -> s
        Left err     -> error (T.unpack err)
      go (s, outs) line = let (s', out) = replStep s line in (s', outs <> [out])
  pure (snd (foldl go (loaded, []) inputs))

-- 🖨️ A list of numbers as Lam source (negative ones in brackets).
-- 数のリストを Lam のソースに（負の数は括弧で）
lamList :: [Integer] -> Text
lamList xs = "[" <> T.intercalate ", " (map num xs) <> "]"
-- ANCHOR_END: replAfter

-- ANCHOR: genMaybe
-- 🎲 Nothing, or Just a small number (a negative one, too).
-- Nothing か、小さな数の Just（負の数も）
genMaybe :: Gen (Maybe Integer)
genMaybe = oneof [pure Nothing, Just <$> choose (-3, 3)]

-- 🖨️ Maybe and Bool, as Lam writes them.
-- Maybe と Bool を、Lam の書き方で
lamMaybe :: Maybe Integer -> Text
lamMaybe m = maybe "Nothing" (\n -> "Just " <> num n) m

lamBool :: Bool -> Text
lamBool b = if b then "true" else "false"
-- ANCHOR_END: genMaybe

-- 🖨️ A list of numbers, as Lam writes it: [1, 2, 3]
-- 数のリストを、Lam の書き方で
renderList :: [Integer] -> Text
renderList xs = "[" <> T.intercalate ", " (map (T.pack . show) xs) <> "]"

-- ANCHOR: inRecord
-- 📇 let r = <record> in <expression>, for the REPL.
-- REPL のため
inRecord :: Text -> Text -> Text
inRecord r e = "let r = " <> r <> " in " <> e

-- 🔢 A number as Lam source code (a negative one is (-5): the minus
--    right before the number), and the value part of an answer.
-- 数を Lam のソースに（負の数は (-5)）。答えから、値の部分を取り出す
num :: Integer -> Text
num n
  | n < 0     = "(" <> T.pack (show n) <> ")"
  | otherwise = T.pack (show n)

valuePart :: Text -> Text
valuePart = T.strip . fst . T.breakOn " : "
-- ANCHOR_END: inRecord

-- ANCHOR: genResult
-- 🎲 Ok with a small number, or Err with a short word.
-- 小さな数の Ok か、短い言葉の Err
genResult :: Gen (Either Text Integer)
genResult = oneof [ Right <$> choose (-3, 3)
                  , Left <$> elements ["no", "bad", "oops"] ]

-- 🖨️ A Result as Lam writes it, and a list of them as the REPL shows.
-- Result を Lam の書き方で。リストの Result を REPL の見せ方で
lamResult :: Either Text Integer -> Text
lamResult r = case r of
  Right n -> "Ok " <> num n
  Left e  -> "Err \"" <> e <> "\""

lamResultList :: Either Text [Integer] -> Text
lamResultList r = case r of
  Right ns -> "Ok [" <> T.intercalate ", " (map (T.pack . show) ns) <> "]"
  Left e   -> "Err \"" <> e <> "\""

-- 🏷️ A name with a number:  named "x" 2  is  x2
-- 番号の付いた名前
named :: Text -> Int -> Text
named x i = x <> T.pack (show i)
-- ANCHOR_END: genResult
