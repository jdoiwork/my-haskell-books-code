-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import Data.Char (isAlpha, toLower)
import Data.Either (isLeft)
import Data.Functor.Identity (runIdentity)
import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Scientific (scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Control.Exception as E
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Timeout (timeout)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time (UTCTime)

import Lam.Builtins (preludeConstructors)
import Lam.Format (center, groupDigits, padLeft, padRight, zeroPad)
import Lam.Json (fromAeson, parseJson, toAeson)
import Lam.Parser
import Lam.Pcre (Cut (..), Regex (..), compileRegex)
import Lam.Prelude
import Lam.Pretty
import Lam.Stdlib (splitOn)
import Lam.Program
import Lam.Module
import Lam.OneLiner
import Lam.Script (runScriptFlag, scriptOf)
import Lam.Repl
import Lam.Syntax
import Lam.Types
import Lam.World
import Lam.Guard
import Lam.Sqlite
import Lam.Eval (Cell (..), OpenMode (..), Request (..), Value (..), renderValue)
import Data.Maybe (fromMaybe)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Control.Monad.State.Strict (modify, runState)
import System.FilePath (dropExtension, replaceExtension, takeDirectory,
                        takeExtension, takeFileName, (</>))
import System.Random (mkStdGen)
import Lam.Files (globRoot)
import Lam.Answers (shortRecord)
import Text.Megaparsec (eof)
import Data.Time (diffUTCTime, getCurrentTime)
-- ANCHOR_END: head

-- ANCHOR: run
-- 🔁 Type the lines into a REPL with the prelude, one by one.
--    IO is done in a fake world, with nothing to read.
-- プレリュード入りの REPL に、1行ずつ打ちこむ。
-- IO は、読むものが何もない偽物の世界でやる
runLines :: [Text] -> [Text]
runLines inputs = fst (runLinesIn emptyWorld inputs)

-- 🎭 The same, in a given fake world. The answer has the world after.
-- 同じだが、渡した偽物の世界で。答えには、あとの世界も入る
runLinesIn :: FakeWorld -> [Text] -> ([Text], FakeWorld)
runLinesIn world inputs = runFake world (go preludeSession inputs)
  where
    go _ [] = pure []
    go s (line : rest) = do
      (s', out) <- step fakeWorld s line
      (out :) <$> go s' rest
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
-- 📜 Run an example program, as `lam file.lam args...` would show it,
--    but in a fake world: the keyboard types the lines of the .in file
--    (if there is one), the arguments are the lines of the .args file
--    (if there is one), the environment is the KEY=VALUE lines of the
--    .env file (if there is one), and the files are
--    examples/orders.json and every .lam file under examples/ (for
--    the modules). The standard error comes after the standard output,
--    and exit n (but not 0) is shown at the end as [exit n].
-- 例のプログラムを、lam file.lam 引数... と同じように動かす。ただし
-- 偽物の世界で。キーボードは .in ファイルの行（あれば）を打ち、引数は
-- .args ファイルの行（あれば）、環境変数は .env ファイルの KEY=VALUE の
-- 行（あれば）、ファイルは examples/orders.json と、examples/ の下の
-- .lam ファイル全部（モジュールのため）。
-- 標準エラーは標準出力の後ろに、exit n（0 のほか）は最後に [exit n] と見せる
runExample :: FilePath -> IO Text
runExample base = do
  dbs <- memoryDatabases
  fst <$> runExampleWith dbs id base

-- 🎭 The same, with databases given (to run twice on the same ones),
--    and a change to the world before the run. The answer has the
--    world after. <base>.web is the fake web (URL, status, file),
--    <base>.flags the flags (--allow-net, ...), and <base>.files more
--    files to put in the fake world.
-- 同じだが、データベースを渡し（同じもので2回動かすため）、動かす前に
-- 世界を変えられる。答えには、あとの世界も入る。<base>.web は偽物の
-- Web（URL・状態コード・ファイル）、<base>.flags はフラグ、<base>.files
-- は偽物の世界に置くファイル
runExampleWith
  :: Databases -> (FakeWorld -> FakeWorld) -> FilePath
  -> IO (Text, FakeWorld)
runExampleWith dbs = runExampleFull dbs id

-- 🎭 The same, with a change to the flags too (--dry-run, ...).
-- 同じだが、フラグも変えられる（--dry-run など）
runExampleFull
  :: Databases -> (Options -> Options) -> (FakeWorld -> FakeWorld)
  -> FilePath -> IO (Text, FakeWorld)
runExampleFull dbs changeOpts change base = do
  source <- Utf8.readFile (base <> ".lam")
  input  <- linesOf (base <> ".in")
  args   <- linesOf (base <> ".args")
  env    <- linesOf (base <> ".env")
  web    <- linesOf (base <> ".web") >>= mapM page
  flags  <- linesOf (base <> ".flags")
  extra  <- linesOf (base <> ".files") >>= mapM (\f -> (,) f <$> Utf8.readFile (T.unpack f))
  -- ANCHOR: goldenFiles
  -- <base>.times: a file (read from the disk) and when it was changed.
  -- <base>.run: a command and its answer (code, the file of its output).
  -- <base>.times：ファイル（ディスクから読む）と、変えた時刻。
  -- <base>.run：コマンドと、その答え（終了コード、出力のファイル）
  times  <- linesOf (base <> ".times") >>= mapM stamp
  runs   <- linesOf (base <> ".run") >>= mapM command
  -- ANCHOR_END: goldenFiles
  orders <- Utf8.readFile "examples/orders.json"
  modules <- lamFiles "examples"
  opts <- either fail (pure . fst) (readOptions (map T.unpack flags))
  let world = change emptyWorld
        { worldInput = input
        , worldArgs  = args
        , worldEnv   = Map.fromList [ (k, T.drop 1 v)
                                    | l <- env, let (k, v) = T.breakOn "=" l ]
        , worldFiles =
            Map.fromList (("examples/orders.json", orders) : modules <> extra
                           <> [ (p, t) | (p, t, _) <- times ])
        , worldTimes = Map.fromList [ (p, ms) | (p, _, ms) <- times ]
        , worldCommands = Map.fromList runs
        , worldWeb   = Map.fromList web }
  (result, world') <-
    runFakeIO world (runProgram (guarded (changeOpts opts) (fakeWorldIO dbs))
                       (fakeFiles world) preludeSession
                       (base <> ".lam") source)
  let shown = case result of
        Finished ""  -> ""
        Finished out -> out <> "\n"
        Errored err  -> err <> "\n"
        ExitedWith 0 -> ""
        ExitedWith n -> "[exit " <> T.pack (show n) <> "]\n"
  pure (outputOf world' <> errorsOf world' <> shown, world')
  where
    linesOf file = do
      there <- doesFileExist file
      if there then T.lines <$> Utf8.readFile file else pure []
    -- "URL 200 web/jma.json": the file is next to the .web file.
    -- ファイルは .web ファイルの隣から
    page l = case T.words l of
      [url, status, file] -> do
        body <- Utf8.readFile (takeDirectory base </> T.unpack file)
        pure (url, (read (T.unpack status), body))
      _ -> fail ("a bad line in " <> base <> ".web: " <> T.unpack l)
    -- "examples/files/logs/a.log 2026-11-01T09:10:00Z"
    stamp l = case T.words l of
      [path, iso] -> case (iso8601ParseM (T.unpack iso) :: Maybe UTCTime) of
        Just t -> do
          text <- Utf8.readFile (T.unpack path)
          pure (path, text, floor (utcTimeToPOSIXSeconds t * 1000))
        Nothing -> fail ("a bad time in " <> base <> ".times: " <> T.unpack l)
      _ -> fail ("a bad line in " <> base <> ".times: " <> T.unpack l)
    -- "0 gitlog.txt git log --format=%an": the file is next to the .run.
    -- ファイルは .run ファイルの隣から
    command l = case T.words l of
      code : file : cmd -> do
        out <- Utf8.readFile (takeDirectory base </> T.unpack file)
        pure (cmd, (read (T.unpack code), out, ""))
      _ -> fail ("a bad line in " <> base <> ".run: " <> T.unpack l)

-- 📂 Every .lam file under a folder: its path and its text.
-- フォルダーの下の .lam ファイル全部：道と中身
lamFiles :: FilePath -> IO [(Text, Text)]
lamFiles dir = do
  names <- listDirectory dir
  concat <$> mapM (\n -> do
    let path = dir <> "/" <> n
    folder <- doesDirectoryExist path
    if folder then lamFiles path
    else if ".lam" `T.isSuffixOf` T.pack n
      then (\t -> [(T.pack path, t)]) <$> Utf8.readFile path
      else pure []) names

-- 🥇 Golden test: the output must be the same as the .out file.
-- ゴールデンテスト：出力が .out ファイルと同じであること
golden :: String -> Spec
golden base =
  it ("examples/" <> base <> ".lam") $ do
    actual   <- runExample ("examples/" <> base)
    expected <- Utf8.readFile ("examples/" <> base <> ".out")
    actual `shouldBe` expected
-- ANCHOR_END: golden

-- 🔢 Numbers and lists of numbers, written as Lam reads them
--    (a negative number is 0 - n: Lam has no negative literal).
-- Lam が読める形の数と、数のリスト（負の数は 0 - n）
lamInt :: Integer -> Text
lamInt n = if n < 0 then "(0 - " <> T.pack (show (negate n)) <> ")"
           else T.pack (show n)

lamInts :: [Integer] -> Text
lamInts xs = "[" <> T.intercalate ", " (map lamInt xs) <> "]"

-- ✂️ The value of a REPL answer, without its type (the type of []
--    is not [Int] but [a]).
-- REPL の答えの、型を外した値（[] の型は [Int] ではなく [a]）
-- 🔢 A number as Lam reads it (a negative one is 0 - n).
-- Lam が読む形の数（負の数は 0 - n）
num :: Integer -> Text
num n
  | n < 0     = "(0 - " <> T.pack (show (abs n)) <> ")"
  | otherwise = T.pack (show n)

valuePart :: Text -> Text
valuePart = fst . T.breakOn " : "

-- 🔢 The same, as the REPL shows them: -1 (and (-1) inside Just).
-- REPL が見せる形
shownInt :: Integer -> Text
shownInt = T.pack . show

shownInts :: [Integer] -> Text
shownInts xs = "[" <> T.intercalate ", " (map shownInt xs) <> "]"

-- 🔢 A Maybe Int, as the REPL shows it.
-- REPL が見せる Maybe Int
maybeInt :: Maybe Integer -> Text
maybeInt m = case m of
  Nothing -> "Nothing : Maybe Int"
  Just n | n < 0     -> "Just (" <> T.pack (show n) <> ") : Maybe Int"
         | otherwise -> "Just " <> T.pack (show n) <> " : Maybe Int"

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
      runLines inputs
        `shouldBe`
          [ "12 : Int", "true : Bool"
          , "add : Int -> Int -> Int", "3 : Int"
          , "in the definition of bad: type error: \
            \the types do not fit: Int where Bool is needed"
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
          , "type error: the types do not fit: Int where String is needed" ]
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
          , "type error: the types do not fit: Bool where Int is needed"
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
          , "Int -> Int -> [Int]" ]
    -- ANCHOR_END: listPrelude

    -- ANCHOR: listLikeHaskell
    -- 「Lam のリストの関数は、Haskell のものと同じ答えを出す」
    it "computes the same lists as Haskell" $
      -- Lam has no negative numbers to write, so 0 to 9.
      -- Lam には負の数の書き方がないので、0 から 9
      property $ forAll (listOf (choose (0, 9))) $ \xs ->
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
        [ "and false (1 / 0 == 0)"
        , "false && 1 / 0 == 0"
        , "true || 1 / 0 == 0"
        , "true && 1 / 0 == 0" ]
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
      , "result", "tree", "orders", "count", "cents", "hello", "report"
      , "receipt", "shoplog", "pair", "csv", "calc", "menu"
      , "wordfreq", "greet", "average", "echo", "shout", "sum", "wc"
      , "outside/weather", "outside/dex"
      , "guard/peek", "guard/sneaky", "guard/env"
      , "stdlib/signup", "stdlib/quiz", "stdlib/tally"
      , "files/clock", "files/cleanup", "files/authors"
      -- ANCHOR: recordGolden
      , "records/annotate", "records/report", "records/crew"
      , "records/broken/unnoted", "records/broken/noted"
      , "records/broken/first", "records/broken/swap"
      , "records/broken/largest", "records/broken/order"
      , "records/answers/annotated", "records/answers/describe"
      , "records/answers/describe-broken", "records/answers/scores"
      , "records/answers/members" ]
      -- ANCHOR_END: recordGolden
  -- ANCHOR_END: examples

  -- 「モジュール」
  describe "modules" $ do
    -- ANCHOR: moduleGolden
    mapM_ golden
      [ "wordfreq/wordfreq", "tools/vocab", "ring/shop", "clash/report"
      , "clash/fixed", "price/receipt", "price/broken" ]
    -- ANCHOR_END: moduleGolden

    -- ANCHOR: parseModule
    -- 「module と import の行を読む」
    it "reads the module line and the import lines" $
      fmap (\(Program h is _ _) -> (h, is))
        (runLamParser program "m.lam" (T.unlines
          [ "module Shapes exposing (Shape (..), area, Json (JInt, JStr))"
          , "import Json"
          , "import Json as J (get)" ]))
        `shouldBe` Right
          ( Just (Header (Loc "m.lam" 1 1) "Shapes" (ExposeSome
              [ ExposeType "Shape" AllConstructors, ExposeValue "area"
              , ExposeType "Json" (TheseConstructors ["JInt", "JStr"]) ]))
          , [ Import (Loc "m.lam" 2 1) "Json" Nothing Nothing
            , Import (Loc "m.lam" 3 1) "Json" (Just "J")
                (Just (ExposeSome [ExposeValue "get"])) ] )
    -- ANCHOR_END: parseModule

    -- ANCHOR: importNames
    -- 「import した名前の付き方：前にモジュールの名前、as なら別名だけ、
    --   かっこの一覧なら名前なしでも」
    it "names what it imports: with the module, the alias, or bare" $ do
      withShapes "import Shapes\nShapes.area (Shapes.square 3)"
        `shouldBe` Finished "9 : Int"
      withShapes "import Shapes as S\nS.area (S.Circle 2)"
        `shouldBe` Finished "12 : Int"
      -- Unlike Haskell, as takes the place of the name: Shapes. is gone.
      -- Haskell とちがい、as は名前の代わり。Shapes. は使えない
      withShapes "import Shapes as S\nShapes.area (S.Circle 2)"
        `shouldBe` Errored
          "main.lam:2:1: type error: unknown name: Shapes.area"
      withShapes "import Shapes (area, Shape (..))\narea (Rect 2 5)"
        `shouldBe` Finished "10 : Int"
      withShapes "import Shapes (area)\nsquare 3"
        `shouldBe` Errored "main.lam:2:1: type error: unknown name: square"
      withShapes (T.unlines [ "import Shapes as S (area)"
                            , "(area (S.Circle 1), S.area (S.square 2))" ])
        `shouldBe` Finished "(3, 4) : (Int, Int)"
      -- A ( at column 1 is the expression at the end, not the list.
      -- 1桁目の ( は、一覧ではなく、最後の式
      withShapes "import Shapes\n(Shapes.area (Shapes.square 2), 1)"
        `shouldBe` Finished "(4, 1) : (Int, Int)"
    -- ANCHOR_END: importNames

    -- ANCHOR: hiding
    -- 「見せていない名前は、ほかのファイルからは見えない」
    it "keeps the names a module does not expose" $ do
      withShapes "import Shapes\nShapes.secret"
        `shouldBe` Errored "type error: unknown name: Shapes.secret"
      withShapes "import Shapes (secret)\n1"
        `shouldBe` Errored
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
      case runLines [ "Json.mkDec 1999 2", ":browse Json" ] of
        [shown, browsed] -> do
          shown `shouldBe` "JDec 1999 2 : Json"
          last (T.lines browsed) `shouldBe` "(hidden: JDec)"
        other -> expectationFailure (show other)
    -- ANCHOR_END: hiding

    -- ANCHOR: patterns
    -- 「別名の付いた構成子も、パターンに書ける」
    it "matches constructors written with the module's name" $
      withShapes (T.unlines
        [ "import Shapes as S"
        , "let name s = case s of"
        , "  | S.Circle _ -> \"circle\""
        , "  | S.Rect _ _ -> \"rect\""
        , "(name (S.Circle 1), name (S.square 2))" ])
        `shouldBe` Finished "(\"circle\", \"rect\") : (String, String)"
    -- ANCHOR_END: patterns

    -- ANCHOR: finding
    -- 「探す順：まず隣、それから LAM_PATH のフォルダーを順に」
    it "looks next to the file first, then in LAM_PATH" $ do
      let next  = "module Greet exposing (hi)\nlet hi = \"next to it\""
          inLib = "module Greet exposing (hi)\nlet hi = \"in lib\""
          world files = emptyWorld
            { worldFiles = Map.fromList files
            , worldEnv = Map.fromList [("LAM_PATH", "lib:more")] }
          go files = fst (runFake (world files)
            (runProgram fakeWorld (fakeFiles (world files)) preludeSession
               "app/main.lam" "import Greet\nGreet.hi"))
      go [("app/Greet.lam", next), ("lib/Greet.lam", inLib)]
        `shouldBe` Finished "\"next to it\" : String"
      go [("more/Greet.lam", next), ("lib/Greet.lam", inLib)]
        `shouldBe` Finished "\"in lib\" : String"
      go [] `shouldBe` Errored ("app/main.lam:1:1: error: no module Greet "
        <> "(looked for app/Greet.lam, lib/Greet.lam, more/Greet.lam)")
    -- ANCHOR_END: finding

    -- ANCHOR: once
    -- 「2つのファイルが import しても、モジュールのファイルは1度だけ読む」
    it "reads a module file only once, even if two files import it" $ do
      let files = Map.fromList
            [ ("A.lam", "module A exposing (a)\nimport C\nlet a = C.c + 1")
            , ("B.lam", "module B exposing (b)\nimport C\nlet b = C.c + 2")
            , ("C.lam", "module C exposing (c)\nlet c = 10") ]
          logging = Files
            { readSource = \path -> do
                modify (path :)
                pure (Map.lookup (T.pack path) files)
            , searchPath = [] }
          (loaded, readLog) = runState
            (loadProgram logging preludeSession "main.lam"
               "import A\nimport B\nA.a + B.b") []
      fmap (fmap render . snd) loaded `shouldBe` Right (Just "A.a + B.b")
      reverse readLog `shouldBe` ["A.lam", "C.lam", "B.lam"]
    -- ANCHOR_END: once

    -- ANCHOR: moduleErrors
    -- 「モジュールのファイルのまちがいを、場所と一緒に教える」
    it "tells what is wrong with a module file, and where" $ do
      let with m = fst . runModules [("M.lam", m)]
      with "let x = 1" "import M\n1" `shouldBe` Errored
        ("M.lam:1:1: error: a module starts with the line  "
         <> "module M exposing (...)")
      with "module N exposing (x)\nlet x = 1" "import M\n1" `shouldBe`
        Errored "M.lam:1:1: error: the file M.lam must be the module M, not N"
      with "module M exposing (y)\nlet x = 1" "import M\n1" `shouldBe`
        Errored ("M.lam:1:1: error: the module M exposes y, "
                 <> "but does not define it")
      with "module M exposing (x)\nlet x = 1\nx" "import M\n1" `shouldBe`
        Errored ("M.lam: error: a module cannot end with an expression"
                 <> " (only a program can)")
      with "module M exposing (x)\nlet x = 1 / 0" "import M\n1" `shouldBe`
        Errored "M.lam:2:11: in the definition of x: error: divided by zero"
    -- ANCHOR_END: moduleErrors

    -- ANCHOR: clashTests
    -- 「2つのモジュールの同じ名前・同じ型は、エラーにする」
    it "stops when two modules give the same name or the same type" $ do
      let box m n = T.unlines
            [ "module " <> m <> " exposing (Box (..), size)"
            , "data Box = Box Int", "let size = " <> n ]
          a = box "A" "1"
          b = box "B" "2"
          go src = fst (runModules [("A.lam", a), ("B.lam", b)] src)
      go "import A (size)\nimport B (size)\n1"
        `shouldBe` Errored
          "main.lam:2:1: error: the name size comes from both A and B"
      go "import A\nimport B\n1" `shouldBe` Errored
        "main.lam:2:1: error: the type Box comes from both A and B"
      -- Only the names in front differ: A.size and B.size are fine.
      -- 前の名前だけがちがうなら、A.size と B.size は大丈夫
      go "import A as X\nX.size" `shouldBe` Finished "1 : Int"
      fst (runModules [] "data Maybe a = Nope\n1") `shouldBe` Errored
        ("in the data type Maybe: type error: "
         <> "the type Maybe belongs to the module Maybe")
    -- ANCHOR_END: clashTests

    -- ANCHOR: ringTest
    -- 「import の輪は、ぐるぐる回らずにエラーにする」
    it "finds imports that go around in a ring" $
      fst (runModules
        [ ("A.lam", "module A exposing (a)\nimport B\nlet a = 1")
        , ("B.lam", "module B exposing (b)\nimport A\nlet b = 2") ]
        "import A\n1")
        `shouldBe` Errored ("B.lam:2:1: error: the imports go around in a "
                            <> "ring: main.lam -> A.lam -> B.lam -> A.lam")
    -- ANCHOR_END: ringTest

    -- ANCHOR: exposedProp
    -- 「exposing に書いた名前だけが、ほかのファイルから見える」
    it "shows exactly the names in the exposing list" $
      property $ forAll (sublistOf ["one", "two", "three"]) $ \shown ->
        let header =
              "module Nums exposing (" <> T.intercalate ", " shown <> ")"
            nums = T.unlines
              [ header, "let one = 1", "let two = 2", "let three = 3" ]
            seen n = fst (runModules [("Nums.lam", nums)]
                           ("import Nums\nNums." <> n))
                       /= Errored ("type error: unknown name: Nums." <> n)
        in not (null shown) ==>
             [ n | n <- ["one", "two", "three"], seen n ] === shown
    -- ANCHOR_END: exposedProp

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
          , "Maybe Int -> Int" ]
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
          [ "scores : Map Int"
          , "{\"Python\": 82, \"TS\": 75} : Map Int"
          , "Just 75 : Maybe Int"
          , "Nothing : Maybe Int"
          , "true : Bool"
          , "Map a"
          , "true : Bool"
          , "type error: String -> String is not in the class Comparable"
          , "{\"the\": 3} : Map Int"
          , "{\"TS\": 2, \"name\": 1} : Map Int"
          , "type error: the types do not fit: Int where String is needed" ]
    -- ANCHOR_END: maps

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
          , "{\"a\": JInt 2} : Map Json" ]
    -- ANCHOR_END: parseJson

    -- ANCHOR: numbers
    -- 「数は、整数なら JInt、小数なら JDec。どちらもぴったり」
    it "reads whole numbers as JInt, and fractions as JDec, exactly" $
      runLines
        [ "Json.parse \"[42, 19.99, -0.5, 1.0, 1e3, 12345678901234567890]\""
        , "Json.show (JArr [Json.mkDec 1999 2, Json.mkDec (0 - 5) 1, "
            <> "JInt 1000])" ]
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

    -- ANCHOR: roundTrip
    -- 「aeson の値を Lam の Json にして戻すと、もとの値」
    it "turns aeson's values into Lam's Json and back" $
      property $ forAll genAeson $ \j ->
        toAeson (fromAeson j) == Right j
    -- ANCHOR_END: roundTrip

    -- ANCHOR: mkDec
    -- 「Json.mkDec は、Json.parse と同じ形の小数を作る」
    it "makes a number in the same shape as Json.parse" $
      runLines
        [ "JDec 19990 3"
        , "Json.mkDec 19990 3", "Json.mkDec 1900 2", "Json.mkDec 0 5"
        , "Json.mkDec 1999 2 == Json.mkDec 19990 3" ]
        `shouldBe`
          [ "type error: JDec is hidden by the module Json"
              <> " (it is not in its exposing list)"
          , "JDec 1999 2 : Json", "JInt 19 : Json", "JInt 0 : Json"
          , "true : Bool" ]

    -- 「どんな c と d でも、c × 10^(-d) を書いた JSON を読んだものと同じ」
    -- (JDec is hidden now, so Haskell writes the number.)
    -- （JDec は隠れたので、数は Haskell が書く）
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

    -- 「bind は値が先、andThen は関数が先。中身は同じ」
    it "has bind with the value first, and andThen with the function first" $
      runLines
        [ "let down n = if n > 0 then Just (n - 1) else Nothing"
        , "Maybe.bind (Just 3) down"
        , "Maybe.andThen down (Just 3)"
        , "Maybe.bind (Just 0) down" ]
        `shouldBe`
          [ "down : Int -> Maybe Int"
          , "Just 2 : Maybe Int", "Just 2 : Maybe Int", "Nothing : Maybe Int" ]
    -- 「Result と IO にも、値が先の bind と関数が先の andThen がある」
    it "has bind and andThen for Result and IO, too" $
      runLines
        [ ":type Result.bind", ":type Result.andThen", ":type IO.andThen"
        , "Result.bind (Ok 1) (\\n -> Ok (n + 1))"
        , "IO.andThen (\\x -> pure (x + 1)) (pure 1)" ]
        `shouldBe`
          [ "Result a b -> (b -> Result a c) -> Result a c"
          , "(a -> Result b c) -> Result b a -> Result b c"
          , "(a -> IO b) -> IO a -> IO b"
          , "Ok 2 : Result a Int", "2 : Int" ]

  -- 「入出力」
  describe "IO" $ do
    -- ANCHOR: doSugar
    -- 「do は、bind と関数に開く」
    it "opens do into bind and functions" $
      map (fmap render . parseExpr)
        [ "do x <- getLine; putStrLn x"
        , "do putStr \"a\"; let b = \"b\"; putStrLn b"
        , "do\n  name <- getLine\n  putStrLn (fromMaybe \"\" name)" ]
        `shouldBe` map Right
          [ "bind getLine (\\x -> putStrLn x)"
          , "bind (putStr \"a\") (\\_ -> let b = \"b\" in putStrLn b)"
          , "bind getLine (\\name -> putStrLn (fromMaybe \"\" name))" ]
    -- ANCHOR_END: doSugar

    -- ANCHOR: keywordStart
    -- 「キーワードで始まる名前も読める（double、iffy、letter）」
    it "reads names that start with a keyword" $
      runLines
        [ "let double x = x * 2", "double 4"
        , "let iffy = 1", "iffy + 1", "let letter = \"a\"", "letter" ]
        `shouldBe`
          [ "double : Int -> Int", "8 : Int", "iffy : Int", "2 : Int"
          , "letter : String", "\"a\" : String" ]
    -- ANCHOR_END: keywordStart

    -- ANCHOR: ioTypes
    -- 「IO の値の型」
    it "gives IO values their types" $
      runLines
        [ ":type putStrLn", ":type getLine", ":type bind"
        , ":type do x <- pure 1; pure (x + 1)"
        , "do x <- pure 1"
        , "putStrLn \"hi\" == putStrLn \"hi\"" ]
        `shouldBe`
          [ "String -> IO ()", "IO (Maybe String)"
          , "IO a -> (a -> IO b) -> IO b", "IO Int"
          , "<repl>:1:15:\n  |\n1 | do x <- pure 1\n  |               ^\n\
            \the last line of do must be an expression\n"
          , "type error: IO () is not in the class Comparable" ]
    -- ANCHOR_END: ioTypes

    -- ANCHOR: makingIsNotDoing
    -- 「作るだけでは、何も起きない。やるたびに起きる」
    it "does nothing when an action is made, and does it each time" $
      runLinesIn emptyWorld
        [ "let hi = putStrLn \"hi\""
        , "[hi, hi, hi]"
        , "do hi; hi" ]
        `shouldBe`
          ( [ "hi : IO ()"
            , "[<IO action>, <IO action>, <IO action>] : [IO ()]"
            , "" ]
          , emptyWorld { worldOutput = ["hi\n", "hi\n"] } )
    -- ANCHOR_END: makingIsNotDoing

    -- ANCHOR: fakeFiles
    -- 「偽物の世界のファイルを、読んで書く」
    it "reads and writes the files of a fake world" $ do
      let world = emptyWorld
            { worldFiles = Map.fromList [("memo.txt", "λ memo\n")] }
          (outs, world') = runLinesIn world
            [ "readFile \"memo.txt\""
            , "readFile \"nothing.txt\""
            , "writeFile \"new.txt\" \"hello\"" ]
      outs `shouldBe`
        [ "Ok \"λ memo\\n\" : Result String String"
        , "Err \"nothing.txt: no such file\" : Result String String"
        , "Ok () : Result String ()" ]
      worldFiles world' `shouldBe`
        Map.fromList [("memo.txt", "λ memo\n"), ("new.txt", "hello")]
    -- ANCHOR_END: fakeFiles

    -- ANCHOR: echo
    -- 「読んだ行を、そのまま全部書き出す（でたらめな行で）」
    it "echoes every line it reads" $
      forAll (listOf (T.filter (/= '\n') <$> genText)) $ \ls ->
        let echo = "let echo = do line <- getLine; case line of \
                   \| Nothing -> pure () | Just s -> do putStrLn s; echo"
            (_, world') = runLinesIn emptyWorld { worldInput = ls }
                            [echo, "echo"]
        in outputOf world' === T.concat [ l <> "\n" | l <- ls ]
    -- ANCHOR_END: echo

    -- ANCHOR: report
    -- 「report.lam は、偽物の世界に report.json を書く」
    it "writes report.json into the fake world" $ do
      source <- Utf8.readFile "examples/report.lam"
      orders <- Utf8.readFile "examples/orders.json"
      let world = emptyWorld
            { worldFiles = Map.fromList [("examples/orders.json", orders)] }
          (result, world') =
            runFake world (runProgram fakeWorld (fakeFiles world)
                             preludeSession "report.lam" source)
      result `shouldBe` Finished ""
      Map.lookup "report.json" (worldFiles world')
        `shouldBe` Just "{\"mug\":1,\"sticker\":6}"
    -- ANCHOR_END: report

    -- ANCHOR: checkMain
    -- 「main は、3つの形のどれかでなければならない」
    it "says when main has none of the three shapes" $
      map (\src -> fst (runFake emptyWorld
                      (runProgram fakeWorld noFiles preludeSession "m" src)))
        ["let main = 1", "let main = putStrLn", "let main = pure 3"]
        `shouldBe` map
          (\t -> Errored ("in main: type error: main must be IO (), "
                      <> "[String] -> IO (), or [String] -> Map String "
                      <> "-> IO (), but it is " <> t))
          [ "Int", "String -> IO ()", "IO Int" ]
    -- ANCHOR_END: checkMain

  -- 「値を埋めこむ書式」
  describe "f-strings" $ do
    -- ANCHOR: formatSugar
    -- 「f"..." は、++ と関数の呼び出しに開く」
    it "opens f\"...\" into ++ and calls" $
      map (fmap render . parseExpr)
        [ "f\"Hello, {name}!\""
        , "f\"{n:>8,} {n:05}\""
        , "f\"{x=} {x = :^6}\""
        , "f\"{{}} {f\"{x}\"}\""
        , "f\"\"" ]
        `shouldBe` map Right
          [ "\"Hello, \" ++ display name ++ \"!\""
          , "padLeft 8 (groupDigits n) ++ \" \" ++ zeroPad 5 n"
          , "\"x=\" ++ show x ++ \" x = \" ++ center 6 (display x)"
          , "\"{} \" ++ display (display x)"
          , "\"\"" ]
    -- ANCHOR_END: formatSugar

    -- ANCHOR: formatValues
    -- 「値を埋めこみ、桁をそろえる（Python と同じ答え）」
    it "puts values in, and lines them up like Python" $
      runLines
        [ "let name = \"Lam\""
        , "f\"[{name:<6}] [{name:>6}] [{name:^6}] [{name:^5}]\""
        , "f\"{1234567:,} {0 - 1234:,} {42:05} {0 - 42:05} {123456:03}\""
        , "f\"{name=} {[name]} {Just (1, true)} {{\""
        , "f\"{name:>2}|{\"日本\":>4}|\"" ]
        `shouldBe`
          [ "name : String"
          , "\"[Lam   ] [   Lam] [ Lam  ] [ Lam ]\" : String"
          , "\"1,234,567 -1,234 00042 -0042 123456\" : String"
          , "\"name=\\\"Lam\\\" [\\\"Lam\\\"] Just (1, true) {\" : String"
          , "\"Lam|  日本|\" : String" ]
    -- ANCHOR_END: formatValues

    -- ANCHOR: formatErrors
    -- 「書式のまちがいは、動かす前にパーサーが見つける」
    it "finds the mistakes of a format before running" $
      map (fmap (const ()) . parseExpr)
        [ "f\"{x:>}\"", "f\"{x:8}\"", "f\"{x:>05}\"", "f\"{x:05,}\""
        , "f\"{x:>8d}\"", "f\"{x : xs}\"", "f\"{\\y -> y}\"", "f\"a } b\"" ]
        `shouldSatisfy` all isLeft
    -- ANCHOR_END: formatErrors

    -- 「エラーの文が、まちがいの場所を言う」
    it "says what is wrong with a format, and where" $
      runLines ["f\"{x:>}\"", "f\"{x:8}\"", "f\"a } b\""]
        `shouldBe`
          [ "<repl>:1:7:\n  |\n1 | f\"{x:>}\"\n  |       ^\n\
            \a width must follow < > or ^\n"
          , "<repl>:1:6:\n  |\n1 | f\"{x:8}\"\n  |      ^\n\
            \a width needs < > or ^ before it (or 0, for a number)\n"
          , "<repl>:1:5:\n  |\n1 | f\"a } b\"\n  |     ^\n\
            \a single } must be written }}\n" ]

    -- ANCHOR: formatTypes
    -- 「埋めこめない値と、数にしか使えない書式は、型のエラー」
    it "gives a type error for values that cannot be shown" $
      runLines
        [ ":type display"
        , "f\"{putStrLn}\""
        , "f\"{\"Lam\":,}\""
        , "data Handler = Button (Int -> Int)"
        , "f\"{Button (\\n -> n)}\""
        , "data Box a = Box a"
        , "f\"{Box {a: [1]}}\""
        , "f\"{Box (\\n -> n)}\"" ]
        `shouldBe`
          [ "Showable a => a -> String"
          , "type error: String -> IO () is not in the class Showable"
          , "type error: the types do not fit: String where Int is needed"
          , "Button : (Int -> Int) -> Handler"
          , "type error: Handler is not in the class Showable \
            \(Button holds Int -> Int)"
          , "Box : a -> Box a"
          , "\"Box {\\\"a\\\": [1]}\" : String"
          , "type error: a -> a is not in the class Showable" ]
    -- ANCHOR_END: formatTypes

    -- 「f と文字列のあいだに空白があれば、いままでどおり関数の呼び出し」
    it "still calls a function f when a space comes before the string" $
      runLines ["let f s = s ++ \"!\"", "f \"hi\"", "f\"hi\""]
        `shouldBe`
          ["f : String -> String", "\"hi!\" : String", "\"hi\" : String"]

    -- ANCHOR: padProps
    -- 「そろえた文字は、幅より短くならず、もとの文字を右・左・真ん中に持つ」
    it "pads to the width, keeping the text on its side" $
      forAll ((,) <$> choose (0, 12) <*> genText) $ \(w, s) ->
        let n      = max (fromIntegral w) (T.length s)
            blank k  = T.replicate k " "
            d      = n - T.length s
        in  padLeft w s  === blank d <> s
       .&&. padRight w s === s <> blank d
       .&&. center w s   === blank (d `div` 2) <> s <> blank (d - d `div` 2)

    -- 「カンマを外すと、もとの数。0 で埋めた数を読むと、もとの数」
    it "groups digits and pads with zeros without changing the number" $
      forAll ((,) <$> arbitrary <*> choose (0, 12)) $ \(n, w) ->
        let grouped = groupDigits n
            parts   = T.splitOn "," (T.dropWhile (== '-') grouped)
            padded  = zeroPad w n
        in  read (T.unpack (T.filter (/= ',') grouped)) === n
       .&&. all ((== 3) . T.length) (drop 1 parts)
       .&&. all ((`elem` [1, 2, 3]) . T.length) (take 1 parts)
       .&&. read (T.unpack (T.filter (/= '-') padded)) === abs n
       .&&. T.isPrefixOf "-" padded === (n < 0)
       .&&. T.length padded === max (fromIntegral w)
                                    (T.length (T.pack (show n)))
    -- ANCHOR_END: padProps

  -- 「正規表現」
  describe "regular expressions" $ do
    -- ANCHOR: regexSugar
    -- 「re"..." は $regex の呼び出しに開き、中の \ はそのまま」
    it "opens re\"...\" into a call, keeping every \\" $
      map (fmap render . parseExpr)
        [ "re\"\\d{4}-\\d\\d\""
        , "Regex.find re\"a\\\"b\" s" ]
        `shouldBe` map Right
          [ "re\"\\d{4}-\\d\\d\""
          , "Regex.find re\"a\\\"b\" s" ]
    -- ANCHOR_END: regexSugar

    -- 「木の中では、ふつうの文字列を渡す、ふつうの呼び出し」
    it "is an ordinary call in the tree" $
      parseExpr "re\"\\d+\""
        `shouldBe` Right (App (Var "$regex") (Lit (LString "\\d+")))

    -- ANCHOR: regexErrors
    -- 「コンパイルできない正規表現は、動かす前にパーサーが見つける」
    it "finds a regex that does not compile, before running" $ do
      map (fmap (const ()) . parseExpr)
        ["re\"a(b\"", "re\"*\"", "re\"[a-\"", "re\"x{2,1}\""]
        `shouldSatisfy` all isLeft
      runLines ["re\"(\\d{4}\""]
        `shouldBe`
          [ "<repl>:1:4:\n  |\n1 | re\"(\\d{4}\"\n  |    ^\n\
            \this regex does not compile: missing closing parenthesis\n" ]
    -- ANCHOR_END: regexErrors

    -- ANCHOR: regexValues
    -- 「探す・置き換える・切り分ける」
    it "finds, replaces and splits" $
      runLines
        [ "let date = re\"(?<year>\\d{4})-(\\d\\d)-(\\d\\d)\""
        , "Regex.find date \"paid on 2026-11-16\""
        , "Regex.find date \"no date\""
        , "map Match.text (Regex.findAll re\"\\d+\" \"a1b22c\")"
        , "map Match.start (Regex.findAll re\"\\d+\" \"a1b22c\")"
        , "Regex.matches re\"^\\d+$\" \"123\""
        , "Regex.replace re\"\\d+\" \"#\" \"a1b22c\""
        , "Regex.replaceAll re\"\\d+\" \"#\" \"a1b22c\""
        , "Regex.replaceAll re\"\\d+\" \"$1\" \"a1b\""
        , "Regex.replaceWith re\"\\d+\" (\\m -> f\"<{Match.text m}>\") \"a1\""
        , "Regex.split re\",\\s*\" \"a, b,c\""
        , "Regex.split re\"x\" \"\""
        , "map Match.text (Regex.findAll re\"a*\" \"baaac\")"
        , "Regex.find re\"(?<=x)é\" \"éaxé\""
        , "map Match.text (Regex.findAll re\"\\w+\" \"Lam は 小さな\")"
        , "map Match.text (Regex.findAll re\"\\d+\" \"a１２b34\")"
        , "let m = fromMaybe (Match \"\" 0 [] {}) "
            <> "(Regex.find date \"2026-11-16\")"
        , "(Match.group 2 m, Match.group 4 m, Match.named \"year\" m)" ]
        `shouldBe`
          [ "date : Regex"
          , "Just (Match \"2026-11-16\" 8 [\"2026\", \"11\", \"16\"] \
            \{\"year\": \"2026\"}) : Maybe Match"
          , "Nothing : Maybe Match"
          , "[\"1\", \"22\"] : [String]"
          , "[1, 3] : [Int]"
          , "true : Bool"
          , "\"a#b22c\" : String"
          , "\"a#b#c\" : String"
          , "\"a$1b\" : String"
          , "\"a<1>\" : String"
          , "[\"a\", \"b\", \"c\"] : [String]"
          , "[\"\"] : [String]"
          , "[\"\", \"aaa\", \"\", \"\"] : [String]"
          , "Just (Match \"\233\" 3 [] {}) : Maybe Match"
          , "[\"Lam\", \"は\", \"小さな\"] : [String]"
          , "[\"１２\", \"34\"] : [String]"
          , "m : Match"
          , "(Just \"11\", Nothing, Just \"2026\") : \
            \(Maybe String, Maybe String, Maybe String)" ]
    -- ANCHOR_END: regexValues

    -- ANCHOR: perlSugar
    -- 「m/…/ は re"…" に、s/…/…/ は置き換えの関数に、=~ は matches に開く」
    it "opens m/…/, s/…/…/ and =~ like Perl" $
      map (fmap render . parseExpr)
        [ "m/\\d+/i"
        , "s/\\d+/#/g"
        , "s/\\d+/#/"
        , "s/(?<y>\\d+)-(\\d+)/{2}.{y}/g"
        , "line =~ m/ERROR/"
        , "m/2"
        , "s/2/1" ]
        `shouldBe` map Right
          [ "re\"(?i)\\d+\""
          , "Regex.replaceAll re\"\\d+\" \"#\""
          , "Regex.replace re\"\\d+\" \"#\""
          , "Regex.replaceWith re\"(?<y>\\d+)-(\\d+)\" \
            \(\\$m -> $group \"2\" $m ++ \".\" ++ $group \"y\" $m)"
          , "Regex.matches re\"ERROR\" line"
          , "m / 2"
          , "s / 2 / 1" ]
    -- ANCHOR_END: perlSugar

    -- ANCHOR: perlValues
    -- 「Perl 風の書き方で、探して置き換える」
    it "finds and replaces in the style of Perl" $
      runLines
        [ "Regex.find m/lam/i \"I LOVE LAM\""
        , "let hide = s/\\d{2,4}-\\d{2,4}-(?<last>\\d{4})/***-****-{last}/g"
        , "hide \"call 080-9876-5432 or 03-1234-5678\""
        , "s/(\\d+)-(\\d+)/{2}-{1}/ \"11-22 33-44\""
        , "s/(\\d+)/{{{1}}}/g \"a1b2\""
        , "map s/\\s+/ /g [\"a  b\", \"c   d\"]"
        , "(\"ERROR here\" =~ m/^ERROR/, \"ok\" =~ m/^ERROR/)"
        , "Regex.matches m/a\\/b/ \"a/b\""
        , "let half m = m/2"
        , "half 10" ]
        `shouldBe`
          [ "Just (Match \"LAM\" 7 [] {}) : Maybe Match"
          , "hide : String -> String"
          , "\"call ***-****-5432 or ***-****-5678\" : String"
          , "\"22-11 33-44\" : String"
          , "\"a{1}b{2}\" : String"
          , "[\"a b\", \"c d\"] : [String]"
          , "(true, false) : (Bool, Bool)"
          , "true : Bool"
          , "half : Int -> Int"
          , "5 : Int" ]
    -- ANCHOR_END: perlValues

    -- ANCHOR: perlDelimiters
    -- 「/ のかわりに ! # % でもはさめる。( や { は、いままでどおり」
    it "takes ! # and % in place of /" $
      runLines
        [ "m!/usr/bin!"
        , "s#/usr/local#/opt#g \"/usr/local/bin:/usr/local/lib\""
        , "s!a!\\!!g \"banana\""
        , "s%(\\d+)%<{1}>%g \"a1b22\""
        , "let m x = x"
        , "(m(1), m[2], m{a: 3})" ]
        `shouldBe`
          [ "re\"/usr/bin\" : Regex"
          , "\"/opt/bin:/opt/lib\" : String"
          , "\"b!n!n!\" : String"
          , "\"a<1>b<22>\" : String"
          , "m : a -> a"
          , "(1, [2], {\"a\": 3}) : (Int, [Int], Map Int)" ]
    -- ANCHOR_END: perlDelimiters

    -- ANCHOR: perlErrors
    -- 「ないグループ、知らないフラグ、コンパイルできない正規表現は、
    --   動かす前にパーサーが見つける」
    it "finds a missing group, an unknown flag and a bad regex first" $
      runLines ["s/(?<y>\\d+)/{yy}/ \"1\"", "s/x/y/q \"x\"", "m/a(b/"]
        `shouldBe`
          [ "<repl>:1:14:\n  |\n1 | s/(?<y>\\d+)/{yy}/ \"1\"\n\
            \  |              ^\n\
            \this regex has no group yy\n"
          , "<repl>:1:7:\n  |\n1 | s/x/y/q \"x\"\n  |       ^\n\
            \unknown flag 'q' (use gimsx)\n"
          , "<repl>:1:3:\n  |\n1 | m/a(b/\n  |   ^\n\
            \this regex does not compile: missing closing parenthesis\n" ]
    -- ANCHOR_END: perlErrors

    -- ANCHOR: regexCompile
    -- 「動かしてから作る正規表現は、Result で返る」
    it "compiles a regex while running, giving a Result" $
      runLines
        [ ":type Regex.compile"
        , "Regex.compile \"a(b\""
        , "Regex.compile \"a(b)\""
        , "let n = 3"
        , "Regex.compile f\"^\\\\d{{{n}}}$\"" ]
        `shouldBe`
          [ "String -> Result String Regex"
          , "Err \"missing closing parenthesis\" : Result String Regex"
          , "Ok re\"a(b)\" : Result String Regex"
          , "n : Int"
          , "Ok re\"^\\d{3}$\" : Result String Regex" ]
    -- ANCHOR_END: regexCompile

    -- ANCHOR: regexTypes
    -- 「Regex は比べられず、埋めこめない。関数を渡す型も確かめる」
    it "gives types to regexes and their functions" $
      runLines
        [ ":type Regex.replaceWith"
        , "re\"x\" == re\"x\""
        , "f\"{re\"x\"}\""
        , "Regex.replaceWith re\"x\" (\\m -> Match.start m) \"x\"" ]
        `shouldBe`
          [ "Regex -> (Match -> String) -> String -> String"
          , "type error: Regex is not in the class Comparable"
          , "type error: Regex is not in the class Showable"
          , "type error: the types do not fit: Int where String is needed" ]
    -- ANCHOR_END: regexTypes

    -- ANCHOR: regexLimit
    -- 「試す道が多すぎると、PCRE2 はあきらめて、エラーにする」
    it "gives up on a regex that would try too many ways" $
      runLines
        [ "Regex.matches re\"(a+)+$\" \"" <> T.replicate 40 "a" <> "b\"" ]
        `shouldBe` ["error: Regex.matches: match limit exceeded"]
    -- ANCHOR_END: regexLimit

    -- ANCHOR: cutProps
    -- 「切れ端と見つかったものを順に並べると、もとの文字列に戻る」
    it "cuts a text into pieces that sew back into it" $
      forAll ((,) <$> elements regexes <*> genRegexText) $ \(source, t) ->
        case compileRegex source of
          Left err -> counterexample (T.unpack err) False
          Right r  -> case regexSearch r t of
            Left err -> counterexample (T.unpack err) False
            Right (Cut pieces found) ->
              let sewn = T.concat (zipLonger pieces
                           [ w | w :| _ <- found ])
              in  length pieces === length found + 1
             .&&. sewn === t
    -- ANCHOR_END: cutProps

    -- 「名前付きのグループの番号を、PCRE2 から読む」
    it "knows the numbers of the named groups" $
      fmap regexNames (compileRegex "(?<y>\\d+)-(x)-(?<z>.)")
        `shouldBe` Right [("y", 1), ("z", 3)]

  -- 「パーサー」
  describe "parsers" $ do
    -- ANCHOR: parserSugar
    -- 「Parser.do は Parser.bind に開く。値が先」
    it "opens Parser.do into Parser.bind, with the value first" $
      fmap render (parseExpr "Parser.do\n  x <- p\n  _ <- q\n  Parser.pure x")
        `shouldBe` Right
          "Parser.bind p (\\x -> Parser.bind q (\\_ -> Parser.pure x))"
    -- 「Maybe.do も同じ形。ただの do は、名前のない bind に開く」
    it "opens any Name.do the same way, and do into plain bind" $
      map (fmap render . parseExpr)
        [ "Maybe.do\n  x <- m\n  Just x", "do\n  x <- m\n  pure x" ]
        `shouldBe` map Right
          [ "Maybe.bind m (\\x -> Just x)", "bind m (\\x -> pure x)" ]
    -- 「Parser.do は名前ではない」
    it "does not read Parser.do as a name" $
      runLines ["let Parser.do = 1"]
        `shouldSatisfy` all ("cannot end a name" `T.isInfixOf`)
    -- ANCHOR_END: parserSugar
    -- 「Parser.bind はパーサーが先、Parser.andThen は関数が先」
    it "has Parser.bind with the parser first, and Parser.andThen" $
      runLines
        [ ":type Parser.bind", ":type Parser.andThen"
        , "Parser.run (Parser.bind Parser.int (\\n -> Parser.pure (n + 1))) \"41\""
        , "Parser.run (Parser.andThen (\\n -> Parser.pure (n + 1)) Parser.int) \"41\"" ]
        `shouldBe`
          [ "Parser a -> (a -> Parser b) -> Parser b"
          , "(a -> Parser b) -> Parser a -> Parser b"
          , "Ok 42 : Result String Int", "Ok 42 : Result String Int" ]

    -- ANCHOR: parserValues
    -- 「小さなパーサーと、その組み合わせ」
    it "reads with small parsers, put together" $
      runLines
        [ "Parser.run Parser.int \"42\""
        , "Parser.run (Parser.map (\\n -> n * 2) Parser.int) \"21\""
        , "Parser.run (Parser.many (Parser.string \"ab\")) \"ababab\""
        , "Parser.run (Parser.some Parser.int) \"\" == Ok []"
        , "Parser.run (Parser.sepBy Parser.int (Parser.symbol \",\"))\
          \ \"1, 2,3\""
        , "Parser.run (Parser.optional (Parser.string \"-\")) \"\""
        , "Parser.run (Parser.oneOf [Parser.string \"ab\",\
          \ Parser.string \"ac\"]) \"ac\""
        , "Parser.run (Parser.between \"(\" \")\" (Parser.token Parser.int))\
          \ \"( 7 )\""
        , "Parser.run (Parser.regex re\"\\d+\") \"123\""
        ]
        `shouldBe`
          [ "Ok 42 : Result String Int"
          , "Ok 42 : Result String Int"
          , "Ok [\"ab\", \"ab\", \"ab\"] : Result String [String]"
          , "false : Bool"
          , "Ok [1, 2, 3] : Result String [Int]"
          , "Ok Nothing : Result String (Maybe String)"
          , "Ok \"ac\" : Result String String"
          , "Ok 7 : Result String Int"
          , "Ok (Match \"123\" 0 [] {}) : Result String Match" ]
    -- 「Parser.regex は、その場所でしか合わない。start は全体から数える」
    it "matches a regex only right here, and counts from the whole text" $
      runLines
        [ "let p = Parser.andThen (\\_ -> Parser.regex re\"\\d+\")\
          \ (Parser.string \"ab\")"
        , "Result.map Match.start (Parser.run p \"ab12\")"
        , "Result.map Match.text\
          \ (Parser.run (Parser.regex re\"\\d+\") \"x1\") == Ok \"1\"" ]
        `shouldBe`
          [ "p : Parser Match"
          , "Ok 2 : Result String Int"
          , "false : Bool" ]
    -- ANCHOR_END: parserValues

    -- ANCHOR: parserErrors
    -- 「エラーは、いちばん遠くまで読めたところを指す」
    it "points at the place that was read the farthest" $
      runLines
        [ "Parser.run (Parser.oneOf [Parser.int,\
          \ Parser.between \"(\" \")\" Parser.int]) \"(1\"" ]
        `shouldBe`
          [ "Err \"1:3:\\n  |\\n1 | (1\\n  |   ^\\nunexpected end of input\\n\
            \expecting ')' or digit\" : Result String Int" ]
    -- 「Parser.label で、欲しかったものに名前を付ける」
    it "names what was wanted, with Parser.label" $
      runLines
        [ "Parser.run (Parser.label \"a price\" Parser.int) \"x\"" ]
        `shouldBe`
          [ "Err \"1:1:\\n  |\\n1 | x\\n  | ^\\nunexpected 'x'\\n\
            \expecting a price\" : Result String Int" ]
    -- 「何も読まずに合うものをくり返すと、止まらずにエラーになる」
    it "stops many with an error, if the parser reads nothing" $
      runLines [ "Parser.run (Parser.many (Parser.pure 1)) \"\"" ]
        `shouldBe`
          [ "error: Parser.many: the parser read nothing, \
            \so it would go on forever" ]
    -- 「Lam の関数のまちがいは、プログラムを止める」
    it "stops the program when a Lam function fails" $
      runLines [ "Parser.run (Parser.map (\\n -> n / 0) Parser.int) \"1\"" ]
        `shouldBe` [ "error: divided by zero" ]
    -- 「型のまちがいは、動かす前に」
    it "finds a parser of the wrong type before running" $
      runLines
        [ "Parser.run Parser.int \"1\" == Ok \"1\""
        , "Parser.many 3" ]
        `shouldSatisfy` all ("type error" `T.isInfixOf`)
    -- ANCHOR_END: parserErrors

    -- ANCHOR: parserLazy
    -- 「自分を指すパーサーは、Parser.lazy で」
    it "reads nested parentheses, with Parser.lazy" $
      runLines
        [ "let depth = Parser.oneOf\
          \ [ Parser.map (\\d -> d + 1)\
          \     (Parser.between \"(\" \")\" (Parser.lazy (\\_ -> depth)))\
          \ , Parser.pure 0 ]"
        , "Parser.run depth \"((()))\"" ]
        `shouldBe`
          [ "depth : Parser Int", "Ok 3 : Result String Int" ]
    -- ANCHOR_END: parserLazy

    -- ANCHOR: parserProps
    -- 「どんな文字のフィールドでも、引用符で囲んだ CSV から読み戻せる」
    it "reads back any fields, written as CSV in quotes" $
      withNumTests 200 $ forAll (listOf1 genText) $ \fields ->
        ioProperty $ do
          source <- Utf8.readFile "examples/csv.lam"
          let quote f = "\"" <> T.replace "\"" "\"\"" f <> "\""
              line = T.intercalate "," (map quote fields)
          pure $ inProgram source ["Parser.run row " <> renderString line]
            === [ "Ok [" <> T.intercalate ", " (map renderString fields)
                  <> "] : Result String [String]" ]
    -- 「電卓の答えは、Haskell で計算した答えと同じ」
    it "computes what Haskell computes" $
      withNumTests 200 $ forAll genCalc $ \(text, value) -> ioProperty $ do
        source <- Utf8.readFile "examples/calc.lam"
        pure $ inProgram source ["calc " <> renderString text]
          === ["Ok " <> okArgument value <> " : Result String Int"]
    -- ANCHOR_END: parserProps

  -- 「スクリプトとして動かす」
  describe "scripts" $ do
    -- ANCHOR: pipeTests
    -- 「x |> f は f x。いちばん弱く、左に寄る」
    it "reads x |> f as f x, the loosest, leaning left" $
      map (fmap render . parseExpr)
        [ "x |> f |> g"
        , "1 + 2 |> showInt"
        , "a || b |> not"
        , "xs |> map f |> take 3"
        , "f\"{xs |> length}\"" ]
        `shouldBe` map Right
          [ "g (f x)"
          , "showInt (1 + 2)"
          , "not (a || b)"
          , "take 3 (map f xs)"
          , "display (length xs)" ]

    -- 「|> は || とも、case の | とも、ぶつからない」
    it "does not mix |> up with || or the | of case" $
      runLines
        [ "true || false |> not"
        , "case 3 of | 3 -> \"three\" |> toUpper | _ -> \"other\""
        , "[3, 1, 2] |> sort |> map (\\n -> n * 10)" ]
        `shouldBe`
          [ "false : Bool"
          , "\"THREE\" : String"
          , "[10, 20, 30] : [Int]" ]

    -- 「x |> f と f x は、いつも同じ値」
    it "gives the same value as calling the function" $
      property $ \(ns :: [NonNegative Integer]) ->
        -- Lam has no literal for a negative number (it is 0 - 5).
        -- Lam には負の数のリテラルがない（0 - 5 と書く）
        let list = renderList (map getNonNegative ns)
        in runLines [list <> " |> map (\\n -> n * 2) |> sum"]
             === runLines ["sum (map (\\n -> n * 2) " <> list <> ")"]
    -- ANCHOR_END: pipeTests

    -- ANCHOR: wordTests
    -- 「行と単語に切り、並べ替える。sortOn は同じキーの順を変えない」
    it "cuts lines and words, and sorts (keeping ties in order)" $
      runLines
        [ "lines \"a\\nb c\\n\\nd\""
        , "words \"  a  b\\tc\\n\""
        , "sort [3, 1, 2]"
        , "sortOn (\\p -> 0 - snd p) [(\"a\", 1), (\"b\", 2), (\"c\", 1)]"
        , "take 2 [1, 2, 3]"
        , "sortOn (\\f -> 1) [\\x -> x]"
        , "sort [\\x -> x]" ]
        `shouldBe`
          [ "[\"a\", \"b c\", \"\", \"d\"] : [String]"
          , "[\"a\", \"b\", \"c\"] : [String]"
          , "[1, 2, 3] : [Int]"
          , "[(\"b\", 2), (\"a\", 1), (\"c\", 1)] : [(String, Int)]"
          , "[1, 2] : [Int]"
          , "[<function of x>] : [a -> a]"
          , "type error: a -> a is not in the class Comparable" ]
    -- ANCHOR_END: wordTests

    -- ANCHOR: argsTests
    -- 「引数と環境変数：getArgs と lookupEnv、main の3つの形」
    it "gives the arguments and the environment, in three ways" $ do
      let world = emptyWorld
            { worldArgs = ["a", "b"]
            , worldEnv  = Map.fromList [("HOME", "/home/lam")] }
          outOf src = outputOf (snd (script world src))
      outOf "let main = getArgs |> IO.andThen (\\xs -> putStrLn (join \",\" xs))"
        `shouldBe` "a,b\n"
      outOf "let main args = putStrLn (showInt (length args))"
        `shouldBe` "2\n"
      outOf (T.unlines
        [ "let main args env = do"
        , "  home <- lookupEnv \"HOME\""
        , "  putStrLn (fromMaybe \"?\" home)"
        , "  putStrLn (fromMaybe \"?\" (Map.lookup \"HOME\" env))"
        , "  user <- lookupEnv \"USER\""
        , "  putStrLn (fromMaybe \"nobody\" user)" ])
        `shouldBe` "/home/lam\n/home/lam\nnobody\n"
    -- ANCHOR_END: argsTests

    -- ANCHOR: exitTests
    -- 「exit は、そこで止まる。続きの頼みごとはやらない」
    it "stops at exit, and does not do the rest" $ do
      let (ending, world) = script emptyWorld (T.unlines
            [ "let main = do"
            , "  putStrLn \"before\""
            , "  ePutStrLn \"something went wrong\""
            , "  exit 3"
            , "  putStrLn \"after\"" ])
      ending `shouldBe` ExitedWith 3
      outputOf world `shouldBe` "before\n"
      errorsOf world `shouldBe` "something went wrong\n"

    -- 「exit は IO a：if のどちらの側にも置ける。REPL は終わらない」
    it "has the type IO a, and does not end the REPL" $
      runLines
        [ ":type exit"
        , "if true then exit 0 else pure 3"
        , "exit 2" ]
        `shouldBe`
          [ "Int -> IO a"
          , "(exit 0; the REPL goes on)"
          , "(exit 2; the REPL goes on)" ]

    -- 「getContents は、残りの入力を全部読む」
    it "reads the rest of the standard input with getContents" $ do
      let world = emptyWorld { worldInput = ["one", "two", "three"] }
          (_, after) = script world (T.unlines
            [ "let main = do"
            , "  first <- getLine"
            , "  rest <- getContents"
            , "  putStrLn (showInt (length (lines rest)))" ])
      outputOf after `shouldBe` "2\n"
    -- ANCHOR_END: exitTests

    -- ANCHOR: shebangTests
    -- 「#! の行は読み飛ばす。行の番号はずれない」
    it "skips the #! line, and keeps the line numbers" $ do
      let source = "#!/usr/bin/env lam\n\nlet main = putStrLn (showInt (1 / 0))\n"
      fst (script emptyWorld source)
        `shouldBe` Errored
          "script.lam:3:33: in the definition of main: error: divided by zero"
    -- ANCHOR_END: shebangTests

    -- ANCHOR: placeTests
    -- 「エラーは、いちばん内側の場所を指す。定義の中なら、その定義の場所」
    it "points at the innermost place of an error" $ do
      let source = T.unlines
            [ "let average xs ="
            , "  sum xs / length xs"
            , "let main = do"
            , "  putStrLn \"start\""
            , "  putStrLn (showInt (average []))" ]
      fst (script emptyWorld source)
        `shouldBe` Errored "script.lam:2:10: error: divided by zero"

    -- 「型のエラーにも、場所と定義の名前が付く」
    it "puts the place and the definition on a type error" $ do
      let source = "let a = 1\nlet b =\n  a ++ \"!\"\n"
      fst (script emptyWorld source)
        `shouldBe` Errored ("script.lam:3:5: in the definition of b: "
                            <> "type error: Int is not in the class Semigroup")

    -- 「head [] の場所は、呼び出した所」
    it "points at the call of a builtin that fails" $
      fst (script emptyWorld "let main = putStrLn (head (tail [\"x\"]))\n")
        `shouldBe` Errored
          ("script.lam:1:22: in the definition of main: "
             <> "error: head: the list is empty")
    -- ANCHOR_END: placeTests

    -- ANCHOR: wordfreqProp
    -- 「wordfreq.lam は、Haskell で数えたトップ10と同じものを見せる」
    it "shows the same top 10 as Haskell does" $
      forAll genWords $ \ls -> ioProperty $ do
        source <- Utf8.readFile "examples/wordfreq.lam"
        let world = emptyWorld { worldInput = ls }
        pure (outputOf (snd (script world source)) === haskellTop10 ls)
    -- ANCHOR_END: wordfreqProp

  -- 「トレーニングの解答」
  describe "answers" $ do
    -- ANCHOR: answerShort
    -- 「ステップ5：{ name, days } は { name = name, days = days } の略」
    it "reads { name, days } as the fields of the same names" $ do
      let short = runLamParser (spaces *> shortRecord <* eof) "<test>"
      short "{ name, days }" `shouldBe`
        Right (Record [("name", Var "name"), ("days", Var "days")])
      isLeft (short "{ days, days }") `shouldBe` True
      isLeft (short "{ name = 1 }") `shouldBe` True
    -- ANCHOR_END: answerShort

    -- 「getContents と appendFile を Lam で」
    golden "contents"
    golden "append"
    -- 「cat と、オプションのある head」
    golden "cat"
    golden "head"
    -- ANCHOR: answerDialog
    -- 「名前を打たずに終わる。打った行より多くは読まない」
    it "says goodbye at the end of input, and reads only one line" $ do
      source <- Utf8.readFile "examples/hello.lam"
      let run ls = runFake emptyWorld { worldInput = ls }
                     (runProgram fakeWorld noFiles preludeSession
                        "hello" source)
          (_, quiet) = run []
          (_, twice) = run ["Lam", "Haskell"]
      outputOf quiet `shouldBe` "What is your name? Goodbye!\n"
      outputOf twice `shouldBe`
        "What is your name? Hello, Lam!\nYour name has 3 letters.\n"
      worldInput twice `shouldBe` ["Haskell"]
    -- ANCHOR_END: answerDialog

    -- ANCHOR: answerExit
    -- 「ステップ3：終了コードを確かめる」
    it "gives the exit codes of cat and head" $ do
      cat  <- Utf8.readFile "examples/cat.lam"
      hd   <- Utf8.readFile "examples/head.lam"
      let files = Map.fromList [("a.txt", "1\n2\n3\n")]
          run src args = fst (script emptyWorld
            { worldArgs = args, worldFiles = files } src)
      run cat ["a.txt"]                 `shouldBe` Finished ""
      run cat ["a.txt", "nope", "a.txt"] `shouldBe` ExitedWith 1
      run hd  ["-n", "2", "a.txt"]       `shouldBe` Finished ""
      run hd  ["--help"]                 `shouldBe` Finished ""
      run hd  ["-n", "x", "a.txt"]       `shouldBe` ExitedWith 2
      run hd  []                         `shouldBe` ExitedWith 2
      run hd  ["nope"]                   `shouldBe` ExitedWith 1
    -- ANCHOR_END: answerExit

  -- 「外の世界」
  describe "outside" $ do
    -- ANCHOR: weatherTwice
    -- 「次の日に動かすと、表が育ち、変わったところを知らせる」
    it "keeps the days, and tells what changed on the next run" $ do
      dbs  <- memoryDatabases
      next <- Utf8.readFile "examples/outside/web/tokyo-next.json"
      _ <- runExampleWith dbs id "examples/outside/weather"
      (out, _) <- runExampleWith dbs
        (\w -> w { worldWeb = Map.map (const (200, next)) (worldWeb w) })
        "examples/outside/weather"
      T.lines out `shouldContain`
        [ "2026-11-20  rain  30%  max  15.3  min   7.1  rain 4% -> 30%"
        , "2026-11-21  rain   2%  max  14.6  min   4.5  max 13.1 -> 14.6" ]
      T.lines out `shouldContain`
        [ "2026-11-24  rain   1%  max  12.4  min   3.0  new" ]
      -- 11-18 has no max (null) in the next forecast: the old 20.4 is
      -- kept, and it is not a change.
      -- 次の予報には 11-18 の最高がない（null）：前の 20.4 を残し、
      -- 変化とは言わない
      T.lines out `shouldContain`
        [ "2026-11-18  rain 100%  max  20.4  min   9.5" ]
      last (T.lines out) `shouldBe` "kept 8 days, the hottest 20.4"
    -- ANCHOR_END: weatherTwice

    -- ANCHOR: dexTwice
    -- 「まだ持っていないポケモンだけを、Web に聞く」
    it "asks the web only for a Pokemon it does not have yet" $ do
      dbs <- memoryDatabases
      (_, first)  <- runExampleWith dbs id "examples/outside/dex"
      (_, second) <- runExampleWith dbs id "examples/outside/dex"
      reverse (worldSent first) `shouldBe`
        [ "GET https://pokeapi.co/api/v2/pokemon/pikachu"
        , "GET https://pokeapi.co/api/v2/pokemon-species/pikachu"
        , "GET https://pokeapi.co/api/v2/pokemon/nosuchmon"
        , "GET https://pokeapi.co/api/v2/pokemon-species/nosuchmon" ]
      -- The second run: Pikachu is in dex.db, so only the missing one.
      -- 2回目：ピカチュウは dex.db にあるので、ないものだけ
      reverse (worldSent second) `shouldBe`
        [ "GET https://pokeapi.co/api/v2/pokemon/nosuchmon"
        , "GET https://pokeapi.co/api/v2/pokemon-species/nosuchmon" ]
    -- ANCHOR_END: dexTwice

    -- ANCHOR: readers
    -- 「読み方を選べる。Err は、答えがない・状態が悪い・本文が読めない」
    it "reads an answer the way it is told, and says why it failed" $ do
      dbs <- memoryDatabases
      let world = emptyWorld { worldWeb = Map.fromList
            [ ("https://x.test/ok",  (200, "{\"n\": 1}"))
            , ("https://x.test/bad", (200, "{\"n\": "))
            , ("https://x.test/404", (404, "{\"error\": \"no\"}")) ] }
      (ending, after) <- scriptIO dbs defaultOptions world $ T.unlines
        [ "let main = do"
        , "  a <- Http.get \"https://x.test/ok\""
        , "  b <- Http.get \"https://x.test/bad\""
        , "  c <- Http.get \"https://x.test/404\""
        , "  d <- Http.get \"https://x.test/none\""
        , "  e <- Http.request Http.string Http.json \"GET\""
        , "         \"https://x.test/404\" {} \"\""
        , "  f <- Http.request Http.response Http.string \"GET\""
        , "         \"https://x.test/ok\" {} \"\""
        , "  putStrLn (show (a, c, d))"
        , "  putStrLn (show (e, Result.map Http.status f))"
        , "  putStrLn (show b)" ]
      ending `shouldBe` Finished ""
      outputOf after `shouldBe` T.unlines
        [ "(Ok (JObj {\"n\": JInt 1}), "
            <> "Err \"status 404: {\\\"error\\\": \\\"no\\\"}\", "
            <> "Err \"https://x.test/none: cannot connect\")"
        , "(Err (BadStatus 404 (JObj {\"error\": JStr \"no\"})), Ok 200)"
        , "Err \"object value: not enough input\"" ]
    -- ANCHOR_END: readers

    -- ANCHOR: guardProp
    -- 「権限がなければ、ハンドラーに頼まずに答える」
    it "asks the handler only when every permission is given" $
      property $ forAll (sublistOf [Net, Read, Write, Env]) $ \given ->
        forAll (elements sampleRequests) $ \request ->
          let perms = Only (Map.fromList [ (p, Everything) | p <- given ])
              asked = snd (runState (allowOnly perms counting request) 0)
              counting _ = modify (+ (1 :: Int)) >> pure (VTuple [])
              needed = map fst (needs request)
          in asked == (if all (`elem` given) needed then 1 else 0)
    -- ANCHOR_END: guardProp
    -- ANCHOR: scopes
    -- 「ホスト・フォルダー・名前を指して許す」
    it "allows only the hosts, folders and names it is given" $ do
      covers Net (Within ["pokeapi.co"]) (hostOf "https://pokeapi.co/api/v2/")
        `shouldBe` True
      -- The host is pokeapi.co.evil.test, not pokeapi.co.
      -- ホストは pokeapi.co.evil.test で、pokeapi.co ではない
      covers Net (Within ["pokeapi.co"])
        (hostOf "https://pokeapi.co.evil.test/x") `shouldBe` False
      let reads = covers Read (Within ["data"])
      map reads ["data/a.txt", "./data/b/c.txt", "data2/a.txt"
                , "data/../secret.txt", "secret.txt"]
        `shouldBe` [True, True, False, False, False]
      map (covers Write (Within ["."])) ["dex.db", "out/a.txt", "../x.db"]
        `shouldBe` [True, True, False]
      covers Env (Within ["HOME"]) "HOME" `shouldBe` True
      covers Env (Within ["HOME"]) "AWS_SECRET" `shouldBe` False
    it "hides the environment variables that are not allowed" $ do
      let world = emptyWorld { worldEnv = Map.fromList
                    [("HOME", "/home/lam"), ("TOKEN", "s3cret")] }
          source = T.unlines
            [ "let main = do"
            , "  h <- lookupEnv \"HOME\""
            , "  t <- lookupEnv \"TOKEN\""
            , "  e <- getEnvironment"
            , "  putStrLn (show (h, t, Map.keys e))" ]
          run flags = outputOf (snd (runFake world (runProgram
            (guarded (opts flags) fakeWorld) noFiles preludeSession
            "env.lam" source)))
          opts flags = either error fst (readOptions flags)
      run [] `shouldBe`
        "(Just \"/home/lam\", Just \"s3cret\", [\"HOME\", \"TOKEN\"])\n"
      run ["--allow-env=HOME"] `shouldBe`
        "(Just \"/home/lam\", Nothing, [\"HOME\"])\n"
      run ["--deny-all"] `shouldBe` "(Nothing, Nothing, [])\n"
    -- ANCHOR_END: scopes
    -- ANCHOR: flags
    -- 「ファイルより前のフラグを読み、残りはスクリプトのもの」
    it "reads the flags before the file, and leaves the rest" $ do
      readOptions [] `shouldBe` Right (defaultOptions, [])
      readOptions ["--allow-net", "--trace", "a.lam", "--allow-read"]
        `shouldBe` Right
          ( defaultOptions
              { permissions = Only (Map.fromList [(Net, Everything)])
              , tracing = True }
          , ["a.lam", "--allow-read"] )
      let perms flags = fmap (permissions . fst) (readOptions flags)
      perms ["--deny-all"] `shouldBe` Right (Only Map.empty)
      perms ["--allow-read", "--deny-all"]
        `shouldBe` Right (Only (Map.fromList [(Read, Everything)]))
      perms ["--allow-read=data,logs", "--allow-net=pokeapi.co"]
        `shouldBe` Right (Only (Map.fromList
          [(Read, Within ["data", "logs"]), (Net, Within ["pokeapi.co"])]))
      perms ["--allow-read=data", "--allow-read"]
        `shouldBe` Right (Only (Map.fromList [(Read, Everything)]))
      readOptions ["--alow-net", "a.lam"] `shouldSatisfy` isLeft
      readOptions ["--allow-time", "a.lam"] `shouldSatisfy` isLeft
    -- ANCHOR_END: flags
    -- ANCHOR: traceTest
    -- 「許されない頼みごとも、trace には出る」
    it "traces every request, even one that is not allowed" $ do
      dbs <- memoryDatabases
      let opts = defaultOptions { permissions = Only Map.empty
                                , tracing = True }
      (_, after) <- scriptIO dbs opts emptyWorld $ T.unlines
        [ "let main = do"
        , "  a <- readFile \"メモ.txt\""
        , "  b <- Http.get \"https://x.test/\""
        , "  putStrLn (show (a, b))" ]
      errorsOf after `shouldBe` T.unlines
        [ "trace: readFile \"メモ.txt\""
        , "trace: Http.send \"GET\" \"https://x.test/\"" ]
      worldSent after `shouldBe` []
    -- ANCHOR_END: traceTest

    -- ANCHOR: dryRunTest
    -- 「--dry-run は、書かずに、何を書くはずだったかを知らせる」
    it "does not write with --dry-run, but tells what it would do" $ do
      dbs <- memoryDatabases
      let world = emptyWorld
            { worldWeb = Map.fromList [("https://x.test/n", (200, "{}"))] }
          opts = defaultOptions { dryRunning = True }
      _ <- scriptIO dbs defaultOptions world $ T.unlines
        [ "let main = do"
        , "  opened <- Sql.open \"d.db\""
        , "  case opened of"
        , "    | Ok db -> do"
        , "        _ <- Sql.exec db \"CREATE TABLE t (x)\" []"
        , "        pure ()"
        , "    | Err e -> putStrLn e" ]
      (_, after) <- scriptIO dbs opts world $ T.unlines
        [ "let main = do"
        , "  w <- writeFile \"out.txt\" \"hi\""
        , "  p <- Http.post \"https://x.test/n\" JNull"
        , "  opened <- Sql.open \"d.db\""
        , "  case opened of"
        , "    | Ok db -> do"
        , "        n <- Sql.exec db \"INSERT INTO t VALUES (1)\" []"
        , "        rows <- Sql.query db \"SELECT * FROM t\" []"
        , "        putStrLn (show (w, p, n, rows))"
        , "    | Err e -> putStrLn e" ]
      worldFiles after `shouldBe` Map.empty
      worldSent after `shouldBe` []
      outputOf after `shouldBe`
        ("(Ok (), Err \"dry run: POST https://x.test/n was not sent\", "
           <> "Ok 0, Ok [])\n")
      errorsOf after `shouldBe` T.unlines
        [ "dry-run: writeFile \"out.txt\" (2 letters)"
        , "dry-run: POST https://x.test/n"
        , "dry-run: Sql.exec \"INSERT INTO t VALUES (1)\"" ]
    -- ANCHOR_END: dryRunTest

    -- ANCHOR: fixedTest
    -- 「Json.fixed：小数点以下を n けたに。四捨五入は 0 から遠いほうへ」
    it "writes a number with n digits after the point" $ do
      runLines
        [ "Json.fixed 1 (JInt 3)", "Json.fixed 1 (Json.mkDec 1275 2)"
        , "Json.fixed 1 (Json.mkDec (0 - 1275) 2)"
        , "Json.fixed 2 (Json.mkDec 5 1)", "Json.fixed 1 JNull" ]
        `shouldBe`
          [ "\"3.0\" : String", "\"12.8\" : String", "\"-12.8\" : String"
          , "\"0.50\" : String", "\"null\" : String" ]
    it "rounds as Haskell does with Rational, half away from zero" $
      property $ forAll (chooseInteger (-10000000, 10000000)) $ \c ->
        forAll (chooseInteger (-3, 6)) $ \d ->
          forAll (chooseInteger (0, 4)) $ \n ->
            -- Lam has no negative literals: -5 is written (0 - 5).
            -- Lam に負の数のリテラルはないので、-5 は (0 - 5) と書く
            let lit k
                  | k < 0     = "(0 - " <> T.pack (show (negate k)) <> ")"
                  | otherwise = T.pack (show k)
            in runLines [ "Json.fixed " <> lit n <> " (Json.mkDec " <> lit c
                            <> " " <> lit d <> ")" ]
                 === [ "\"" <> fixedOf n c d <> "\" : String" ]
    -- ANCHOR_END: fixedTest

    -- ANCHOR: cellsTest
    -- 「整数・短い小数・文字列・null は、貯めて読むと元のまま」
    it "gives back an integer, a short decimal, a text and null as it was" $
      property $ forAll genCell $ \(cell, shown) -> ioProperty $ do
        dbs <- memoryDatabases
        let ask r = fromMaybe (error "not SQL") (sqlRequest dbs r)
        _ <- ask (SqlOpen "t.db" ReadWrite)
        _ <- ask (SqlExec 1 "CREATE TABLE t (x)" [])
        _ <- ask (SqlExec 1 "INSERT INTO t VALUES (?)" [cell])
        rows <- ask (SqlQuery 1 "SELECT x FROM t" [])
        pure (renderValue rows === "Ok [{\"x\": " <> shown <> "}]")
    it "says no to an integer SQLite cannot keep, and to an array" $ do
      dbs <- memoryDatabases
      (ending, after) <- scriptIO dbs defaultOptions emptyWorld $ T.unlines
        [ "let big = 9223372036854775807"
        , "let main = do"
        , "  opened <- Sql.open \":memory:\""
        , "  case opened of"
        , "    | Ok db -> do"
        , "        a <- Sql.query db \"SELECT ? AS n\" [JInt big]"
        , "        b <- Sql.query db \"SELECT ? AS n\" [JInt (big + 1)]"
        , "        c <- Sql.query db \"SELECT ? AS n\" [JArr []]"
        , "        d <- Sql.exec db \"CREATE TABLE t (x)\" []"
        , "        putStrLn (show (a, b, c, d))"
        , "    | Err e -> putStrLn e" ]
      ending `shouldBe` Finished ""
      outputOf after `shouldBe` T.unlines
        [ "(Ok [{\"n\": JInt 9223372036854775807}], "
            <> "Err \"Sql: the integer is too large\", "
            <> "Err \"Sql: an array or an object cannot be a cell; "
            <> "use Json.show\", "
            <> "Ok 0)" ]
    -- ANCHOR_END: cellsTest

    -- ANCHOR: realFile
    -- 「本物のファイル：読むだけに開くと書けず、ないファイルは作らない」
    it "opens a real file to read only, and makes no missing file" $ do
      tmp <- getTemporaryDirectory
      (path, h) <- openTempFile tmp "lam-outside.db"
      hClose h
      dbs <- realDatabases
      let ask r = fromMaybe (error "not SQL") (sqlRequest dbs r)
          file = T.pack path
      _ <- ask (SqlOpen file ReadWrite)
      _ <- ask (SqlExec 1 "CREATE TABLE t (x)" [])
      _ <- ask (SqlExec 1 "INSERT INTO t VALUES (?)" [CInt 7])
      _ <- ask (SqlClose 1)
      _ <- ask (SqlOpen file ReadOnly)
      written <- ask (SqlExec 2 "INSERT INTO t VALUES (8)" [])
      rows    <- ask (SqlQuery 2 "SELECT x FROM t" [])
      missing <- ask (SqlOpen (file <> "-missing") ReadOnly)
      closeDatabases dbs
      made <- doesFileExist (path <> "-missing")
      removeFile path
      renderValue written `shouldSatisfy` ("Err " `T.isPrefixOf`)
      renderValue rows `shouldBe` "Ok [{\"x\": JInt 7}]"
      renderValue missing `shouldSatisfy` ("Err " `T.isPrefixOf`)
      made `shouldBe` False
    -- ANCHOR_END: realFile

  -- 「ワンライナー」
  describe "one-liners" $ do
    -- ANCHOR: oneLinerGoldenTest
    -- 「例のワンライナーは、本物の lam と同じ答えを出す」
    it "gives the same answers as the real lam" $ do
      names <- listDirectory "examples/oneliner"
      let cmds = [ n | n <- names, takeExtension n == ".cmd" ]
      length cmds `shouldSatisfy` (>= 20)
      mapM_ (\n -> do
               let base = "examples/oneliner" </> dropExtension n
               want <- Utf8.readFile (base <> ".out")
               got  <- runOneLinerExample base
               (n, got) `shouldBe` (n, want))
            cmds
    -- ANCHOR_END: oneLinerGoldenTest

    -- ANCHOR: readCommandTest
    -- 「短いフラグは、Perl と同じくまとめて書ける」
    it "reads short flags together, as Perl does" $ do
      let one args = fmap snd (readCommand args)
      one ["-ane", "x", "a.txt"] `shouldBe` Right (RunOneLiner
        noOneLiner { oneProgram = ["x"], oneMode = EachLine
                   , oneSplit = ByWords } ["a.txt"])
      one ["-F,", "-pe", "x"] `shouldBe` Right (RunOneLiner
        noOneLiner { oneProgram = ["x"], oneMode = Printing
                   , oneSplit = ByRegex "," } [])
      one ["-i.bak", "-pex", "f"] `shouldBe` Right (RunOneLiner
        noOneLiner { oneProgram = ["x"], oneMode = Printing
                   , oneInPlace = Just ".bak" } ["f"])
      one ["-n", "--begin", "0", "-e", "acc"] `shouldBe` Right (RunOneLiner
        noOneLiner { oneProgram = ["acc"], oneMode = EachLine
                   , oneBegin = Just "0" } [])
      one ["--trace", "prog.lam", "-n"]
        `shouldBe` Right (RunFile "prog.lam" ["-n"])
    -- 「一緒に使えないフラグや、知らないフラグは、動かす前に止める」
    it "stops flags that do not go together, before running" $
      map (either Left (const (Right ())) . readCommand)
        [ ["-n", "a.txt"], ["-i", "-ne", "x", "f"], ["-i", "-pe", "x"]
        , ["--begin", "0", "-pe", "x"], ["--end", "x", "-ne", "x"]
        , ["-F(", "-e", "x"], ["-x"], ["-e"] ]
        `shouldBe`
          map Left
            [ "-n, -p, -a, -F, -i, --begin and --end need -e 'program'"
            , "-i needs -p (each line becomes what the program gives)"
            , "-i needs the files to change"
            , "--begin and --end go with -n, not with -p"
            , "--end needs --begin (the first acc)"
            , "-F: missing closing parenthesis"
            , "unknown flag: -x"
            , "-e and -F need something after them" ]
    -- ANCHOR_END: readCommandTest

    -- ANCHOR: byTypeTest
    -- 「s/a/b/g と s/a/b/g line は、どの行にも同じ答えを出す」
    it "gives the same lines for s/a/b/g and s/a/b/g line" $
      property $ forAll (listOf genLine) $ \ls ->
        printed "s/a+/<{0}>/g" ls === printed "s/a+/<{0}>/g line" ls
    -- 「-p の答えは、Haskell で1行ずつ変えたものと同じ」
    it "prints each line changed, as Haskell would" $
      property $ forAll (listOf genLine) $ \ls ->
        printed "toUpper" ls === T.concat [ T.toUpper l <> "\n" | l <- ls ]
    -- ANCHOR_END: byTypeTest

    -- ANCHOR: jsonTest
    -- 「--json と --jsonl と --path を読み、一緒に使えないものは止める」
    it "reads --json, --jsonl and --path" $ do
      let one args = fmap snd (readCommand args)
      one ["--json", "--path=orders.0", "-pe", "x"] `shouldBe` Right
        (RunOneLiner noOneLiner { oneProgram = ["x"], oneMode = Printing
                                , oneInput = WholeJson
                                , onePath = ["orders", "0"] } [])
      map (either Left (const (Right ())) . readCommand)
        [ ["--path=a", "-ne", "x"], ["--json", "--jsonl", "-ne", "x"]
        , ["--path=a..b", "--json", "-ne", "x"]
        , ["--json", "-ane", "x"], ["--json", "-i", "-pe", "x", "f"] ]
        `shouldBe`
          map Left
            [ "--path goes with --json or --jsonl"
            , "--json and --jsonl cannot go together"
            , "--path needs keys: --path=orders.items"
            , "-a and -F go with text, not with --json or --jsonl"
            , "-i goes with text or --jsonl, without --path" ]
    -- 「配列を --json で読んでも、1行ずつ --jsonl で読んでも、同じ答え」
    it "gives the same for an array with --json and for --jsonl" $
      property $ forAll (listOf (chooseInteger (-50, 50))) $ \ns ->
        let objects = [ "{\"n\": " <> T.pack (show n) <> "}" | n <- ns ]
            array   = "[" <> T.intercalate ", " objects <> "]"
            run input text = outputOf $ snd $ runFake
              emptyWorld { worldFiles = Map.fromList [("in", text)] } $
              runOneLiner fakeWorld noFiles preludeSession
                noOneLiner { oneProgram = ["Json.get \"n\" json"]
                           , oneMode = Printing, oneInput = input
                           , onePath = [] } ["in"]
        in run WholeJson array === run JsonLines (T.unlines objects)
    -- 「--path は、キーと配列の番号でたどる。ないキーは Left」
    it "walks down a JSON with --path" $ do
      let j = either (error . T.unpack) id $ do
            v <- either (Left . T.pack) Right
                   (parseJson "{\"a\": [{\"b\": 7}, 8]}")
            Right (fromAeson v)
          shown = either id renderValue
      map (\p -> shown (selectPath p j))
        [["a", "0", "b"], ["a", "1"], ["a", "2"], ["a", "x"], ["c"]
        , ["a", "99999999999999999999"]]
        `shouldBe` ["JInt 7", "JInt 8", "2", "x", "c", "99999999999999999999"]
    -- ANCHOR_END: jsonTest

    -- ANCHOR: scriptTest
    -- 「--print-script が作ったスクリプトは、どの例でも、
    --   ワンライナーと同じ答え」
    it "makes scripts that do what the one-liners do" $ do
      names <- listDirectory "examples/oneliner"
      checked <- fmap concat $ mapM (\n -> do
        let base = "examples/oneliner" </> dropExtension n
        (opts, one, rest, world) <- exampleWorld base
        case scriptOf preludeSession one of
          Left _ -> pure []        -- a type error: no script to make
          Right script -> do
            dbs  <- memoryDatabases
            let handle = guarded opts (fakeWorldIO dbs)
            (e1, w1) <- runFakeIO world
              (runOneLiner handle (fakeFiles world) preludeSession one rest)
            dbs2 <- memoryDatabases
            (e2, w2) <- runFakeIO world
              (runProgram (guarded opts (fakeWorldIO dbs2))
                 (fakeFiles world) preludeSession "script.lam" script)
            (n, shownRun world e2 w2) `shouldBe` (n, shownRun world e1 w1)
            pure [n])
        [ n | n <- names, takeExtension n == ".cmd" ]
      length checked `shouldSatisfy` (>= 25)
    -- ANCHOR_END: scriptTest

    -- ANCHOR: inPlaceTest
    -- 「-i.bak は、元の中身を残してから書き換える。
    --   --dry-run なら何も書かない」
    it "keeps a backup with -i.bak, and writes nothing with --dry-run" $ do
      let world = emptyWorld
            { worldFiles = Map.fromList [("a.txt", "red\nblue\n")] }
          run args = do
            (opts, RunOneLiner one rest) <-
              either fail pure (readCommand args)
            dbs <- memoryDatabases
            runFakeIO world (runOneLiner (guarded opts (fakeWorldIO dbs))
                               (fakeFiles world) preludeSession one rest)
      (ending, after) <- run ["-i.bak", "-pe", "toUpper", "a.txt"]
      ending `shouldBe` Finished ""
      worldFiles after `shouldBe` Map.fromList
        [("a.txt", "RED\nBLUE\n"), ("a.txt.bak", "red\nblue\n")]
      (_, dry) <- run ["--dry-run", "-i", "-pe", "toUpper", "a.txt"]
      worldFiles dry `shouldBe` worldFiles world
      errorsOf dry `shouldBe` "dry-run: writeFile \"a.txt\" (9 letters)\n"
    -- ANCHOR_END: inPlaceTest

    -- ANCHOR: helpersTest
    -- 「toInt は、文字列の全部が数のときだけ。toIntOr は、決めた数で受ける」
    it "reads a whole number with toInt, or uses the number given" $
      runLines [ "map toInt [\"42\", \" -7 \", \"+3\", \"12abc\", \"\"]"
               , "toIntOr 0 \"x\" + toIntOr 0 \"5\"" ]
        `shouldBe`
          [ "[Just 42, Just (-7), Just 3, Nothing, Nothing] : [Maybe Int]"
          , "5 : Int" ]
    it "agrees with Haskell's read on any number" $
      property $ \n ->
        runLines ["toInt \"" <> T.pack (show (n :: Integer)) <> "\""]
        === [ (if n < 0 then "Just (" <> T.pack (show n) <> ")"
                        else "Just " <> T.pack (show n)) <> " : Maybe Int" ]
    -- ANCHOR_END: helpersTest

    -- ANCHOR: precisionTest
    -- 「{x:.1} は Json.fixed 1 x に開き、幅とも一緒に使える」
    it "opens {x:.1} into Json.fixed, and lines it up with a width" $ do
      fmap render (parseExpr "f\"{x:.1}\"")
        `shouldBe` Right "Json.fixed 1 x"
      runLines [ "f\"[{Json.ratio 217 3:.1}] [{Json.ratio 2 3:>6.2}]\""
               , "f\"{Json.ratio (0 - 1) 8:.2} {Json.ratio 5 1:.0}\"" ]
        `shouldBe`
          [ "\"[72.3] [  0.67]\" : String", "\"-0.13 5\" : String" ]
    -- 「. のまちがいも、動かす前に見つける。数でない値は型のエラー」
    it "finds the mistakes of a precision, and its type" $ do
      map (fmap (const ()) . parseExpr)
        [ "f\"{x:.}\"", "f\"{x:05.1}\"", "f\"{x:,.1}\"", "f\"{x:.1>}\"" ]
        `shouldSatisfy` all isLeft
      runLines ["f\"{3:.1}\""] `shouldBe`
        ["type error: the types do not fit: Int where Json is needed"]
    -- 「Json.ratio を fixed で丸めると、Rational で割って丸めたものと同じ」
    it "rounds Json.ratio as Rational does" $
      property $ forAll (chooseInteger (-100000, 100000)) $ \a ->
        forAll (chooseInteger (1, 999)) $ \b ->
          let lit k | k < 0 = "(0 - " <> T.pack (show (negate k)) <> ")"
                    | otherwise = T.pack (show k)
              exact = fromInteger a / fromInteger b :: Rational
              -- 10^12 times, cut toward 0, as ratio keeps it
              -- ratio と同じく、10^12 倍して 0 の方へ切る
              kept = truncate (exact * 10 ^ (12 :: Int)) :: Integer
          in runLines [ "Json.fixed 2 (Json.ratio " <> lit a <> " "
                          <> lit b <> ")" ]
               === [ "\"" <> fixedOf 2 kept 12 <> "\" : String" ]
    -- ANCHOR_END: precisionTest

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
    -- 「区切りでつなぎ直すと、元の文字列」
    -- 「区切りが空でなければ、Haskell と同じ」
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
        , "slice 1 (0 - 1) \"Lam!\""
        , "let palindrome s = s == reverse s"
        , ":type palindrome"
        , "map palindrome [\"level\", \"lam\"]"
        , "reverse (Just 1)" ]
        `shouldBe`
          [ "\"cba\" : String"
          , "[3, 2, 1] : [Int]"
          , "\"Has\" : String"
          , "[2, 3] : [Int]"
          , "\"am\" : String"
          , "palindrome : (Comparable a, Sequence a) => a -> Bool"
          , "(Comparable a, Sequence a) => a -> Bool"
          , "[true, false] : [Bool]"
          , "type error: Maybe Int is not in the class Sequence" ]
    -- 「slice は Python の s[i:j] と同じ（負の数は後ろから）」
    it "slices as Python's s[i:j] does" $
      property $ \(i, j) -> forAll genText $ \text ->
        let len = T.length text
            place k = max 0 (min len (if k < 0 then len + k else k))
            python = T.drop (place i) (T.take (place j) text)
            lit k = if k < 0 then "(0 - " <> T.pack (show (negate k)) <> ")"
                             else T.pack (show k)
        in runLines [ "slice " <> lit (i :: Int) <> " " <> lit (j :: Int)
                        <> " " <> renderString text ]
           === [ renderString python <> " : String" ]
    -- 「文字列の reverse は Haskell と同じ」
    it "reverses a string as Haskell does" $
      property $ forAll genText $ \text ->
        runLines ["reverse " <> renderString text]
          === [renderString (T.reverse text) <> " : String"]
    -- ANCHOR_END: toolboxSequence

    -- 「モジュール List：空の maximum は Nothing」
    it "gives Nothing for the maximum of an empty list" $
      runLines [ "List.maximum [95, 88, 79]", "List.maximum []"
               , ":type List.maximum" ]
        `shouldBe` [ "Just 95 : Maybe Int"
                   , "Nothing : Comparable a => Maybe a"
                   , "Comparable a => [a] -> Maybe a" ]

    -- ANCHOR: toolboxFoldable
    -- 「Foldable：リストにも Map にも Maybe にも」
    it "folds lists, maps and Maybes alike" $
      runLines
        [ ":type any"
        , "any even [1, 3, 4]"
        , "sum {python: 82, ts: 75}"
        , "maximum {python: 82, ts: 75}"
        , "maximum []"
        , "sum (Just 3)"
        , "elem \"Lam\" (Just \"Lam\")"
        , "toList {b: 2, a: 1}"
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
          , "type error: the types do not fit: Int where f Int is needed"
          , "type error: the types do not fit: String where f Int is needed" ]
    -- 「maximum は、Haskell の maximum と同じ。空なら Nothing」
    it "finds the same maximum as Haskell, and Nothing for []" $
      property $ \xs ->
        map valuePart
          (runLines ["maximum " <> lamInts xs, "minimum " <> lamInts xs])
          === map valuePart
                [ maybeInt (if null xs then Nothing else Just (maximum xs))
                , maybeInt (if null xs then Nothing else Just (minimum xs)) ]
    -- 「Map の sum・any は、値のリストと同じ」
    it "folds the values of a map in the order of the keys" $
      property $ \kvs ->
        let m = Map.fromList [ (T.pack (show (k :: Int)), v :: Integer)
                             | (k, v) <- kvs ]
            lit = "(Map.fromList [" <> T.intercalate ", "
                    [ "(" <> renderString k <> ", " <> lamInt v <> ")"
                    | (k, v) <- Map.toList m ] <> "])"
        in runLines [ "sum " <> lit, "foldr (\\x acc -> x - acc) 0 " <> lit ]
           === [ shownInt (sum m) <> " : Int"
               , shownInt (foldr (-) 0 m) <> " : Int" ]
    -- ANCHOR_END: toolboxFoldable

    -- ANCHOR: toolboxPending
    -- 「f がわからないうちの条件は待ち、最後まで決まらなければリストに」
    it "waits for f, and makes it a list when it is never known" $
      runLines
        [ "let total t = sum t"
        , "let average xs = sum xs / length xs"
        , "average [80, 90]"
        , "let grow t = map (\\x -> x + 1) t ++ [0]"
        , "(\\t -> length (map (\\x -> x) t)) (Just 1)" ]
        `shouldBe`
          [ "total : Foldable f => f Int -> Int"
          , "average : [Int] -> Int"
          , "85 : Int"
          , "grow : [Int] -> [Int]"
          , "type error: Maybe Int is not in the class HasLength" ]
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
        , "map toUpper \"abc\"" ]
        `shouldBe`
          [ "Functor f => (a -> b) -> f a -> f b"
          , "[2, 4] : [Int]"
          , "Just 8 : Maybe Int"
          , "{\"a\": 2, \"b\": 4} : Map Int"
          , "Ok 6 : Result a Int"
          , "Err \"no\" : Result String Int"
          , "type error: the types do not fit: String where f String"
              <> " is needed" ]
    -- ANCHOR_END: toolboxFunctor

    -- ANCHOR: toolboxLists
    -- 「リストの道具は、Haskell の zip・nub・partition と同じ」
    it "zips, numbers, splits and makes unique as Haskell does" $
      property $ \xs ys ->
        map valuePart
          (runLines [ "zip " <> lamInts xs <> " " <> lamInts ys
                    , "unique " <> lamInts xs
                    , "partition even " <> lamInts xs
                    , "map fst (indexed " <> lamInts xs <> ")" ])
          === map valuePart [ "[" <> T.intercalate ", "
                  [ "(" <> shownInt a <> ", " <> shownInt b <> ")"
                  | (a, b) <- zip xs ys ] <> "] : [(Int, Int)]"
              , shownInts (nub xs) <> " : [Int]"
              , "(" <> shownInts (filter even xs) <> ", "
                  <> shownInts (filter odd xs) <> ") : ([Int], [Int])"
              , shownInts (map toInteger [0 .. length xs - 1]) <> " : [Int]" ]
    -- ANCHOR_END: toolboxLists

    -- ANCHOR: toolboxMaps
    -- 「Map の道具は、Haskell の Map と同じ（unionWith は引数の順も）」
    it "unions, filters and updates maps as Haskell's Map does" $
      property $ \as bs k ->
        let mapOf kvs = Map.fromList
              [ (T.pack (show (key :: Int)), v :: Integer) | (key, v) <- kvs ]
            a = mapOf as
            b = mapOf bs
            key = T.pack (show (k :: Int))
            lit m = "(Map.fromList [" <> T.intercalate ", "
                      [ "(" <> renderString x <> ", " <> lamInt v <> ")"
                      | (x, v) <- Map.toList m ] <> "])"
            shown m = "{" <> T.intercalate ", "
                        [ renderString x <> ": " <> shownInt v
                        | (x, v) <- Map.toList m ] <> "}"
            bump = \n -> case n of
              Just v | v > 0 -> Just (v - 1)
              _              -> Nothing
        in map valuePart (runLines
             [ "Map.unionWith (\\x y -> x - y) " <> lit a <> " " <> lit b
             , "Map.filter (\\v -> v > 0) " <> lit a
             , "Map.update " <> renderString key
                 <> " (\\n -> case n of | Just v -> if v > 0"
                 <> " then Just (v - 1) else Nothing | Nothing -> Nothing) "
                 <> lit a
             , "Map.union " <> lit a <> " " <> lit b ])
           === map shown [ Map.unionWith (-) a b, Map.filter (> 0) a
                         , Map.alter bump key a, Map.union a b ]
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


  -- 「ファイルとコマンドと時計」
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
      let world = emptyWorld { worldFiles = Map.fromList
            [("logs/a.log", "a"), ("logs/b.txt", "b")] }
          (out, after) = runLinesIn world
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
      Map.keys (worldFiles after) `shouldBe` ["old/2026/a.log"]
    -- ANCHOR_END: folderTest

    -- ANCHOR: cleanupDryRun
    -- 「--dry-run の片付け：何も消さずに、消すはずのものを知らせる」
    it "removes nothing with --dry-run, and tells what it would" $ do
      dbs <- memoryDatabases
      (text, after) <- runExampleFull dbs (\o -> o { dryRunning = True }) id
                         "examples/files/cleanup"
      let logs = Map.filterWithKey
            (\k _ -> "examples/files/logs/" `T.isPrefixOf` k) (worldFiles after)
      Map.size logs `shouldBe` 5
      T.lines text `shouldContain`
        [ "dry-run: File.remove \"examples/files/logs/app-2026-11-01.log\"" ]
    -- ANCHOR_END: cleanupDryRun

    -- ANCHOR: guardFiles
    -- 「見張り：glob は探し始めるフォルダーを読む。コマンドは名前で許す」
    it "needs read for a glob's folder, and run for a command's name" $ do
      map globRoot ["logs/*.log", "*.txt", "a/b/c.txt", "a/*/c/*.log"]
        `shouldBe` ["logs", ".", "a/b/c.txt", "a"]
      needs (Rename "a" "b") `shouldBe` [(Write, "a"), (Write, "b")]
      let Right (opts, _) = readOptions ["--allow-run=git", "x.lam"]
          world = emptyWorld { worldCommands = Map.fromList
            [ (["git", "log"], (0, "ok\n", ""))
            , (["rm", "x"], (0, "", "")) ] }
          (out, _) = runFake world (mapM (guarded opts fakeWorld)
            [ RunProcess "git" ["log"] "", RunProcess "rm" ["x"] ""
            , Exists "a.txt" ])
      map renderValue out `shouldBe`
        [ "Succeeded { stderr = \"\", stdout = \"ok\\n\" }"
        , "CannotStart \"rm: needs --allow-run=rm\"", "false" ]
    -- ANCHOR_END: guardFiles

    -- ANCHOR: processTest
    -- 「Process：3つの終わり方と、output」
    it "tells the three endings of a command apart" $ do
      let world = emptyWorld { worldCommands = Map.fromList
            [ (["ls"], (0, "a\n", "")), (["ls", "x"], (1, "", "no x\n")) ] }
      map valuePart (fst (runLinesIn world
        [ "Process.run \"ls\" []", "Process.run \"ls\" [\"x\"]"
        , "Process.run \"nope\" []"
        , "Process.output (Failed { code = 1, stdout = \"\", "
            <> "stderr = \"no x\\n\" })" ]))
        `shouldBe`
          [ "Succeeded { stderr = \"\", stdout = \"a\\n\" }"
          , "Failed { code = 1, stderr = \"no x\\n\", stdout = \"\" }"
          , "CannotStart \"nope: no such command\""
          , "Err \"exit 1: no x\"" ]
    -- ANCHOR_END: processTest

    -- ANCHOR: timeTest
    -- 「Time：ISO 8601 で書いて読み戻すと同じ。差は切り捨て」
    it "reads back the ISO 8601 it writes" $
      property $ forAll (chooseInteger (0, 4102444800000)) $ \ms ->
        valuePart (head (runLines
          [ "Time.fromIso (Time.toIso (Time.fromMillis "
              <> T.pack (show ms) <> ")) == Ok (Time.fromMillis "
              <> T.pack (show ms) <> ")" ])) === "true"
    it "reads a zone, and cuts the days down" $
      map valuePart (runLines
        [ "Time.fromIso \"2026-11-17T12:00:00+09:00\""
            <> " |> Result.map Time.toIso"
        , "Time.fromIso \"2026-11-17\" |> Result.map Time.toIso"
        , "Time.fromIso \"yesterday\""
        , "Time.diffDays (Time.fromMillis 172799999) (Time.fromMillis 0)"
        , "Time.format (Time.utc) (Time.fromMillis 0)" ])
        `shouldBe`
          [ "Ok \"2026-11-17T03:00:00Z\"", "Ok \"2026-11-17T00:00:00Z\""
          , "Err \"not an ISO 8601 time: yesterday\"", "1"
          , "\"1970-01-01 00:00:00 +00:00\"" ]
    -- ANCHOR_END: timeTest

    -- ANCHOR: randomTest
    -- 「Random：範囲の中で、同じ種なら同じ答え。shuffle は並べ替えるだけ」
    it "throws dice in the range, the same for the same seed" $
      property $ \seed (NonNegative lo) (Positive w) ->
        let world = emptyWorld { worldRandom = mkStdGen seed }
            lines' = [ "Random.int " <> num lo <> " " <> num (lo + w)
                     , "Random.shuffle [1, 2, 3, 4, 5, 6, 7]" ]
            out = map valuePart (fst (runLinesIn world lines'))
            n = read (T.unpack (head out)) :: Integer
            shuffled = T.splitOn ", " (T.dropEnd 1 (T.drop 1 (out !! 1)))
        in n >= lo .&&. n <= lo + w
             .&&. sortOn id shuffled === map (T.pack . show) [1 .. 7 :: Int]
             .&&. out === map valuePart (fst (runLinesIn world lines'))
    -- ANCHOR_END: randomTest

  -- 「型の注釈とレコード」
  describe "annotations and records" $ do
    -- ANCHOR: annotationTest
    -- 「let のない定義、注釈、where を読む。ファイルの終わりの x : xs は式」
    it "reads definitions without let, annotations and where" $ do
      let decls src = fmap (\(Program _ _ ds final) ->
            ( [ case d of
                  DefValue x e -> x <> " = " <> render (stripLocations e)
                  DefSig _ x _ -> x <> " : ..."
                  _            -> "?"
              | d <- ds ]
            , fmap render final ))
            (runLamParser program "" src)
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
        , ":type \\p -> case p of | { x, y } -> x + y"
        , "(\\r -> r.name) { name = \"Ruby\", version = \"4.0\" }"
        , "{ name = \"Lam\" }.age"
        , "{ name = \"Lam\" } == { name = \"Lam\" }"
        , "{ f = \\x -> x } == { f = \\x -> x }" ]
        `shouldBe`
          [ "{ age = 3, name = \"Lam\" } : { age : Int, name : String }"
          , "{ r | name : a } -> a"
          , "{ r | age : a, name : b } -> (b, a)"
          , "{ r | age : Int } -> { r | age : Int }"
          , "{ r | name : a } -> a"
          , "{ r | x : Int, y : Int } -> Int"
          , "\"Ruby\" : String"
          , "type error: { name : String } has no field age"
          , "true : Bool"
          , "type error: a -> a is not in the class Comparable" ]
    -- ANCHOR_END: recordTypesTest

    -- 「本文の REPL のやり取り（上から順に、同じセッションで）」
    it "answers the REPL lines of the chapter" $
      runLines
        [ ":type sum", "total : [Int] -> Int; total xs = sum xs"
        , "r = { name = \"Lam\", age = 3 }", "r.name", "{ r | age = 4 }", "r"
        , "{name: \"Lam\"}", "r.title", "{ r | title = \"x\" }"
        , "{ r | age = \"four\" }", "label r = \"[\" ++ r.name ++ \"]\""
        , "label r", "label { title = \"x\" }", ":type File.info" ]
        `shouldBe`
          [ "Foldable f => f Int -> Int", "total : [Int] -> Int"
          , "r : { age : Int, name : String }", "\"Lam\" : String"
          , "{ age = 4, name = \"Lam\" } : { age : Int, name : String }"
          , "{ age = 3, name = \"Lam\" } : { age : Int, name : String }"
          , "{\"name\": \"Lam\"} : Map String"
          , "type error: { age : Int, name : String } has no field title"
          , "type error: { age : Int, name : String } has no field title"
          , "type error: the types do not fit: String where Int is needed"
          , "label : { r | name : String } -> String", "\"[Lam]\" : String"
          , "type error: { title : String } has no field name"
          , "String -> IO (Result String { modified : Time, size : Int })" ]

    -- ANCHOR: updateProp
    -- 「一部を変えたレコード：変えたフィールドは新しい値、ほかはそのまま」
    it "changes only the fields it names" $
      property $ \(a, b, n) ->
        let r = "{ a = " <> num a <> ", b = " <> num b <> " }"
            changed = "{ r | a = " <> num n <> " }"
        in map valuePart (runLines
             [ inRecord r (changed <> ".a")
             , inRecord r (changed <> ".b")
             , inRecord r "r.a" ])
           === map (T.pack . show) [n, b, a]
    -- ANCHOR_END: updateProp

    -- ANCHOR: aliasTest
    -- 「別名は、中身の型と同じ。モジュールからも見せられる」
    it "takes an alias as the type it stands for, from a module too" $ do
      runLines
        [ "type alias User = { name : String, age : Int }"
        , "older : User -> User; older u = { u | age = u.age + 1 }"
        , "older { name = \"Lam\", age = 3 }"
        , "type alias Pair a = (a, a)"
        , "both : Pair Int -> Int; both p = fst p + snd p"
        , "type alias Bad = Bad" ]
        `shouldBe`
          [ "type alias User"
          , "older : { age : Int, name : String } -> { age : Int, name : String }"
          , "{ age = 4, name = \"Lam\" } : { age : Int, name : String }"
          , "type alias Pair"
          , "both : (Int, Int) -> Int"
          , "in the type alias Bad: type error: unknown type: Bad" ]
      outputOf (snd (runModules
        [ ("Team.lam", T.unlines
            [ "module Team exposing (Member, crew)"
            , "type alias Member = { name : String, days : Int }"
            , "crew : [Member]"
            , "crew = [{ name = \"Lam\", days = 20 }]" ]) ]
        (T.unlines
          [ "import Team"
          , "names : [Member] -> [String]"
          , "names ms = map .name ms"
          , "main = putStrLn (show (names Team.crew))" ])))
        `shouldBe` "[\"Lam\"]\n"
    -- ANCHOR_END: aliasTest

  -- 「複数行」
  describe "program" $
    -- ANCHOR: layout
    -- 「1桁目から始まる行は、新しい定義（か、最後の式）」
    it "starts a new definition at column 1" $
      fmap (\(Program _ _ decls final) ->
              ([ x | DefValue x _ <- decls ], fmap render final))
        (runLamParser program "" "let a = 1\nlet b =\n  a + 1\nb\n")
        `shouldBe` Right (["a", "b"], Just "b")
    -- ANCHOR_END: layout

-- ANCHOR: runModules
-- 🗂️ Run main.lam in a fake world that has these files (the modules).
-- この（モジュールの）ファイルがある偽物の世界で、main.lam を動かす
runModules :: [(Text, Text)] -> Text -> (Ending, FakeWorld)
runModules files source =
  runFake world
    (runProgram fakeWorld (fakeFiles world) preludeSession "main.lam" source)
  where
    world = emptyWorld { worldFiles = Map.fromList files }

-- 🗂️ The same, with Shapes.lam, and only how it ended.
-- 同じく、Shapes.lam を置いて。どう終わったかだけ
withShapes :: Text -> Ending
withShapes = fst . runModules [("Shapes.lam", shapes)]
  where
    shapes = T.unlines
      [ "module Shapes exposing (Shape (..), area, square)"
      , "data Shape = Circle Int | Rect Int Int"
      , "let area s = case s of | Circle r -> 3 * r * r | Rect w h -> w * h"
      , "let square n = Rect n n"
      , "let secret = 42" ]
-- ANCHOR_END: runModules

-- ANCHOR: script
-- 🎭 Run a script in a fake world: how it ended, and the world after.
-- 偽物の世界でスクリプトを動かす：どう終わったかと、あとの世界
script :: FakeWorld -> Text -> (Ending, FakeWorld)
script world source =
  runFake world
    (runProgram fakeWorld (fakeFiles world) preludeSession
       "script.lam" source)
-- ANCHOR_END: script

-- ANCHOR: scriptIO
-- 🎭 Run a script in the fake world with databases (and the guards).
-- データベースのある偽物の世界で、スクリプトを動かす（見張りつき）
scriptIO
  :: Databases -> Options -> FakeWorld -> Text -> IO (Ending, FakeWorld)
scriptIO dbs opts world source =
  runFakeIO world
    (runProgram (guarded opts (fakeWorldIO dbs)) (fakeFiles world)
       preludeSession "script.lam" source)

-- 📨 Requests of every kind, for the property of allowOnly.
-- いろいろな頼みごと（allowOnly の性質のため）
sampleRequests :: [Request]
sampleRequests =
  [ PutStr "hi", GetLine, ReadFile "a.txt", WriteFile "a.txt" "x"
  , GetArgs, LookupEnv "HOME", GetEnvironment
  , HttpSend "GET" "https://x.test/" [] ""
  , HttpSend "POST" "https://x.test/" [] "{}"
  , SqlOpen "a.db" ReadWrite, SqlOpen "a.db" ReadOnly
  , SqlOpen ":memory:" ReadWrite, SqlQuery 1 "SELECT 1" []
  , SqlExec 1 "DELETE FROM t" [], SqlClose 1
  , ListDir "logs", Glob "logs/*.log", Exists "a.txt", IsDir "logs"
  , FileInfo "a.txt", MakeDir "out", Remove "a.txt", Rename "a.txt" "b.txt"
  , CopyFile "a.txt" "out/a.txt", AppendFile "a.txt" "x"
  , RunProcess "git" ["log"] "", RunProcess "ls" [] ""
  , Now, Here, RandomInt 1 6 ]

-- 🎲 A cell, and how Lam shows it when it comes back. A decimal has
--    at most 9 digits, and does not end in 0 (17.40 comes back 17.4).
-- マスと、戻ってきたときの Lam の見え方。小数は9けたまでで、0 で
-- 終わらない（17.40 は 17.4 で戻る）
genCell :: Gen (Cell, Text)
genCell = oneof
  [ pure (CNull, "JNull")
  , (\i -> (CInt i, "JInt " <> paren i))
      <$> chooseInteger (-(2 ^ (63 :: Int)), 2 ^ (63 :: Int) - 1)
  , do d <- chooseInt (1, 4)
       c <- chooseInteger (-999999999, 999999999)
              `suchThat` (\c -> c `mod` 10 /= 0)
       pure (CDec c d, "JDec " <> paren c <> " " <> T.pack (show d))
  , (\t -> (CText t, "JStr " <> renderString t)) . T.pack
      <$> listOf arbitraryPrintableChar ]
  where
    paren n = if n < 0 then "(" <> T.pack (show n) <> ")" else T.pack (show n)
-- ANCHOR_END: scriptIO

-- ANCHOR: fixedOf
-- 🔢 The answer of Json.fixed, from Rational: c / 10^d with n digits,
--    rounded half away from zero. Made without Lam, to compare.
-- Json.fixed の答えを Rational で。Lam を使わずに作って、くらべる
fixedOf :: Integer -> Integer -> Integer -> Text
fixedOf n c d =
  let r = fromInteger c / 10 ^^ d :: Rational
      q = floor (abs r * 10 ^ n + 1 / 2) :: Integer
      v = if r < 0 then negate q else q
      (whole, frac) = abs v `divMod` (10 ^ n)
      sign = if v < 0 then "-" else ""
      fracText = T.justifyRight (fromInteger n) '0' (T.pack (show frac))
  in sign <> T.pack (show whole) <> (if n == 0 then "" else "." <> fracText)
-- ANCHOR_END: fixedOf

-- ANCHOR: oneLinerGolden
-- 🧪 Run examples/oneliner/<name>.cmd (the words after lam) in a fake
--    world: the files of the folder are there, and the keyboard types
--    <name>.in. Then the standard output, the standard error, how it
--    ended, and each file that -i changed or made (as the real
--    make-oneliner-out.sh shows them).
-- examples/oneliner/<名前>.cmd（lam の後ろの語）を偽物の世界で動かす。
-- フォルダーのファイルを置き、キーボードは <名前>.in を打つ。答えは、
-- 標準出力・標準エラー・終わり方と、-i が変えたか作ったファイル
-- （本物で作る make-oneliner-out.sh と同じ見せ方）
runOneLinerExample :: FilePath -> IO Text
runOneLinerExample base = do
  (opts, one, rest, world) <- exampleWorld base
  dbs <- memoryDatabases
  (ending, after) <- runFakeIO world
    (if oneScript one
       then runScriptFlag (guarded opts (fakeWorldIO dbs)) preludeSession one
       else runOneLiner (guarded opts (fakeWorldIO dbs)) (fakeFiles world)
              preludeSession one rest)
  pure (shownRun world ending after)

-- 🎭 The flags, the one-liner, the words after it, and the fake world
--    of an example.
-- 例のフラグ・ワンライナー・後ろの語・偽物の世界
exampleWorld :: FilePath -> IO (Options, OneLiner, [String], FakeWorld)
exampleWorld base = do
  cmd   <- Utf8.readFile (base <> ".cmd")
  there <- doesFileExist (base <> ".in")
  input <- if there then T.lines <$> Utf8.readFile (base <> ".in")
                    else pure []
  let dir = takeDirectory base
  names <- listDirectory dir
  files <- mapM (\n -> (,) (T.pack (dir </> n)) <$> Utf8.readFile (dir </> n))
             [ n | n <- names
                 , takeExtension n `notElem` [".cmd", ".out", ".in"] ]
  (opts, command) <- either fail pure (readCommand (shellWords cmd))
  (one, rest) <- case command of
    RunOneLiner one rest -> pure (one, rest)
    other                -> fail ("not a one-liner: " <> show other)
  pure (opts, one, rest, emptyWorld { worldInput = input
                                    , worldArgs  = map T.pack rest
                                    , worldFiles = Map.fromList files })

-- 🖥️ How a run looks: the standard output, the standard error, how it
--    ended, and each file that it changed or made.
-- 動かした様子：標準出力・標準エラー・終わり方・変えたか作ったファイル
shownRun :: FakeWorld -> Ending -> FakeWorld -> Text
shownRun world ending after =
  outputOf after <> errorsOf after <> shown <> T.concat changed
  where
    shown = case ending of
      Finished ""  -> ""
      Finished out -> out <> "\n"
      Errored err  -> err <> "\n[exit 1]\n"
      ExitedWith 0 -> ""
      ExitedWith n -> "[exit " <> T.pack (show n) <> "]\n"
    changed = [ "== " <> f <> " ==\n" <> t
              | (f, t) <- Map.toList (worldFiles after)
              , Map.lookup f (worldFiles world) /= Just t ]

-- ✂️ The words of a command line: spaces cut, '...' keeps them.
-- コマンドの行の語：空白で切る。'...' の中は切らない
shellWords :: Text -> [String]
shellWords = go . T.unpack . T.strip
  where
    go s = case dropWhile (== ' ') s of
      ""  -> []
      s'  -> let (w, rest) = word s' in w : go rest
    word s = case s of
      []           -> ("", [])
      ' ' : rest   -> ("", rest)
      '\'' : rest  ->
        let (quoted, after) = break (== '\'') rest
            (w, r) = word (drop 1 after)
        in (quoted <> w, r)
      c : rest     -> let (w, r) = word rest in (c : w, r)

-- 🖨️ What -pe 'program' prints for these lines, in a fake world.
-- これらの行に、-pe 'プログラム' が出すもの（偽物の世界で）
printed :: Text -> [Text] -> Text
printed program ls =
  outputOf $ snd $ runFake emptyWorld { worldInput = ls } $
    runOneLiner fakeWorld noFiles preludeSession
      noOneLiner { oneProgram = [program], oneMode = Printing } []

-- 🎲 A line of a, b and spaces (so that s/a+/.../g finds something).
-- a・b・空白の行（s/a+/…/g が見つけるものがあるように）
genLine :: Gen Text
genLine = T.pack <$> listOf (elements "aab ")
-- ANCHOR_END: oneLinerGolden

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

-- ANCHOR: genCalc
-- 🎲 A calculation and its value, written with spaces here and there,
--    and ( ) around some of the parts. No / (Haskell's div and Lam's
--    / are the same, but 0 would stop the test).
-- 計算と、その値。空白をところどころに入れ、部品のいくつかを ( ) で
-- 包む。/ は使わない（0 で割ると止まるため）
genCalc :: Gen (Text, Integer)
genCalc = sized (\n -> go (min n 6))
  where
    go :: Int -> Gen (Text, Integer)
    go 0 = number
    go n = frequency
      [ (1, number)
      , (3, do
          (a, x) <- go (n `div` 2)
          (b, y) <- go (n `div` 2)
          (o, f) <- elements [("+", (+)), ("-", (-)), ("*", (*))]
          sp <- elements ["", " ", "  "]
          pure ("(" <> a <> sp <> o <> sp <> b <> ")", f x y)) ]
    number = do
      k <- choose (0, 99)
      pure (T.pack (show k), k)

-- 🖨️ A number as an argument of Ok: Ok 3, Ok (-3).
-- Ok の引数としての数
okArgument :: Integer -> Text
okArgument v
  | v < 0     = "(" <> T.pack (show v) <> ")"
  | otherwise = T.pack (show v)

-- 📜 Load a program, then type lines into it.
-- プログラムを読みこんでから、行を打ちこむ
inProgram :: Text -> [Text] -> [Text]
inProgram source inputs =
  case runIdentity (loadProgram noFiles preludeSession "example" source) of
  Left err -> [err]
  Right (s, _) -> fst (runFake emptyWorld (go s inputs))
  where
    go _ [] = pure []
    go s (line : rest) = do
      (s', out) <- step fakeWorld s line
      (out :) <$> go s' rest

-- ANCHOR_END: genCalc

-- ANCHOR: genRegexText
-- 🎲 Some regexes, and texts made of the letters they look for (and of
--    U+E000, the first letter the cutting could use as its mark).
-- いくつかの正規表現と、それが探す文字でできた文字列（切るときの印の
-- 候補 U+E000 も混ぜる）
regexes :: [Text]
regexes =
  ["\\d+", "a*", "[λラ]", "\\s", "(?<=a)b", "", "(a)|(b)", "^", "$", "b+?"]

genRegexText :: Gen Text
genRegexText = T.pack <$> listOf (elements "ab1 λラ\n\xE000\xE001")

-- 🪡 a1 b1 a2 b2 ... an (one more a than b).
-- 交互に並べる（a のほうが1つ多い）
zipLonger :: [Text] -> [Text] -> [Text]
zipLonger as bs = case (as, bs) of
  (a : as', b : bs') -> a : b : zipLonger as' bs'
  _                  -> as
-- ANCHOR_END: genRegexText

-- ANCHOR: genMaybe
-- 🎲 Nothing, or Just a small number (Lam has no negative numbers).
-- Nothing か、小さな数の Just（Lam には負の数がない）
genMaybe :: Gen (Maybe Integer)
genMaybe = oneof [pure Nothing, Just <$> choose (0, 3)]

-- 🖨️ Maybe and Bool, as Lam writes them.
-- Maybe と Bool を、Lam の書き方で
lamMaybe :: Maybe Integer -> Text
lamMaybe m = maybe "Nothing" (\n -> "Just " <> T.pack (show n)) m

lamBool :: Bool -> Text
lamBool b = if b then "true" else "false"
-- ANCHOR_END: genMaybe

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

-- 🖨️ A list of numbers, as Lam writes it: [1, 2, 3]
-- 数のリストを、Lam の書き方で
renderList :: [Integer] -> Text
renderList xs = "[" <> T.intercalate ", " (map (T.pack . show) xs) <> "]"

-- ANCHOR: inRecord
-- 📇 let r = <record> in <expression>, for the REPL.
-- REPL のため
inRecord :: Text -> Text -> Text
inRecord r e = "let r = " <> r <> " in " <> e
-- ANCHOR_END: inRecord
