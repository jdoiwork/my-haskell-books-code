-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import Test.Hspec
import Test.QuickCheck

import Lam.Answers (roundTrips)
import Lam.Parser
import Lam.Prelude
import Lam.Pretty
import Lam.Program
import Lam.Repl
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: run
-- 🔁 Type the lines into a REPL with the prelude, one by one.
-- プレリュード入りの REPL に、1行ずつ打ちこむ
runLines :: [Text] -> [Text]
runLines inputs = snd (foldl go (preludeSession, []) inputs)
  where
    go (s, outs) line =
      let (s', out) = step s line in (s', outs <> [out])
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
-- 📜 Run an example program, as `lam file.lam` would show it.
-- 例のプログラムを、lam file.lam と同じように動かす
runExample :: FilePath -> IO Text
runExample file = do
  source <- Utf8.readFile file
  pure $ case loadProgram preludeSession file source of
    Right (_, Just out) -> out <> "\n"
    Right (_, Nothing)  -> ""
    Left err            -> err <> "\n"

-- 🥇 Golden test: the output must be the same as the .out file.
-- ゴールデンテスト：出力が .out ファイルと同じであること
golden :: String -> Spec
golden base =
  it ("examples/" <> base <> ".lam") $ do
    actual   <- runExample ("examples/" <> base <> ".lam")
    expected <- Utf8.readFile ("examples/" <> base <> ".out")
    actual `shouldBe` expected
-- ANCHOR_END: golden

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
            let (s', out) = step s line in (s', outs <> [out])
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
          [ "(a -> b -> b) -> b -> [a] -> b"
          , "(a -> b) -> [a] -> [b]"
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
      , "float/answers/stats", "float/answers/units"
      , "float/answers/safe" ]
      -- ANCHOR_END: floatGolden
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
          , "(a -> Bool) -> [a] -> Maybe a"
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
          [ "r : Number a => { age : a, name : String }"
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
      fmap snd (loadProgram preludeSession "<test>" "half : Int -> Int\ndouble x = x * 2\n")
        `shouldBe` Left "error: the type annotation of half needs the definition of half right below it"

  -- 「トレーニングの解答」
  describe "answers" $
    -- ANCHOR: answerRoundTrip
    -- 「ステップ4：見せた Float を読み直すと、同じ Float に戻る」
    it "reads a shown Float back as the same Float" $
      property roundTrips
    -- ANCHOR_END: answerRoundTrip

  -- 「キーワードで始まる名前」
  describe "names" $
    -- 「キーワードで始まる名前も読める（iffy、letter、cases、dataset）」
    it "reads names that start with a keyword" $
      runLines
        [ "let iffy = 1", "iffy + 1", "let letter = \"a\"", "letter"
        , "let cases = 2", "cases * 3", "let dataset = true", "dataset" ]
        `shouldBe`
          [ "iffy : Number a => a", "2 : Int", "letter : String"
          , "\"a\" : String", "cases : Number a => a", "6 : Int"
          , "dataset : Bool", "true : Bool" ]

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
          , "x : Number a => a", "9 : Int", "9 : Int"
          , "type error: a number and a function are mixed up \
            \(a -> b is not in the class Number): x -1 gives -1 to x \
            \(write x - 1), and -f x is (-f) x (write -(f x))"
          , "-11 : Int"
          , "double : Number a => a -> a", "8 : Int", "3 : Float"
          , "type error: the types do not fit: Float where Int is needed"
          , "3 : Float"
          , "type error: String is not in the class Number"
          , "3 : Int", "Number a => a", "n : Number a => a"
          , "3 : Int", "-4 : Int", "error: divided by zero"
          , "Nothing : Maybe Int", "Infinity : Float", "NaN : Float"
          , "Just 3.5 : Maybe Float", "Just 3 : Maybe Int"
          , "Just (-3) : Maybe Int", "Just 2 : Maybe Int"
          , "Just (-2) : Maybe Int", "Just (-3) : Maybe Int"
          , "Just (-2) : Maybe Int", "Nothing : Maybe Int", "3 : Int"
          , "3 : Int", "1 : Int", "0 : Int", "2 : Int"
          , "Number a => Maybe a -> a"
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
      let loaded = case loadProgram preludeSession "split.lam" source of
            Right (s, _) -> s
            Left err     -> error (T.unpack err)
          go (s, outs) line =
            let (s', out) = step s line in (s', outs <> [out])
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
  describe "program" $
    -- ANCHOR: layout
    -- 「1桁目から始まる行は、新しい定義（か、最後の式）」
    it "starts a new definition at column 1" $
      fmap (\(decls, final) ->
              ([ x | DefValue x _ <- decls ], fmap render final))
        (runLamParser program "" "let a = 1\nlet b =\n  a + 1\nb\n")
        `shouldBe` Right (["a", "b"], Just "b")
    -- ANCHOR_END: layout

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
