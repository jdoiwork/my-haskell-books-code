-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Map as Map
import Data.Scientific (scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Control.Exception as E
import System.Timeout (timeout)
import Text.Megaparsec (parse)

import Lam.Answers
import Lam.Builtins (preludeConstructors)
import Lam.Json (fromAeson, toAeson)
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
          [ "(a -> b -> b) -> b -> [a] -> b"
          , "(a -> b) -> [a] -> [b]"
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
      , "result", "tree", "orders" ]
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
      | let table = constructors (sessionData preludeSession)
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
        , "Json.show (JArr [JDec 1999 2, JDec (0 - 5) 1, JInt 1000])" ]
        `shouldBe`
          [ "Ok (JArr [JInt 42, JDec 1999 2, JDec (-5) 1, JInt 1, JInt 1000, "
              <> "JInt 12345678901234567890]) : Result String Json"
          , "\"[19.99,-0.5,1000]\" : String" ]

    -- 「とても大きな指数でも、桁を並べない（2秒で終わる）」
    it "does not spell out the digits of a very large exponent" $ do
      let lines' = runLines
            [ "Json.parse \"1e1000000000\""
            , "Json.show (JDec 1 (0 - 1000000000))" ]
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

  -- 「トレーニングの解答」
  describe "answers" $ do
    -- 「Map で単語を数える」「値段をセントで足す」
    golden "count"
    golden "cents"
    -- ANCHOR: answerPretty
    -- 「Json.pretty で、字下げして書く」
    it "writes JSON with indent, with Json.pretty" $
      snd (step (withBuiltin prettyBuiltin preludeSession)
             ("Json.pretty (JObj {\"name\": JStr \"Lam\", "
                <> "\"tags\": JArr [JInt 1]})"))
        `shouldBe`
          ("\"{\\n  \\\"name\\\": \\\"Lam\\\",\\n  "
             <> "\\\"tags\\\": [\\n    1\\n  ]\\n}\" : String")
    -- ANCHOR_END: answerPretty

  -- 「複数行」
  describe "program" $
    -- ANCHOR: layout
    -- 「1桁目から始まる行は、新しい定義（か、最後の式）」
    it "starts a new definition at column 1" $
      fmap (\(decls, final) ->
              ([ x | DefValue x _ <- decls ], fmap render final))
        (parse program "" "let a = 1\nlet b =\n  a + 1\nb\n")
        `shouldBe` Right (["a", "b"], Just "b")
    -- ANCHOR_END: layout

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
