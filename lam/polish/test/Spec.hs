-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.IO.Utf8 as Utf8
import Test.Hspec
import Text.Megaparsec (parse)

import Lam.Parser
import Lam.Prelude
import Lam.Pretty
import Lam.Program
import Lam.Repl
import Lam.Types
-- ANCHOR_END: head

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
          , "Int -> Int -> Int", "Int -> Int -> Int" ]
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

  -- 「例のプログラム（ゴールデンテスト）」
  describe "examples" $
    mapM_ golden ["fib", "church", "broken", "answers"]

  -- 「複数行」
  describe "program" $
    -- ANCHOR: layout
    -- 「1桁目から始まる行は、新しい定義（か、最後の式）」
    it "starts a new definition at column 1" $
      fmap (\(defs, final) -> (map fst defs, fmap render final))
        (parse program "" "let a = 1\nlet b =\n  a + 1\nb\n")
        `shouldBe` Right (["a", "b"], Just "b")
    -- ANCHOR_END: layout
