{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The answers of the training of "Turning Lam into Python".
-- 「Python に変換する」のトレーニングの解答。
module Lam.Answers
  ( morePyPrims
  , lispBackend
  , rubyBackend
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, toUpper)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Lam.Backend (Backend (..))
import Lam.Core
import Lam.Syntax (Literal (..), Name, Op (..))
import Lam.ToPython (pyNatives, pyPrims)

-- ANCHOR: more
-- 🧰 Step 1: the names the runtime of Python has with the answer of
--    answers/runtime/answers.py: Time.format.
-- ステップ1：answers/runtime/answers.py の解答を足したときに、Python の
-- ランタイムが持つ名前
morePyPrims :: [Text]
morePyPrims = pyPrims <> ["Time.format"]
-- ANCHOR_END: more

-- ANCHOR: lisp
-- 🧺 Step 2: a Backend that only writes the lowered program, as
--    S-expressions: (define List.sum (lambda (xs) ...)). It runs
--    nothing, so it says it has what the runtime of Python has.
-- ステップ2：小さな木にしたプログラムを、S 式で書くだけの Backend。何も
-- 動かさないので、Python のランタイムと同じ名前を持つことにする
lispBackend :: Backend
lispBackend = Backend
  { backendName    = "Lisp"
  , backendCommand = "to-lisp"
  , backendSuffix  = ".lisp"
  , backendTarget  = Target (Set.fromList pyPrims) (Set.fromList pyNatives)
  , backendWrite   = \_ lowered -> T.unlines
      ([ "(define " <> globalHome g <> "." <> globalName g <> " "
           <> sexp (globalBody g) <> ")"
       | g <- loweredGlobals lowered ]
       <> ["(main " <> sexp (loweredMain lowered) <> ")"])
  }

sexp :: Core -> Text
sexp core = case core of
  CLit l       -> lit l
  CLocal x     -> x
  CGlobal h n  -> h <> "." <> n
  CPrim p      -> p
  CCon c _ _   -> c
  CLam _ _     -> let (xs, b) = params core
                  in "(lambda (" <> T.unwords xs <> ") " <> sexp b <> ")"
  CApp _ _     -> let (f, as) = spine core
                  in "(" <> T.unwords (map sexp (f : as)) <> ")"
  CLet x e b   -> "(let ((" <> x <> " " <> sexp e <> ")) " <> sexp b <> ")"
  CIf c a b    -> "(if " <> T.unwords (map sexp [c, a, b]) <> ")"
  COp o l r    -> "(" <> T.pack (show o) <> " " <> sexp l <> " " <> sexp r
                    <> ")"
  CTuple es    -> "(tuple " <> T.unwords (map sexp es) <> ")"
  CList es     -> "(list " <> T.unwords (map sexp es) <> ")"
  CCase e alts -> "(case " <> sexp e <> " "
                    <> T.unwords [ "(" <> T.pack (show p) <> " " <> sexp b
                                   <> ")" | (p, b) <- alts ] <> ")"
  CRecord fs   -> "(record " <> fields fs <> ")"
  CField e n   -> "(field " <> sexp e <> " " <> n <> ")"
  CUpdate e fs -> "(update " <> sexp e <> " " <> fields fs <> ")"
  where
    fields fs = T.unwords [ "(" <> n <> " " <> sexp v <> ")" | (n, v) <- fs ]
    spine c = case c of
      CApp f a -> let (g, as) = spine f in (g, as <> [a])
      _        -> (c, [])
-- ANCHOR_END: lisp

-- ANCHOR: ruby
-- 💎 Step 3: a small Backend for Ruby. Only Ints, Strings and Bools,
--    functions, let and if, and putStrLn with showInt: enough for a
--    factorial. Ruby's Integer is as long as it must be, and its / on
--    two Integers rounds down, as Lam's //.
-- ステップ3：Ruby の小さな Backend。Int・String・Bool、関数、let と if、
-- putStrLn と showInt だけ。階乗には足りる。Ruby の Integer は要るだけ
-- 長く、Integer どうしの / は、Lam の // と同じく下へ丸める
rubyBackend :: Backend
rubyBackend = Backend
  { backendName    = "Ruby"
  , backendCommand = "to-ruby"
  , backendSuffix  = ".rb"
  , backendTarget  = Target (Set.fromList (map fst rubyPrims)) Set.empty
  , backendWrite   = \file lowered -> T.unlines
      ( [ "# Made by lam to-ruby from " <> T.pack file
            <> ". Run it with ruby."
        , "PRIMS = {" ]
        <> [ "  " <> str p <> " => " <> r <> "," | (p, r) <- rubyPrims ]
        <> [ "}" ]
        <> [ prim p <> " = PRIMS.fetch(" <> str p <> ")"
           | p <- loweredPrims lowered ]
        <> [ global (globalHome g) (globalName g) <> " = "
               <> ruby (globalBody g)
           | g <- loweredGlobals lowered ]
        <> [ ruby (loweredMain lowered) <> ".()" ] )
  }

-- 📋 The runtime: an IO value is a function of no arguments, and
--    running it is calling it.
-- ランタイム：IO の値は引数のない関数で、動かすのは呼ぶこと
rubyPrims :: [(Name, Text)]
rubyPrims =
  [ ("IO.putStrLn", "->(s) { -> { $stdout.write(s + \"\\n\"); nil } }")
  , ("IO.returnIO", "->(x) { -> { x } }")
  , ("IO.bindIO", "->(m) { ->(f) { -> { f.(m.()).() } } }")
  , ("showInt", "->(i) { i.to_s }") ]

ruby :: Core -> Text
ruby core = case core of
  CLit l      -> lit l
  CLocal x    -> local x
  CGlobal h n -> global h n
  CPrim p     -> prim p
  CLam x b    -> "->(" <> local x <> ") { " <> ruby b <> " }"
  CApp f a    -> ruby f <> ".(" <> ruby a <> ")"
  CLet x e b  -> "(" <> local x <> " = " <> ruby e <> "; " <> ruby b <> ")"
  CIf c a b   -> "(" <> ruby c <> " ? " <> ruby a <> " : " <> ruby b <> ")"
  COp o l r | Just s <- lookup o ops -> "(" <> ruby l <> " " <> s <> " "
                                          <> ruby r <> ")"
  -- The others (case, lists, records) are not here yet.
  -- ほか（case・リスト・レコード）は、まだない
  _ -> "raise(\"not in Ruby yet\")"
  where
    ops = [ (Add, "+"), (Sub, "-"), (Mul, "*"), (IntDiv, "/"), (Eq, "==")
          , (Ne, "!="), (Lt, "<"), (Le, "<="), (Gt, ">"), (Ge, ">=")
          , (And, "&&"), (Or, "||"), (Concat, "+") ]

-- 🏷️ A name at the top is a constant of Ruby (it begins with a capital
--    letter); a local is a local (with _ after a word Ruby keeps).
-- 上の段の名前は、Ruby の定数（大文字で始まる）。ローカルはローカル
--    （Ruby が取ってある語には、後ろに _）
global :: Name -> Name -> Text
global h n = case T.uncons (word home) of
  Just (c, rest) -> T.cons (toUpper c) rest <> "_" <> word n
  Nothing        -> "M_" <> word n
  where
    home = last (T.splitOn "/" (T.replace ".lam" "" h))

prim :: Name -> Text
prim p = "Prim_" <> word p

local :: Name -> Text
local x
  | x `elem` keywords = word x <> "_"
  | otherwise = word x
  where
    keywords = [ "alias", "and", "begin", "break", "case", "class", "def"
               , "do", "else", "elsif", "end", "ensure", "for", "if", "in"
               , "module", "next", "nil", "not", "or", "redo", "rescue"
               , "retry", "return", "self", "super", "then", "undef"
               , "unless", "until", "when", "while", "yield" ]

word :: Text -> Text
word = T.map (\c -> if isAlphaNum c || c == '_' then c else '_')
-- ANCHOR_END: ruby

-- 🔢 A literal, for both: a string as JSON writes it, with # kept from
--    the #{...} of Ruby.
-- そのまま書く値。文字列は JSON の書き方で、Ruby の #{...} にならない
-- よう # を守る
lit :: Literal -> Text
lit l = case l of
  LInt n | n < 0     -> "(" <> T.pack (show n) <> ")"
         | otherwise -> T.pack (show n)
  LFloat x    -> T.pack (show x)
  LBool True  -> "true"
  LBool False -> "false"
  LString s   -> str s

str :: Text -> Text
str s = T.replace "#" "\\#" (TE.decodeUtf8 (BL.toStrict (Aeson.encode s)))
-- ANCHOR_END: all
