-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Definitions and whole programs: shared by the REPL, program
--   files and the prelude.
-- 定義とプログラム全体：REPL、プログラムのファイル、プレリュードで共有する。
module Lam.Program
  ( Session (..)
  , emptySession
  , define
  , evaluate
  , loadProgram
  ) where

import qualified Data.Map as Map
import Data.Text (Text)

import Lam.Eval
import Lam.Parser (program)
import Lam.Syntax
import Lam.Types
import qualified Data.Text as T
import Text.Megaparsec (errorBundlePretty, parse)
-- ANCHOR_END: head

-- ANCHOR: session
-- 🗂️ What has been defined so far: types and values.
-- これまでに定義したもの：型と値
data Session = Session
  { sessionTypes  :: TypeEnv
  , sessionValues :: Env
  }

emptySession :: Session
emptySession = Session Map.empty Map.empty
-- ANCHOR_END: session

-- ANCHOR: define
-- 🏷️ Add one definition: check its type, then compute it.
--    The error says which definition went wrong.
-- 定義を1つ足す：型を確かめ、それから計算する。
-- エラーには、どの定義でまちがえたかを書く
define :: Session -> Name -> Expr -> Either Text (Session, Text)
define session x e =
  case inferDefinition (sessionTypes session) x e of
    Left err -> Left (inDefinition ("type error: " <> renderTypeError err))
    Right sc ->
      let env'   = Map.insert x v (sessionValues session)
          result = eval env' e
          v      = either (const (VInt 0)) id result
      in case result of
           Left err -> Left (inDefinition ("error: " <> renderError err))
           Right _  -> Right
             ( Session (Map.insert x sc (sessionTypes session)) env'
             , x <> " : " <> renderScheme sc )
  where
    -- 「x の定義で：……」
    inDefinition msg = "in the definition of " <> x <> ": " <> msg

-- 🧮 Check the type of an expression, then compute it.
-- 式の型を確かめ、それから計算する
evaluate :: Session -> Expr -> Either Text Text
evaluate session e =
  case inferType (sessionTypes session) e of
    Left err -> Left ("type error: " <> renderTypeError err)
    Right t  -> case eval (sessionValues session) e of
      Left err -> Left ("error: " <> renderError err)
      Right v  -> Right (renderValue v <> " : " <> renderType t)
-- ANCHOR_END: define

-- ANCHOR: load
-- 📜 Load a program: every definition in order, then the last
--    expression (if there is one).
-- プログラムを読みこむ：定義を順に全部、それから最後の式（あれば）
loadProgram
  :: Session -> FilePath -> Text -> Either Text (Session, Maybe Text)
loadProgram session file source =
  case parse program file source of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right (defs, final) -> do
      session' <- foldl addOne (Right session) defs
      case final of
        Nothing -> Right (session', Nothing)
        Just e  -> do
          out <- evaluate session' e
          Right (session', Just out)
  where
    addOne acc (x, e) = do
      s <- acc
      fst <$> define s x e
-- ANCHOR_END: load
-- ANCHOR_END: all
