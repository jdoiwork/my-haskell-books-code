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
  , defineData
  , evaluate
  , loadProgram
  ) where

import qualified Data.Map as Map
import Data.Text (Text)

import Lam.Builtins
import Lam.Data
import Lam.Eval
import Lam.Json (jsonBuiltins)
import Lam.Parser (program)
import Lam.Syntax
import Lam.Types
import qualified Data.Text as T
import Text.Megaparsec (errorBundlePretty, parse)
-- ANCHOR_END: head

-- ANCHOR: session
-- 🗂️ What has been defined so far: types, values, and data types.
-- これまでに定義したもの：型、値、data の型
data Session = Session
  { sessionTypes  :: TypeEnv
  , sessionValues :: Env
  , sessionData   :: DataTable
  }

-- 🧰 A session with only the builtins (no prelude yet).
--    The rows for JSON come from Lam.Json.
-- 組み込みの関数だけのセッション（プレリュードは、まだ）。
-- JSON の行は Lam.Json から
emptySession :: Session
emptySession = Session
  { sessionTypes  = Map.fromList
      [ (builtinName p, builtinType p) | p <- allBuiltins ]
  , sessionValues = Map.fromList
      [ (builtinName p, builtinValue p) | p <- allBuiltins ]
  , sessionData   = noData
  }
  where
    allBuiltins = builtins <> jsonBuiltins
-- ANCHOR_END: session

-- ANCHOR: define
-- 🏷️ Add one definition: check its type, then compute it.
--    The error says which definition went wrong.
-- 定義を1つ足す：型を確かめ、それから計算する。
-- エラーには、どの定義でまちがえたかを書く
define :: Session -> Name -> Expr -> Either Text (Session, Text)
define session x e =
  case inferDefinition (sessionData session) (sessionTypes session) x e of
    Left err -> Left (inDefinition ("type error: " <> renderTypeError err))
    Right sc ->
      let env'   = Map.insert x v (sessionValues session)
          result = eval env' e
          v      = either (const (VInt 0)) id result
      in case result of
           Left err -> Left (inDefinition ("error: " <> renderError err))
           Right _  -> Right
             ( session
                 { sessionTypes  = Map.insert x sc (sessionTypes session)
                 , sessionValues = env' }
             , x <> " : " <> renderScheme sc )
  where
    -- 「x の定義で：……」
    inDefinition msg = "in the definition of " <> x <> ": " <> msg

-- ANCHOR: evaluate
-- 🧮 Check the type of an expression, then compute it.
-- 式の型を確かめ、それから計算する
evaluate :: Session -> Expr -> Either Text Text
evaluate session e =
  case inferType (sessionData session) (sessionTypes session) e of
    Left err -> Left ("type error: " <> renderTypeError err)
    Right t  -> case eval (sessionValues session) e of
      Left err -> Left ("error: " <> renderError err)
      Right v  -> Right (renderValue v <> " : " <> renderScheme t)
-- ANCHOR_END: evaluate
-- ANCHOR_END: define

-- ANCHOR: defineData
-- 📐 Add a data type. The constructors go into the environment, as
--    functions (or, without arguments, as values).
--    The answer shows the type of each constructor.
-- data の型を足す。構成子は、関数として（引数がなければ値として）
-- 環境に入る。答えには、構成子ごとの型を見せる
defineData :: Session -> DataDecl -> Either Text (Session, Text)
defineData session decl =
  case declareData (sessionData session) decl of
    Left err -> Left ("in the data type " <> dataName decl
                        <> ": type error: " <> renderTypeError err)
    Right (table, cons) -> Right
      ( session
          { sessionTypes  = Map.fromList cons `Map.union` sessionTypes session
          , sessionValues = Map.fromList values `Map.union` sessionValues session
          , sessionData   = table }
      , T.intercalate "\n"
          [ c <> " : " <> renderScheme sc | (c, sc) <- cons ] )
  where
    values =
      [ (c, constructorValue c i (length args))
      | (i, (c, args)) <- zip [0 ..] (dataCons decl) ]

-- 🧱 Nothing is a value. Just waits for 1 argument, like a builtin.
-- Nothing は値。Just は、組み込みの関数と同じく、引数を1つ待つ
constructorValue :: Name -> Int -> Int -> Value
constructorValue c i arity
  | arity == 0 = VCon c i []
  | otherwise  = VPrim c arity (Right . VCon c i) []
-- ANCHOR_END: defineData

-- ANCHOR: load
-- 📜 Load a program: every declaration in order, then the last
--    expression (if there is one).
-- プログラムを読みこむ：宣言を順に全部、それから最後の式（あれば）
loadProgram
  :: Session -> FilePath -> Text -> Either Text (Session, Maybe Text)
loadProgram session file source =
  case parse program file source of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right (decls, final) -> do
      session' <- foldl addOne (Right session) decls
      case final of
        Nothing -> Right (session', Nothing)
        Just e  -> do
          out <- evaluate session' e
          Right (session', Just out)
  where
    addOne acc decl = do
      s <- acc
      fst <$> case decl of
        DefValue x e -> define s x e
        DefData d    -> defineData s d
-- ANCHOR_END: load
-- ANCHOR_END: all
