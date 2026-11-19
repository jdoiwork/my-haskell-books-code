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
  , defineTyped
  , defineData
  , defineAlias
  , evaluate
  , loadProgram
  ) where

import qualified Data.Map as Map
import Data.Text (Text)

import Lam.Builtins
import Lam.Data
import Lam.Eval
import Lam.Float (floatBuiltins)
import Lam.Parser (program, runLamParser)
import Lam.Syntax
import Lam.Types
import qualified Data.Text as T
import Text.Megaparsec (errorBundlePretty)
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
-- 組み込みの関数だけのセッション（プレリュードは、まだ）
emptySession :: Session
emptySession = Session
  { sessionTypes  = Map.fromList
      [ (builtinName p, builtinType p) | p <- allBuiltins ]
  , sessionValues = Map.fromList
      [ (builtinName p, builtinValue p) | p <- allBuiltins ]
  , sessionData   = noData
  }

-- ANCHOR: allBuiltins
-- 🧰 The builtins of every chapter so far.
-- ここまでの章の、組み込みの関数ぜんぶ
allBuiltins :: [Builtin]
allBuiltins = builtins <> floatBuiltins
-- ANCHOR_END: allBuiltins
-- ANCHOR_END: session

-- ANCHOR: define
-- ANCHOR: defineOnly
-- 🏷️ Add one definition: check its type, then compute it.
--    The error says which definition went wrong.
-- 定義を1つ足す：型を確かめ、それから計算する。
-- エラーには、どの定義でまちがえたかを書く
define :: Session -> Name -> Expr -> Either Text (Session, Text)
define session x e = defineWith session x e
  (inferDefinition (sessionData session) (sessionTypes session) x e)

-- 🏷️ The same, with the type that the checking gave (or its error).
-- 同じだが、型を確かめた結果（かエラー）を受け取る
defineWith
  :: Session -> Name -> Expr -> Either TypeError Scheme
  -> Either Text (Session, Text)
defineWith session x e checked =
  case checked of
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
-- ANCHOR_END: defineOnly

-- ANCHOR: defineTyped
-- ✍️ Add a definition with a type annotation above it. The annotation
--    becomes the type, once the definition is checked against it
--    (inferAnnotated).
-- 型の注釈つきの定義を足す。定義が注釈に合うと確かめたら、注釈が
-- そのまま型になる（inferAnnotated）
defineTyped
  :: Session -> Name -> Signature -> Expr -> Either Text (Session, Text)
defineTyped session x sig e = defineWith session x e $ do
  let table = sessionData session
  (declared, names) <- annotationScheme table sig
  inferAnnotated table (sessionTypes session) x names declared e
-- ANCHOR_END: defineTyped

-- ANCHOR: evaluate
-- 🧮 Check the type of an expression, then compute it.
-- 式の型を確かめ、それから計算する
evaluate :: Session -> Expr -> Either Text Text
evaluate session e =
  case inferType (sessionData session) (sessionTypes session) e of
    Left err -> Left ("type error: " <> renderTypeError err)
    Right t  -> case eval (sessionValues session) e of
      Left err -> Left ("error: " <> renderError err)
      -- A number of any type is shown as an Int: 1 + 2 is 3 : Int.
      -- どの型でもよい数は、Int にして見せる
      Right v  -> Right (renderValue v <> " : "
                           <> renderScheme (defaultNumbers t))
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

-- ANCHOR: defineAlias
-- 🏷️ Add a type alias. Only the type checker needs it: no value is
--    made, and the evaluator never hears of it.
-- 型の別名を足す。要るのは型検査器だけ。値は作らず、評価器は知らない
defineAlias :: Session -> AliasDecl -> Either Text (Session, Text)
defineAlias session decl =
  case declareAlias (sessionData session) decl of
    -- 「型の別名 Member で：型のエラー：……」
    Left err -> Left ("in the type alias " <> aliasName decl
                      <> ": type error: " <> renderTypeError err)
    Right table -> Right
      ( session { sessionData = table }
      , "type alias " <> aliasName decl )
-- ANCHOR_END: defineAlias

-- ANCHOR: load
-- 📜 Load a program: every declaration in order, then the last
--    expression (if there is one).
-- プログラムを読みこむ：宣言を順に全部、それから最後の式（あれば）
loadProgram
  :: Session -> FilePath -> Text -> Either Text (Session, Maybe Text)
loadProgram session file source =
  case runLamParser program file source of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right (decls, final) -> do
      session' <- addDecls session decls
      case final of
        Nothing -> Right (session', Nothing)
        Just e  -> do
          out <- evaluate session' e
          Right (session', Just out)

-- ANCHOR: addDecls
-- 📜 Add declarations to a session, in order. A type annotation goes
--    with the definition right below it.
-- 宣言を、順にセッションに足す。型の注釈は、すぐ下の定義と組にする
addDecls :: Session -> [Decl] -> Either Text Session
addDecls s decls = case decls of
  [] -> Right s
  DefSig x sig : DefValue y e : rest
    | x == y -> next (defineTyped s x sig e) rest
  -- 「add の型の注釈のすぐ下に、add の定義が要ります」
  DefSig x _ : _ -> Left ("error: the type annotation of " <> x
    <> " needs the definition of " <> x <> " right below it")
  DefValue x e : rest -> next (define s x e) rest
  DefData d : rest    -> next (defineData s d) rest
  DefAlias a : rest   -> next (defineAlias s a) rest
  where
    next done rest = do
      (s', _) <- done
      addDecls s' rest
-- ANCHOR_END: addDecls
-- ANCHOR_END: load
-- ANCHOR_END: all
