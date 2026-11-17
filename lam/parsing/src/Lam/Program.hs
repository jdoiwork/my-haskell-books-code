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
  , runProgram
  ) where

import qualified Data.Map as Map
import Data.Text (Text)

import Lam.Builtins
import Lam.Data
import Lam.Eval
import Lam.Format (formatBuiltins)
import Lam.Io (Handler, ioBuiltins, runAction)
import Lam.Json (jsonBuiltins)
import Lam.Parser (program, runLamParser)
import Lam.Parsing (parsingBuiltins)
import Lam.Regex (regexBuiltins)
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
--    The rows for JSON come from Lam.Json, those for IO from Lam.Io,
--    those for f"..." from Lam.Format, those for regular
--    expressions from Lam.Regex, and those for parsers from
--    Lam.Parsing.
-- 組み込みの関数だけのセッション（プレリュードは、まだ）。
-- JSON の行は Lam.Json から、IO の行は Lam.Io から、
-- f"..." の行は Lam.Format から、正規表現の行は Lam.Regex から、
-- パーサーの行は Lam.Parsing から
emptySession :: Session
emptySession = Session
  { sessionTypes  = Map.fromList
      [ (builtinName p, builtinType p) | p <- allBuiltins ]
  , sessionValues = Map.fromList
      [ (builtinName p, builtinValue p) | p <- allBuiltins ]
  , sessionData   = noData
  }
  where
    allBuiltins =
      builtins <> jsonBuiltins <> ioBuiltins <> formatBuiltins
        <> regexBuiltins <> parsingBuiltins
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
-- 🧮 Check the type of an expression, then compute it. When the value
--    is IO, the handler does it, like GHCi: show what it gives, but
--    show nothing for ().
-- 式の型を確かめ、それから計算する。値が IO なら、GHCi と同じく、
-- ハンドラーでやってみせる。くれた値を見せるが、() なら何も見せない
evaluate :: Monad m => Handler m -> Session -> Expr -> m (Either Text Text)
evaluate handle session e =
  case inferType (sessionData session) (sessionTypes session) e of
    Left err -> pure (Left ("type error: " <> renderTypeError err))
    Right t  -> case eval (sessionValues session) e of
      Left err -> pure (Left ("error: " <> renderError err))
      Right (VIO action) -> do
        done <- runAction handle action
        pure $ case done of
          Left err          -> Left ("error: " <> renderError err)
          Right (VTuple []) -> Right ""
          Right v           -> Right (renderValue v <> " : " <> inside t)
      Right v -> pure (Right (renderValue v <> " : " <> renderScheme t))
  where
    -- The type of what the action gives: String for IO String.
    -- 頼みごとがくれる値の型。IO String なら String
    inside (Forall vs ns ty) = case ty of
      TCon "IO" [r] -> renderScheme (Forall vs ns r)
      _             -> renderScheme (Forall vs ns ty)
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
-- 📜 Load a program: every declaration in order. The last expression
--    (if there is one) is given back, not computed yet: it may be IO.
-- プログラムを読みこむ：宣言を順に全部。最後の式（あれば）は、
-- 計算せずに返す（IO かもしれないので）
loadProgram
  :: Session -> FilePath -> Text -> Either Text (Session, Maybe Expr)
loadProgram session file source =
  case runLamParser program file source of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right (decls, final) -> do
      session' <- foldl addOne (Right session) decls
      Right (session', final)
  where
    addOne acc decl = do
      s <- acc
      fst <$> case decl of
        DefValue x e -> define s x e
        DefData d    -> defineData s d
-- ANCHOR_END: load

-- ANCHOR: runProgram
-- 🚪 Run a program file: load it, show the last expression (if there
--    is one), then do main (if there is one). main must be IO ().
--    The answer is the text to show, or an error.
-- プログラムのファイルを動かす：読みこみ、最後の式（あれば）を見せ、
-- それから main（あれば）をやる。main は IO () でなければならない
runProgram
  :: Monad m
  => Handler m -> Session -> FilePath -> Text -> m (Either Text Text)
runProgram handle session file source =
  case loadProgram session file source of
    Left err -> pure (Left err)
    Right (s, final) -> do
      shown <- maybe (pure (Right "")) (evaluate handle s) final
      case (shown, Map.lookup "main" (sessionTypes s)) of
        (Left err, _)     -> pure (Left err)
        (Right out, Nothing) -> pure (Right out)
        (Right out, Just sc) -> case checkMain sc of
          Left err -> pure (Left ("in main: type error: "
                                    <> renderTypeError err))
          Right () -> fmap (out <>) <$> evaluate handle s (Var "main")
-- ANCHOR_END: runProgram
-- ANCHOR_END: all
