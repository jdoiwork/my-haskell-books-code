-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Definitions and whole programs: shared by the REPL, program
--   files and the prelude.
-- 定義とプログラム全体：REPL、プログラムのファイル、プレリュードで共有する。
module Lam.Program
  ( Session (..)
  , Module (..)
  , emptySession
  , allBuiltins
  , define
  , defineTyped
  , defineData
  , defineAlias
  , explain
  , Ending (..)
  , evaluate
  , startMain
  , placedEval
  , placedType
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Lam.Builtins
import Lam.Data
import Lam.Eval
import Lam.Files (filesBuiltins)
import Lam.Float (floatBuiltins)
import Lam.Concurrent (concurrentBuiltins)
import Lam.Format (formatBuiltins)
import Lam.Io (Concurrent, Handler, Stop (..), ioBuiltins, runAction)
import Lam.Outside (outsideBuiltins)
import Lam.Json (jsonBuiltins)
import Lam.Parsing (parsingBuiltins)
import Lam.Regex (regexBuiltins)
import Lam.Stdlib (stdlibBuiltins)
import Lam.Syntax
import Lam.Types
import qualified Data.Text as T
-- ANCHOR_END: head

-- ANCHOR: session
-- 🗂️ What has been defined so far: types, values, and data types.
-- これまでに定義したもの：型、値、data の型
data Session = Session
  { sessionTypes   :: TypeEnv
  , sessionValues  :: Env
  , sessionData    :: DataTable
  -- ANCHOR: moduleFields
  -- The modules this session knows: the standard ones, and the ones
  -- imported.
  -- このセッションが知っているモジュール：標準のものと、import したもの
  , sessionModules :: Map Name Module
  -- Each imported name (as it is written: Json.get, get), and the
  -- module it came from.
  -- import した名前（書くとおりの形）と、その出どころのモジュール
  , sessionOrigins :: Map Name Name
  -- Each hidden constructor (Json.JDec, JDec), and its module.
  -- 隠された構成子と、そのモジュール
  , sessionHidden  :: Map Name Name
  -- Each imported data type, and the module that declared it.
  -- import した data の型と、それを宣言したモジュール
  , sessionHomes   :: Map Name Name
  -- ANCHOR_END: moduleFields
  -- ANCHOR: sigsField
  -- Each type annotation, as written, with its place. The command line
  -- reads the one of main (its records, and the comments above it).
  -- 書いたとおりの型の注釈と、その場所。コマンドラインは main のもの
  -- （そのレコードと、上のコメント）を読む
  , sessionSigs    :: Map Name (Loc, Signature)
  -- ANCHOR_END: sigsField
  }

-- ANCHOR: module
-- 📦 A module, checked and computed: what other files may use.
-- 型を確かめ、計算し終えたモジュール：ほかのファイルが使ってよいもの
data Module = Module
  { moduleName     :: Name
  , moduleFile     :: FilePath   -- "<Json>" for a standard module
  , moduleStandard :: Bool
    -- The values and the constructors it shows, without the module's
    -- name: get, mkDec, JInt.
    -- 見せる値と構成子。モジュールの名前は付けない
  , moduleExports  :: Map Name (Scheme, Value)
  , moduleCons     :: Map Name Constructor
    -- For each data type it shows: the constructors it shows, and
    -- the ones it hides.  Json: ([JNull, ..., JObj], [JDec])
    -- 見せる data の型ごとに、見せる構成子と、隠す構成子
  , moduleShown    :: Map Name ([Name], [Name])
    -- Its own data types, and the table that knows them.
    -- 自分で宣言した data の型と、それを知っている表
  , moduleTypes    :: [Name]
  , moduleData     :: DataTable
  }
-- ANCHOR_END: module

-- 🧰 A session with only the builtins (no prelude yet).
--    The rows for JSON come from Lam.Json, those for IO from Lam.Io,
--    those for f"..." from Lam.Format, those for regular
--    expressions from Lam.Regex, those for parsers from
--    Lam.Parsing, and those of the toolbox from Lam.Stdlib.
-- 組み込みの関数だけのセッション（プレリュードは、まだ）。
-- JSON の行は Lam.Json から、IO の行は Lam.Io から、
-- f"..." の行は Lam.Format から、正規表現の行は Lam.Regex から、
-- パーサーの行は Lam.Parsing から、道具箱の行は Lam.Stdlib から
emptySession :: Session
emptySession = Session
  { sessionTypes  = Map.fromList
      [ (builtinName p, builtinType p) | p <- allBuiltins ]
  , sessionValues = Map.fromList
      [ (builtinName p, builtinValue p) | p <- allBuiltins ]
  , sessionData    = noData
  , sessionModules = Map.empty
  , sessionOrigins = Map.empty
  , sessionHidden  = Map.empty
  , sessionHomes   = Map.empty
  , sessionSigs    = Map.empty
  }

-- ANCHOR: allBuiltins
-- 🧰 Every builtin, from every table.
-- どの表の組み込みも、全部
allBuiltins :: [Builtin]
allBuiltins =
  builtins <> jsonBuiltins <> ioBuiltins <> formatBuiltins
    <> regexBuiltins <> parsingBuiltins <> outsideBuiltins
    <> stdlibBuiltins <> filesBuiltins <> floatBuiltins
    <> concurrentBuiltins
-- ANCHOR_END: allBuiltins
-- ANCHOR_END: session

-- ANCHOR: define
-- ANCHOR: defineOnly
-- 🏷️ Add one definition: check its type, then compute it.
--    The error says where (first) and which definition went wrong.
-- 定義を1つ足す：型を確かめ、それから計算する。
-- エラーには、場所（先頭に）と、どの定義でまちがえたかを書く
define :: Session -> Name -> Expr -> Either Text (Session, Text)
define session x e = defineWith session { sessionSigs = unsigned } x e
  (inferDefinition (sessionData session) (sessionTypes session) x e)
  where
    -- A new definition without an annotation forgets the old one.
    -- 注釈のない新しい定義は、前の注釈を忘れる
    unsigned = Map.delete x (sessionSigs session)

-- 🏷️ The same, with the type that the checking gave (or its error).
-- 同じだが、型を確かめた結果（かエラー）を受け取る
defineWith
  :: Session -> Name -> Expr -> Either TypeError Scheme
  -> Either Text (Session, Text)
defineWith session x e checked =
  case checked of
    Left err -> Left (inDefinition (placedType (explain session err)))
    Right sc ->
      let env'   = Map.insert x v (sessionValues session)
          result = eval env' e
          v      = either (const (VInt 0)) id result
      in case result of
           Left err -> Left (inDefinition (placedEval err))
           Right _  -> Right
             ( session
                 { sessionTypes  = Map.insert x sc (sessionTypes session)
                 , sessionValues = env' }
             , x <> " : " <> renderScheme sc )
  where
    -- 「場所：x の定義で：……」
    inDefinition (place, msg) =
      place <> "in the definition of " <> x <> ": " <> msg
-- ANCHOR_END: defineOnly

-- ANCHOR: defineTyped
-- ✍️ Add a definition with a type annotation above it. The annotation
--    becomes the type, once the definition is checked against it
--    (inferAnnotated). An error of the annotation itself, with no
--    place of its own, gets the place of the annotation's line.
-- 型の注釈つきの定義を足す。定義が注釈に合うと確かめたら、注釈が
-- そのまま型になる（inferAnnotated）。場所のない注釈のエラーには、
-- 注釈の行の場所を付ける
defineTyped
  :: Session -> Loc -> Name -> Signature -> Expr
  -> Either Text (Session, Text)
defineTyped session loc x sig e = defineWith signed x e $ do
  let table = sessionData session
  (declared, names) <- annotated (annotationScheme table sig)
  annotated
    (inferAnnotated table (sessionTypes session) x names declared e)
  where
    signed = session
      { sessionSigs = Map.insert x (loc, sig) (sessionSigs session) }
    annotated :: Either TypeError a -> Either TypeError a
    annotated r = case r of
      Left err@(TypeAt _ _) -> Left err
      Left err              -> Left (TypeAt loc err)
      Right ok              -> Right ok
-- ANCHOR_END: defineTyped

-- ANCHOR: placed
-- 📍 An error, cut into its place ("file:12:15: ", or nothing) and
--    the rest ("error: divided by zero"). The place goes first, as in
--    GHC, so that an editor can jump there.
-- エラーを、場所（"file:12:15: " か、なし）と残りに分ける。
-- GHC と同じく場所を先頭に置くと、エディターがそこへ飛べる
placedEval :: EvalError -> (Text, Text)
placedEval err = case err of
  Located loc e -> (renderLoc loc <> ": ", snd (placedEval e))
  _             -> ("", "error: " <> renderError err)

placedType :: TypeError -> (Text, Text)
placedType err = case err of
  TypeAt loc e -> (renderLoc loc <> ": ", snd (placedType e))
  _            -> ("", "type error: " <> renderTypeError err)
-- ANCHOR_END: placed

-- ANCHOR: explain
-- 🙈 An unknown name that is a hidden constructor: say so.
--    "unknown name: JDec" becomes "JDec is hidden by the module Json".
-- 知らない名前が、隠された構成子なら、そう言う
explain :: Session -> TypeError -> TypeError
explain session err = case err of
  TypeAt loc e -> TypeAt loc (explain session e)
  UnknownName x        | Just m <- hiddenIn x -> Hidden x m
  UnknownConstructor c | Just m <- hiddenIn c -> Hidden c m
  _ -> err
  where
    hiddenIn x = Map.lookup x (sessionHidden session)
-- ANCHOR_END: explain

-- ANCHOR: ending
-- 🏁 How a program (or one expression) ended.
-- プログラム（か、式1つ）の終わり方
data Ending
  = Finished Text         -- to the end: the text to show
  | Errored Text          -- an error: the message (exit code 1)
  | ExitedWith Int        -- exit n
  deriving (Eq, Show)

-- The place and the rest, put together again.
-- 場所と残りを、もう一度つなぐ
joined :: (Text, Text) -> Text
joined (place, msg) = place <> msg
-- ANCHOR_END: ending

-- ANCHOR: evaluate
-- 🧮 Check the type of an expression, then compute it. When the value
--    is IO, the handler does it, like GHCi: show what it gives, but
--    show nothing for ().
-- 式の型を確かめ、それから計算する。値が IO なら、GHCi と同じく、
-- ハンドラーでやってみせる。くれた値を見せるが、() なら何も見せない
evaluate :: Concurrent m => Handler m -> Session -> Expr -> m Ending
evaluate handle session e =
  case inferType (sessionData session) (sessionTypes session) e of
    Left err -> pure (Errored (joined (placedType (explain session err))))
    Right t  -> case eval (sessionValues session) e of
      Left err -> pure (Errored (joined (placedEval err)))
      Right (VIO action) -> ran t <$> runAction handle action
      Right v -> pure (Finished (renderValue v <> " : " <> shown t))
  where
    -- What the IO action gave, as the REPL shows it.
    -- IO がくれたものを、REPL が見せる形で
    ran t done = case done of
      Left (Failed err) -> Errored (joined (placedEval err))
      Left (Exited n)   -> ExitedWith n
      Right (VTuple []) -> Finished ""
      Right v           -> Finished (renderValue v <> " : " <> inside t)
    -- A number of any type is shown as an Int: 1 + 2 is 3 : Int.
    -- どの型でもよい数は、Int にして見せる
    shown = renderScheme . defaultNumbers
    -- The type of what the action gives: String for IO String.
    -- 頼みごとがくれる値の型。IO String なら String
    inside sc = case defaultNumbers sc of
     Forall vs ns ty -> case ty of
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
defineData session decl
  -- ANCHOR: foreignType
  -- An imported type cannot be declared again here.
  -- import した型は、ここで宣言し直せない
  | Just m <- Map.lookup (dataName decl) (sessionHomes session) =
      Left (inData (renderTypeError (ForeignType (dataName decl) m)))
  -- ANCHOR_END: foreignType
  | otherwise = case declareData (sessionData session) decl of
      Left err -> Left (inData (renderTypeError err))
      Right (table, cons) -> Right
        ( session
            { sessionTypes  =
                Map.fromList cons `Map.union` sessionTypes session
            , sessionValues =
                Map.fromList values `Map.union` sessionValues session
            , sessionData   = table }
        , T.intercalate "\n"
            [ c <> " : " <> renderScheme sc | (c, sc) <- cons ] )
  where
    inData msg =
      "in the data type " <> dataName decl <> ": type error: " <> msg
    values =
      [ (c, constructorValue c i (length args))
      | (i, (c, args)) <- zip [0 ..] (dataCons decl) ]

-- ANCHOR: defineAlias
-- 🏷️ Add a type alias. Only the type checker needs it: no value is
--    made, and the evaluator never hears of it.
-- 型の別名を足す。要るのは型検査器だけ。値は作らず、評価器は知らない
defineAlias :: Session -> AliasDecl -> Either Text (Session, Text)
defineAlias session decl =
  case declareAlias (sessionData session) decl of
    -- 「型の別名 User で：型のエラー：……」
    Left err -> Left ("in the type alias " <> aliasName decl
                      <> ": type error: " <> renderTypeError err)
    Right table -> Right
      ( session { sessionData = table }
      , "type alias " <> aliasName decl )
-- ANCHOR_END: defineAlias

-- 🧱 Nothing is a value. Just waits for 1 argument, like a builtin.
-- Nothing は値。Just は、組み込みの関数と同じく、引数を1つ待つ
constructorValue :: Name -> Int -> Int -> Value
constructorValue c i arity
  | arity == 0 = VCon c i []
  | otherwise  = VPrim c arity (Right . VCon c i) []
-- ANCHOR_END: defineData

-- ANCHOR: startMain
-- 🚪 The expression that starts main. The arguments and the
--    environment are only sugar: they come from getArgs and
--    getEnvironment, like the lines of do.
--      main : [String] -> IO ()   starts as   bind getArgs main
-- main を始める式。引数と環境変数は糖衣構文：do の行と同じく、
-- getArgs と getEnvironment からもらう
startMain :: MainShape -> Expr
startMain shape = case shape of
  MainPlain   -> Var "main"
  MainArgs    -> call "bind" [Var "getArgs", Var "main"]
  MainArgsEnv -> call "bind" [Var "getArgs", Lam "$args"
                   (call "bind" [ Var "getEnvironment"
                                , App (Var "main") (Var "$args") ])]
  -- ANCHOR: startCli
  -- The options (or a subcommand) are read by $cli, which the runner
  -- makes from the type of main before it starts (Lam.Cli).
  -- オプション（かサブコマンド）は $cli が読む。$cli は、実行役が
  -- 始める前に main の型から作る（Lam.Cli）
  MainOptions  -> call "bind" [Var "getArgs", Var "$cli"]
  MainCommands -> call "bind" [Var "getArgs", Var "$cli"]
  -- ANCHOR_END: startCli
  where
    call f = foldl App (Var f)
-- ANCHOR_END: startMain
-- ANCHOR_END: all
