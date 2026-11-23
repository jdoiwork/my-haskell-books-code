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
  , explain
  , define
  , defineTyped
  , defineData
  , defineAlias
  , defineClass
  , defineInstance
  , defineInfix
  , Ending (..)
  , placedEval
  , placedType
  , evaluate
  , MainShape (..)
  , mainShape
  , startMain
  , mainBind
  , mainArgs
  , mainEnv
  , mainCli
  , runMain
  , printText
  , addDecls
  , keyWarnings
  , children
  , Def (..)
  , pairSignatures
  , groupsOf
  ) where

import Control.Monad (foldM, forM)
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.List (nub, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Lam.Builtins
import Lam.Class
import Lam.Data
import Lam.Eval
import Lam.Float (floatBuiltins)
import Lam.Format (formatBuiltins)
import Lam.Io (Concurrent, Handler, Stop (..), ioBuiltins, ioOf, runAction, stopBuiltins)
import Lam.Json (jsonBuiltins)
import Lam.Files (filesBuiltins)
import Lam.Bytes (bytesBuiltins)
import Lam.Regex (regexBuiltins)
import Lam.Shell (shellBuiltins)
import Lam.Concurrent (concurrentBuiltins)
import Lam.Channels (actorBuiltins, channelBuiltins)
import Lam.Parsing (parsingBuiltins)
import Lam.Decode (decodeBuiltins)
import Lam.Outside (outsideBuiltins)
import Lam.Server (serverBuiltins)
import Lam.Stdlib (stdlibBuiltins)
import Lam.Toolbox (toolboxBuiltins)
import Lam.Pretty (renderTypeExpr)
import Lam.Syntax
import Lam.Types
import qualified Data.Text as T
-- ANCHOR_END: head

-- ANCHOR: session
-- 🗂️ What has been defined so far: types, values, and data types.
-- これまでに定義したもの：型、値、data の型
data Session = Session
  { sessionTypes  :: TypeEnv
  , sessionValues :: Env
  , sessionData   :: DataTable
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
  -- Each imported type, type alias and class, and the module that
  -- declared it.
  -- import した型・型の別名・クラスと、それを宣言したモジュール
  , sessionHomes   :: Map Name Name
  -- Each instance it knows (Display, Json), and the module that
  -- declared it.
  -- 知っているインスタンス（Display と Json）と、それを宣言したモジュール
  , sessionInstanceHomes :: Map (Name, Name) Name
  -- ANCHOR_END: moduleFields
  -- ANCHOR: operatorsField
  -- The operators it knows: its own (infixl ...), and the imported
  -- ones. The parser reads the file with them.
  -- 知っている演算子：自分のもの（infixl ...）と、import したもの。
  -- パーサーは、これでファイルを読む
  , sessionOperators :: Operators
  -- ANCHOR_END: operatorsField
  -- ANCHOR: sigsField
  -- Each type annotation, as written. The command line reads the one
  -- of main (its records, with their help and defaults).
  -- 書いたとおりの型の注釈。コマンドラインは main のもの（そのレコードと、
  -- ヘルプと既定値）を読む
  , sessionSigs    :: Map Name Signature
  -- The file of the program and its text (Nothing in the REPL): the
  -- command line takes its name, and the help above main.
  -- プログラムのファイルと、その文字（REPL では Nothing）。コマンド
  -- ラインは、その名前と、main の上のヘルプを取る
  , sessionProgram :: Maybe (FilePath, Text)
  -- ANCHOR_END: sigsField
  -- ANCHOR: warningsField
  -- The names the program (or the REPL) defined itself, not the
  -- prelude's: a key of a Map that looks like one of them gets a
  -- warning. And the warnings found while loading, to tell before the
  -- program runs.
  -- プログラム（か REPL）が自分で定義した名前（プレリュードのものでは
  -- ない）：そのどれかに見える Map のキーには警告を出す。それと、読み
  -- こむあいだに見つけた警告。プログラムを動かす前に知らせる
  , sessionOwn      :: Set.Set Name
  , sessionWarnings :: [Text]
  -- ANCHOR_END: warningsField
  }

-- ANCHOR: module
-- 📦 A module, checked and computed: what other files may use.
-- 型を確かめ、計算し終えたモジュール：ほかのファイルが使ってよいもの
data Module = Module
  { moduleName     :: Name
  , moduleFile     :: FilePath   -- "<Json>" for a standard module
  , moduleStandard :: Bool
    -- The values, constructors and methods it shows, without the
    -- module's name: get, mkDec, JInt, describe.
    -- 見せる値・構成子・メソッド。モジュールの名前は付けない
  , moduleExports  :: Map Name (Scheme, Value)
  , moduleCons     :: Map Name Constructor
    -- For each type (or class) it shows: the constructors (or
    -- methods) it shows, and the ones it hides.
    --   Json: ([JNull, ..., JObj], [JDec])
    -- 見せる型（クラス）ごとに、見せる構成子（メソッド）と、隠すもの
  , moduleShown    :: Map Name ([Name], [Name])
    -- Its own types, type aliases and classes, and the table that
    -- knows them (with every instance it knows).
    -- 自分で宣言した型・型の別名・クラスと、それを知っている表
    -- （知っているインスタンスも全部入っている）
  , moduleTypes    :: [Name]
  , moduleData     :: DataTable
    -- The instances it knows, with their homes, and their values
    -- (the dictionaries: names a program cannot write, like
    -- $Display$Money). They go wherever the module goes.
    -- 知っているインスタンスと、その出どころ。その値（辞書。
    -- $Display$Money のような、プログラムには書けない名前）。
    -- モジュールと一緒に、どこへでも行く
  , moduleInstances :: Map (Name, Name) Name
  , moduleDicts     :: Env
    -- ANCHOR: moduleOperators
    -- The operators it shows, with the names inside the module:
    --   <+>  is  infixl 7 addPrice
    -- 見せる演算子。呼ぶ関数は、モジュールの中の名前のまま
  , moduleOperators :: Operators
    -- ANCHOR_END: moduleOperators
    -- The warnings found while loading its file.
    -- そのファイルを読みこむあいだに見つけた警告
  , moduleWarnings :: [Text]
  }
-- ANCHOR_END: module

-- 🧰 A session with only the builtins (no prelude yet).
-- 組み込みの関数だけのセッション（プレリュードは、まだ）
emptySession :: Session
emptySession = Session
  { sessionTypes  = Map.fromList
      [ (builtinName p, builtinType p) | p <- allBuiltins ]
  , sessionValues = Map.fromList
      [ (builtinName p, builtinValue p) | p <- allBuiltins ]
  , sessionData   = noData
  , sessionModules = Map.empty
  , sessionOrigins = Map.empty
  , sessionHidden  = Map.empty
  , sessionHomes   = Map.empty
  , sessionInstanceHomes = Map.empty
  , sessionOperators = Map.empty
  , sessionSigs    = Map.empty
  , sessionProgram = Nothing
  , sessionOwn      = Set.empty
  , sessionWarnings = []
  }

-- ANCHOR: allBuiltins
-- 🧰 The builtins of every chapter so far.
-- ここまでの章の、組み込みの関数ぜんぶ
allBuiltins :: [Builtin]
allBuiltins =
  builtins <> floatBuiltins <> jsonBuiltins <> stdlibBuiltins
    <> formatBuiltins <> ioBuiltins <> stopBuiltins <> filesBuiltins
    <> bytesBuiltins <> regexBuiltins <> parsingBuiltins
    <> decodeBuiltins <> outsideBuiltins <> shellBuiltins
    <> concurrentBuiltins
    <> channelBuiltins
    <> actorBuiltins
    <> serverBuiltins
    <> toolboxBuiltins
-- ANCHOR_END: allBuiltins
-- ANCHOR_END: session

-- ANCHOR: define
-- ANCHOR: defineOnly
-- 🏷️ Add one definition: check its type, then compute it.
--    The error says which definition went wrong.
-- 定義を1つ足す：型を確かめ、それから計算する。
-- エラーには、どの定義でまちがえたかを書く
define :: Session -> Name -> Expr -> Either Text (Session, Text)
define session x e = defineWith unsigned x e
  (inferDefinition (sessionData session) (sessionTypes session) x e)
  where
    -- A new definition without an annotation forgets the old one.
    -- 注釈のない新しい定義は、前の注釈を忘れる
    unsigned = session { sessionSigs = Map.delete x (sessionSigs session) }

-- 🏷️ The same, with the type that the checking gave (or its error).
--    The checking also gives the expression to compute: the same one,
--    with the dictionaries of the declared classes put in.
-- 同じだが、型を確かめた結果（かエラー）を受け取る。確かめた結果には、
-- 計算する式も入っている（宣言したクラスの辞書を入れた式）
defineWith
  :: Session -> Name -> Expr -> Either TypeError (Scheme, Expr)
  -> Either Text (Session, Text)
defineWith session x _ checked =
  case checked of
    Left err -> Left (inDefinition (placedType (explain session err)))
    Right (sc, e) ->
      let env'   = Map.insert x v (sessionValues session)
          result = eval env' e
          v      = either (const (VInt 0)) id result
      in case result of
           Left err -> Left (inDefinition (placedEval err))
           Right _  -> Right
             ( session
                 { sessionTypes  = Map.insert x sc (sessionTypes session)
                 , sessionValues = env' }
             , x <> " : " <> renderUsual sc )
  where
    -- 「場所：x の定義で：……」
    inDefinition (place, msg) =
      place <> "in the definition of " <> x <> ": " <> msg
-- ANCHOR_END: defineOnly

-- ANCHOR: placed
-- 📍 An error, cut into its place ("shop.lam:12:15: ", or nothing) and
--    the rest ("error: divided by zero"). The place goes first, as in
--    GHC, so that an editor can jump there.
-- エラーを、場所（"shop.lam:12:15: " か、なし）と残りに分ける。
-- GHC と同じく場所を先頭に置くと、エディターがそこへ飛べる
placedEval :: EvalError -> (Text, Text)
placedEval err = case placeOf err of
  (Just loc, e) -> (renderLoc loc <> ": ", "error: " <> renderError e)
  (Nothing, e)  -> ("", "error: " <> renderError e)

placedType :: TypeError -> (Text, Text)
placedType err = case err of
  TypeAt loc e -> (renderLoc loc <> ": ", snd (placedType e))
  _            -> ("", "type error: " <> renderTypeError err)

-- The place and the rest, put together again.
-- 場所と残りを、もう一度つなぐ
joined :: (Text, Text) -> Text
joined (place, msg) = place <> msg
-- ANCHOR_END: placed

-- ANCHOR: ending
-- 🏁 How a program (or one expression) ended.
-- プログラム（か、式1つ）の終わり方
data Ending
  = Finished Text         -- to the end: the text to show
  | Errored Text          -- an error: the message (exit code 1)
  | ExitedWith Int        -- exit n
  deriving (Eq, Show)

-- 🛑 How the runner stopped, as an Ending.
-- 実行役の止まり方を、終わり方に
stopped :: Stop -> Ending
stopped stop = case stop of
  Failed err -> Errored (joined (placedEval err))
  Exited n   -> ExitedWith n
-- ANCHOR_END: ending

-- ANCHOR: defineTyped
-- ✍️ Add a definition with a type annotation above it. The annotation
--    becomes the type, once the definition is checked against it
--    (inferAnnotated).
-- 型の注釈つきの定義を足す。定義が注釈に合うと確かめたら、注釈が
-- そのまま型になる（inferAnnotated）
defineTyped
  :: Session -> Name -> Signature -> Expr -> Either Text (Session, Text)
defineTyped session x sig e = defineWith signed x e $ do
  let table = sessionData session
  (declared, names) <- annotationScheme table sig
  inferAnnotated table (sessionTypes session) x names declared e
  where
    signed = session { sessionSigs = Map.insert x sig (sessionSigs session) }
-- ANCHOR_END: defineTyped

-- ANCHOR: evaluate
-- 🧮 Check the type of an expression, then compute it. When the value
--    is IO, the handler does it, like GHCi: show what it gives, but
--    show nothing for ().
-- 式の型を確かめ、それから計算する。値が IO なら、GHCi と同じく、
-- ハンドラーでやってみせる。くれた値を見せるが、() なら何も見せない
evaluate :: Concurrent m => Handler m -> Session -> Expr -> m Ending
evaluate handle session e =
  case typed of
    Left err -> pure (Errored (joined (placedType (explain session err))))
    -- ANCHOR: evaluateOpen
    -- A dictionary still needed: total [] has no type to choose with.
    -- まだ辞書が要る：total [] には、選ぶための型がない
    Right (Forall _ ns t, _)
      | (_, c) : _ <- dictNeeds ns ->
          pure (Errored ("type error: "
                         <> renderTypeError (AmbiguousClass c t)))
    -- ANCHOR_END: evaluateOpen
    Right (sc, e') -> case eval (sessionValues session) e' of
      Left err -> pure (Errored (joined (placedEval err)))
      Right (VIO action) -> ran sc <$> runAction handle action
      -- A number of any type is shown as an Int: 1 + 2 is 3 : Int.
      -- どの型でもよい数は、Int にして見せる
      Right v -> pure (Finished (renderValue v <> " : "
                                   <> renderScheme (defaultNumbers sc)))
  where
    -- What the IO action gave, as the REPL shows it.
    -- IO がくれたものを、REPL が見せる形で
    ran sc done = case done of
      Left stop         -> stopped stop
      Right (VTuple []) -> Finished ""
      Right v           -> Finished (renderValue v <> " : " <> inside sc)
    table = sessionData session
    types = sessionTypes session
    -- ANCHOR: defaultIO
    -- pure 1 could be any Applicative (or monad): then it is an IO,
    -- as in GHCi.
    -- pure 1 はどの Applicative（やモナド）でもありうる。そのときは、
    -- GHCi と同じく IO
    typed = case inferType table types e of
      Right (Forall _ ns (TApp (TVar m) _), _)
        | any (`elem` Map.findWithDefault mempty m ns)
              [Declared "Applicative", Declared "Monad"] ->
            inferIO table types Nothing e
      other -> other
    -- ANCHOR_END: defaultIO
    -- The type of what the action gives: String for IO String.
    -- 頼みごとがくれる値の型。IO String なら String
    inside sc = case defaultNumbers sc of
      Forall vs ns (TCon "IO" [r]) -> renderScheme (Forall vs ns r)
      other                        -> renderScheme other
-- ANCHOR_END: evaluate

-- ANCHOR: runMain
-- ANCHOR: mainShape
-- 🚪 main, the way into a program, has one of five shapes:
--      main : IO ()
--      main : [String] -> IO ()                        (the arguments)
--      main : [String] -> Map String String -> IO ()   (and the
--                                                       environment)
--      main : { name : String } -> IO ()       (options, from the type)
--      main : { add : { … } -> IO (), list : IO () }      (subcommands)
--    and each may give IO (Result e ()) in place of IO (). A record
--    decides the last two; otherwise the number of arrows of its type
--    decides the shape.
-- プログラムの入り口 main は、5つの形のどれか。どれも IO () の代わりに
-- IO (Result e ()) でもよい。レコードなら最後の2つ。そうでなければ、
-- 型の矢印の数で、形を決める
data MainShape
  = MainPlain | MainArgs | MainArgsEnv
  -- ANCHOR: mainShapesCli
  | MainOptions      -- { name : String } -> IO ()
  | MainCommands     -- { add : … -> IO (), list : IO () }
  -- ANCHOR_END: mainShapesCli
  deriving (Eq, Show)

mainShape :: Scheme -> MainShape
mainShape (Forall _ _ t) = case t of
  -- ANCHOR: mainShapeCli
  TFun (TRecord _ _) _ -> MainOptions
  TRecord _ _          -> MainCommands
  -- ANCHOR_END: mainShapeCli
  _ -> case arrows t of
    0 -> MainPlain
    1 -> MainArgs
    _ -> MainArgsEnv
  where
    arrows ty = case ty of
      TFun _ r -> 1 + arrows r :: Int
      _        -> 0
-- ANCHOR_END: mainShape

-- ANCHOR: startMain
-- 🚪 The expression that starts main. The arguments and the
--    environment are only sugar: they come from getArgs and
--    getEnvironment, as the lines of a do would get them.
--      main : [String] -> IO ()   starts as   bind getArgs main
--    The names are hidden ones ($main.args is IO.getArgs), so a
--    program of its own getArgs does not change them.
-- main を始める式。引数と環境変数は糖衣構文：do の行と同じく、
-- getArgs と getEnvironment からもらう。名前は隠した名前（$main.args は
-- IO.getArgs）なので、プログラムが自分の getArgs を作っても変わらない
startMain :: MainShape -> Expr
startMain shape = case shape of
  MainPlain   -> Var "main"
  MainArgs    -> call mainBind [Var mainArgs, Var "main"]
  MainArgsEnv -> call mainBind [Var mainArgs, Lam "$args"
                   (call mainBind [ Var mainEnv
                                  , App (Var "main") (Var "$args") ])]
  -- ANCHOR: startCli
  -- The words are read by $main.cli, which the runner makes from the
  -- type of main before it starts (Lam.Cli). It takes main, and the
  -- words, and calls main with its record (or the subcommand).
  -- 語は $main.cli が読む。$main.cli は、実行役が始める前に main の型
  -- から作る（Lam.Cli）。main と語を受け取り、レコードを渡して main
  -- （かサブコマンド）を呼ぶ
  MainOptions  -> call mainBind [Var mainArgs, App (Var mainCli) (Var "main")]
  MainCommands -> call mainBind [Var mainArgs, App (Var mainCli) (Var "main")]
  -- ANCHOR_END: startCli
  where
    call f = foldl App (Var f)

-- 🙈 The hidden names that startMain uses (Lam.Prelude puts them in,
--    but Lam.Cli puts in $main.cli).
-- startMain が使う隠した名前（Lam.Prelude が入れる。$main.cli だけは
-- Lam.Cli が入れる）
mainBind, mainArgs, mainEnv, mainCli :: Name
mainBind = "$main.bind"
mainArgs = "$main.args"
mainEnv  = "$main.env"
mainCli  = "$main.cli"
-- ANCHOR_END: startMain

-- 🚪 Do main. Its type must fit its shape, with IO () or
--    IO (Result e ()) at the end: then an Err stops the program, with
--    its words (as Rust's  fn main() -> Result<(), E>).
--    How it ended: to the end, an error, or exit n.
-- main をやる。型は形に合い、最後は IO () か IO (Result e ()) で
-- なければならない。後者なら、Err はその文でプログラムを止める（Rust の
-- fn main() -> Result<(), E> と同じ）。答えは終わり方：最後まで、
-- エラー、または exit n
runMain :: Concurrent m => Handler m -> Session -> m Ending
runMain handle session =
  case (checked (Just tUnit), checked Nothing) of
    (Right (_, e), _) -> run e
    (Left _, Right (Forall _ ns (TCon "IO" [TCon "Result" [_, u]]), e))
      | unitOrAny ns u -> run e
    -- 「main で：型のエラー：……」（IO () として確かめたときのもの）
    (Left err, _) -> pure (Errored ("in main: type error: "
                                      <> renderTypeError (badMain err)))
  where
    shape = maybe MainPlain mainShape
              (Map.lookup "main" (sessionTypes session))
    checked answer =
      inferIO (sessionData session) (sessionTypes session) answer
              (startMain shape)
    -- With arguments, the error of the hidden bind would only puzzle:
    -- tell which shapes main may have.
    -- 引数があると、隠した bind のエラーはわかりにくいだけなので、
    -- main に許される形を教える
    badMain err = case (shape, Map.lookup "main" (sessionTypes session)) of
      (MainPlain, _)  -> explain session err
      (_, Just sc)    -> BadMain (schemeType sc)
      (_, Nothing)    -> explain session err
    schemeType (Forall _ _ t) = t
    run e = case eval (sessionValues session) e >>= ioOf of
      Left err     -> pure (Errored (joined (placedEval err)))
      Right action -> do
        done <- runAction handle action
        pure $ case done of
          Left stop                     -> stopped stop
          Right (VCon "Err" _ [reason]) -> Errored ("error: " <> said reason)
          Right _                       -> Finished ""
    -- (), or anything at all (pure (Err 404) never says what an Ok
    -- holds). A number is not anything at all: pure (Ok 3) is wrong.
    -- ()、または何でもよい（pure (Err 404) は Ok の中身を言わない）。
    -- 数は「何でもよい」ではない。pure (Ok 3) はまちがい
    unitOrAny ns u = case u of
      TVar v -> not (Map.member v ns)
      _      -> u == tUnit
    -- A string as it is (no quotes); anything else as the REPL shows it.
    -- 文字列はそのまま（" なし）。ほかは REPL の見せ方で
    said v = case v of
      VString t -> t
      _         -> renderValue v
-- ANCHOR_END: runMain

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

-- ANCHOR: printText
-- 🖨️ :print e  writes a String as it is, and a [String] one item a
--    line: no quotes and no \n, so that lined-up text can be seen.
--    (A program writes with putStrLn; :print is a quick look.)
-- :print e は、String をそのまま、[String] を1行に1つずつ書く。引用符も
-- \n もないので、そろえた文字を目で見られる（プログラムは putStrLn で
-- 書く。:print は、さっと見るためのもの）
printText :: Session -> Expr -> Either Text Text
printText session e = do
  (sc, t) <- typeOf e
  text <- case t of
    TCon "String" [] -> Right e
    TCon "List" [TCon "String" []] ->
      Right (App (App (Var "join") (Lit (LString "\n"))) e)
    -- 「:print は String か [String] を書きます」
    _ -> Left ("type error: :print writes a String or a [String], not "
               <> renderScheme (defaultNumbers sc))
  case inferType (sessionData session) (sessionTypes session) text of
    Left err -> Left ("type error: " <> renderTypeError err)
    Right (_, e') -> case eval (sessionValues session) e' of
      Left err          -> Left ("error: " <> renderError err)
      Right (VString s) -> Right s
      Right v           -> Right (renderValue v)
  where
    typeOf x =
      case inferType (sessionData session) (sessionTypes session) x of
        Left err -> Left ("type error: " <> renderTypeError (explain session err))
        Right (sc@(Forall _ _ t), _) -> Right (sc, t)
-- ANCHOR_END: printText
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
      Left ("in the data type " <> dataName decl <> ": type error: "
            <> renderTypeError (ForeignType "type" (dataName decl) m))
  -- ANCHOR_END: foreignType
  | otherwise =
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

-- ANCHOR: defineClass
-- 🎓 Add a class. Its methods go into the environment: their types,
--    and their values (each one picks the method from a dictionary).
--    The answer shows the type of each method.
-- クラスを足す。メソッドが環境に入る：型と、値（辞書からメソッドを
-- 取り出す）。答えには、メソッドごとの型を見せる
defineClass :: Session -> ClassDecl -> Either Text (Session, Text)
defineClass = defineClassPart Whole

-- 🎓 A part of a class (see Part), with its values computed.
-- クラスの一部（Part を見る）。値も計算する
defineClassPart
  :: Part -> Session -> ClassDecl -> Either Text (Session, Text)
defineClassPart part session decl = do
  (s, values) <- classParts part session decl
  env <- computeAll (sessionValues s) values
  let info  = Map.lookup (classDeclName decl) (classes (sessionData s))
      types = [ (m, sc) | (m, sc, _) <- maybe [] classMethodsOf info ]
  Right ( s { sessionValues = env }
        , T.intercalate "\n"
            [ m <> " : " <> renderScheme sc | (m, sc) <- types ] )

-- 🎓 A part of a class: the session with its types, and the values to
--    compute.
-- クラスの一部：型を入れたセッションと、計算する値
classParts
  :: Part -> Session -> ClassDecl -> Either Text (Session, [(Name, Expr)])
classParts part session decl
  | Just m <- Map.lookup (classDeclName decl) (sessionHomes session) =
      Left ("in the class " <> classDeclName decl <> ": type error: "
            <> renderTypeError (ForeignType "class" (classDeclName decl) m))
  | otherwise =
  case declareClassPart part (sessionData session) (sessionTypes session)
         decl of
    -- 「クラス Describe で：型のエラー：……」
    Left err -> Left ("in the class " <> classDeclName decl
                      <> ": type error: " <> renderTypeError err)
    Right (table, types, values) -> Right
      ( session
          { sessionTypes = Map.fromList types
                             `Map.union` sessionTypes session
          , sessionData  = table }
      , values )

-- 🧩 Add an instance: its methods and its dictionary are values (with
--    names a program cannot write). Nothing new gets a type.
-- インスタンスを足す。メソッドと辞書は値になる（プログラムからは
-- 書けない名前）。型の付く新しい名前は、ない
defineInstance :: Session -> InstanceDecl -> Either Text (Session, Text)
defineInstance = defineInstancePart Whole

-- 🧩 A part of an instance (see Part), with its values computed.
-- インスタンスの一部（Part を見る）。値も計算する
defineInstancePart
  :: Part -> Session -> InstanceDecl -> Either Text (Session, Text)
defineInstancePart part session decl = do
  (s, values) <- instanceParts part session decl
  env <- computeAll (sessionValues s) values
  Right (s { sessionValues = env }, instanceLabel decl)

-- 🧩 A part of an instance: the session with its table, and the values
--    to compute.
-- インスタンスの一部：表を入れたセッションと、計算する値
instanceParts
  :: Part -> Session -> InstanceDecl -> Either Text (Session, [(Name, Expr)])
instanceParts part session decl =
  case declareInstancePart part (sessionData session)
         (sessionTypes session) decl of
    -- 「インスタンス Describe Shape で：型のエラー：……」
    Left err -> Left ("in the instance " <> instClass decl <> " "
                      <> renderTypeExpr 2 (instType decl) <> ": type error: "
                      <> renderTypeError err)
    Right (table, values, _) ->
      Right (session { sessionData = table }, values)

-- 🏷️ What the REPL shows for an instance:  instance Describe Shape
-- REPL がインスタンスに見せるもの
instanceLabel :: InstanceDecl -> Text
instanceLabel (InstanceDecl context c te _) =
  "instance " <> contextText <> c <> " " <> renderTypeExpr 2 te
  where
    contextText = case context of
      []  -> ""
      [k] -> cond k <> " => "
      ks  -> "(" <> T.intercalate ", " (map cond ks) <> ") => "
    cond (k, b) = k <> " " <> b

-- ⚙️ Compute some values together: each may use the others (the
--    dictionary uses the methods, and the methods use the dictionary).
-- いくつかの値を一緒に計算する。どれも、ほかを使ってよい（辞書は
-- メソッドを、メソッドは辞書を使う）
computeAll :: Env -> [(Name, Expr)] -> Either Text Env
computeAll env defs =
  let env'    = Map.union
                  (Map.fromList [ (x, valueOf r) | (x, r) <- results ]) env
      results = [ (x, eval env' e) | (x, e) <- defs ]
      valueOf = either (const (VInt 0)) id
  in case [ err | (_, Left err) <- results ] of
       err : _ -> Left ("error: " <> renderError err)
       []      -> Right env'
-- ANCHOR_END: defineClass

-- ANCHOR: defineAlias
-- 🏷️ Add a type alias. Only the type checker needs it: no value is
--    made, and the evaluator never hears of it.
-- 型の別名を足す。要るのは型検査器だけ。値は作らず、評価器は知らない
defineAlias :: Session -> AliasDecl -> Either Text (Session, Text)
defineAlias session decl
  | Just m <- Map.lookup (aliasName decl) (sessionHomes session) =
      Left ("in the type alias " <> aliasName decl <> ": type error: "
            <> renderTypeError (ForeignType "type alias" (aliasName decl) m))
  | otherwise =
  case declareAlias (sessionData session) decl of
    -- 「型の別名 Member で：型のエラー：……」
    Left err -> Left ("in the type alias " <> aliasName decl
                      <> ": type error: " <> renderTypeError err)
    Right table -> Right
      ( session { sessionData = table }
      , "type alias " <> aliasName decl )
-- ANCHOR_END: defineAlias

-- ANCHOR: load
-- ANCHOR: addDecls
-- 📜 Add declarations to a session. A type annotation goes with the
--    definition right below it. The order of the definitions does not
--    matter, as in Haskell:
--      1. the types (data, type alias) and the infix lines, in order,
--      2. the heads of the classes and the instances,
--      3. the definitions, each after the ones it uses (the ones that
--         use each other, together: see groupsOf),
--      4. the bodies of the classes and the instances (they may use
--         any definition),
--      5. every value is computed together (see computeLabelled).
-- 宣言をセッションに足す。型の注釈は、すぐ下の定義と組にする。定義の
-- 順番は、Haskell と同じく、どうでもよい：
--   1. 型（data、type alias）と infix の行を、順に
--   2. クラスとインスタンスの頭
--   3. 定義。それぞれ、使う定義のあとに（おたがいを使うものは一緒に。
--      groupsOf を見る）
--   4. クラスとインスタンスの中身（どの定義を使ってもよい）
--   5. 値は全部まとめて計算する（computeLabelled を見る）
addDecls :: Session -> [Decl] -> Either Text Session
addDecls s decls = do
  defs <- pairSignatures decls
  s1 <- foldM addType s decls
  s2 <- foldM addHead s1 decls
  (s3, values) <- foldM addGroup (s2, []) (groupsOf defs)
  (s4, bodies) <- foldM addBody (s3, []) decls
  env <- computeLabelled (sessionValues s4) (values <> bodies)
  let own = sessionOwn s <> Set.fromList (map defName defs)
      found = concatMap (keyWarnings own)
        (map defExpr defs <> [ e | (_, e, _) <- values <> bodies ])
  pure s4 { sessionValues = env, sessionOwn = own
          , sessionWarnings = sessionWarnings s <> nub found }
-- ANCHOR_END: addDecls

-- ANCHOR: pairSignatures
-- 🏷️ One definition of a file, with its annotation (if it has one).
-- ファイルの定義1つと、その注釈（あれば）
data Def = Def
  { defName :: Name
  , defSig  :: Maybe Signature
  , defExpr :: Expr
  }

-- ✍️ Each annotation with the definition right below it.
-- 注釈を、すぐ下の定義と組にする
pairSignatures :: [Decl] -> Either Text [Def]
pairSignatures decls = case decls of
  [] -> Right []
  DefSig x sig : DefValue y e : rest
    | x == y -> (Def x (Just sig) e :) <$> pairSignatures rest
  -- 「add の型の注釈のすぐ下に、add の定義が要ります」
  DefSig x _ : _ -> Left ("error: the type annotation of " <> x
    <> " needs the definition of " <> x <> " right below it")
  DefValue x e : rest -> (Def x Nothing e :) <$> pairSignatures rest
  _ : rest -> pairSignatures rest
-- ANCHOR_END: pairSignatures

-- ANCHOR: groupsOf
-- 🔗 The definitions in groups, each group after the groups it uses:
--    the strongly connected components of "uses", as Haskell does.
--    isEven and isOdd use each other, so they are one group. Groups
--    that do not use each other keep the order they were written in.
--    (A name defined twice keeps the old way: one by one, in order.)
-- 定義をグループに分け、使うグループのあとに並べる。Haskell と同じく、
-- 「使う」の強連結成分。isEven と isOdd はおたがいを使うので、1つの
-- グループ。使い合わないグループは、書いた順のまま（2回定義した名前が
-- あれば、これまでどおり1つずつ、順に）
groupsOf :: [Def] -> [[Def]]
groupsOf defs
  | length (nub names) /= length names = map pure defs
  | otherwise = map (map (byName Map.!)) (reverse (snd (foldl visit
      (Set.empty, []) (map fst comps))))
  where
    names  = map defName defs
    byName = Map.fromList [ (defName d, d) | d <- defs ]
    uses d = [ y | y <- names, y `Set.member` freeVars (defExpr d) ]
    -- The groups, numbered in the order of their first definition.
    -- グループ。最初の定義の順に番号を付ける
    comps = zip [0 :: Int ..] (sortOn (minimum . map place) (map flattenSCC
      (stronglyConnComp [ (defName d, defName d, uses d) | d <- defs ])))
    place x = Map.findWithDefault 0 x (Map.fromList (zip names [0 :: Int ..]))
    groupOf = Map.fromList [ (x, i) | (i, xs) <- comps, x <- xs ]
    members = Map.fromList comps
    needed i = nub [ j | x <- members Map.! i, y <- uses (byName Map.! x)
                       , let j = groupOf Map.! y, j /= i ]
    visit (seen, out) i
      | i `Set.member` seen = (seen, out)
      | otherwise =
          let (seen', out') = foldl visit (Set.insert i seen, out)
                                (sortOn id (needed i))
          in (seen', members Map.! i : out')
-- ANCHOR_END: groupsOf

-- ANCHOR: addParts
-- 🧱 Step 1: a data type, a type alias, an infix line.
-- 手順1：data の型、型の別名、infix の行
addType :: Session -> Decl -> Either Text Session
addType s d = case d of
  DefData x       -> fst <$> defineData s x
  DefAlias a      -> fst <$> defineAlias s a
  DefInfix _ o f  -> Right (defineInfix s o f)
  _               -> Right s

-- 🎓 Step 2: the head of a class (its methods' types and values), or
--    of an instance (into the table).
-- 手順2：クラスの頭（メソッドの型と値）か、インスタンスの頭（表へ）
addHead :: Session -> Decl -> Either Text Session
addHead s d = case d of
  DefClass c    -> fst <$> defineClassPart Head s c
  DefInstance i -> fst <$> defineInstancePart Head s i
  _             -> Right s

-- 🧩 Step 4: the body of a class (its defaults) or of an instance (its
--    methods and dictionary): the values, to compute in step 5.
-- 手順4：クラスの中身（既定のメソッド）か、インスタンスの中身（メソッドと
-- 辞書）。値は、手順5で計算する
addBody
  :: (Session, [Pending]) -> Decl -> Either Text (Session, [Pending])
addBody (s, done) d = case d of
  DefClass c    -> later (classParts Body s c)
  DefInstance i -> later (instanceParts Body s i)
  _             -> Right (s, done)
  where
    later parts = do
      (s', values) <- parts
      Right (s', done <> [ (x, e, plainError) | (x, e) <- values ])
    plainError err = "error: " <> renderError err
-- ANCHOR_END: addParts

-- ANCHOR: addGroup
-- 🔁 A value to compute, and how to say its error.
-- 計算する値と、そのエラーの言い方
type Pending = (Name, Expr, EvalError -> Text)

-- 👯 Step 3: the types of one group of definitions. The annotated ones
--    have their types from the start; the others are found alone, or
--    together (inferGroup) when they use each other.
-- 手順3：定義のグループ1つの型。注釈のあるものは、はじめから型がある。
-- ほかは1つで、おたがいを使うなら一緒に（inferGroup）見つける
addGroup
  :: (Session, [Pending]) -> [Def] -> Either Text (Session, [Pending])
addGroup (s, done) group = do
  let table = sessionData s
  declared <- forM [ (d, sig) | d <- group, Just sig <- [defSig d] ] $
    \(d, sig) -> case annotationScheme table sig of
      Left err -> Left (inDefinition (defName d) (typeFailure s err))
      Right (sc, names) -> Right (d, sig, sc, names)
  let withDeclared = s { sessionTypes = Map.fromList
        [ (defName d, sc) | (d, _, sc, _) <- declared ]
        `Map.union` sessionTypes s }
      plain = [ d | d <- group, defSig d == Nothing ]
  found <- case plain of
    [] -> Right []
    [d] -> either (Left . inDefinition (defName d) . typeFailure s)
             (\(sc, e) -> Right [(defName d, sc, e)])
             (inferDefinition table (sessionTypes withDeclared)
                (defName d) (defExpr d))
    _ -> either (\(x, err) -> Left (inDefinition x (typeFailure s err)))
           Right
           (inferGroup table (sessionTypes withDeclared)
              [ (defName d, defExpr d) | d <- plain ])
  let s' = withDeclared
        { sessionTypes = Map.fromList [ (x, sc) | (x, sc, _) <- found ]
                           `Map.union` sessionTypes withDeclared
        , sessionSigs = foldr Map.delete (sessionSigs s)
                          [ x | (x, _, _) <- found ] }
  checked <- forM declared $ \(d, sig, sc, names) ->
    case inferAnnotated table (sessionTypes s') (defName d) names sc
           (defExpr d) of
      Left err -> Left (inDefinition (defName d) (typeFailure s err))
      Right (_, e) -> Right (defName d, sig, e)
  let signed = s' { sessionSigs = Map.fromList
        [ (x, sig) | (x, sig, _) <- checked ] `Map.union` sessionSigs s' }
      values = [ (x, e) | (x, _, e) <- found ]
               <> [ (x, e) | (x, _, e) <- checked ]
  Right (signed, done <> [ (x, e, inDefinition x . placedEval)
                         | (x, e) <- values ])

-- 「場所：x の定義で：……」
inDefinition :: Name -> (Text, Text) -> Text
inDefinition x (place, msg) =
  place <> "in the definition of " <> x <> ": " <> msg

-- A type error, with its place cut out (see placedType).
-- 型のエラー。場所を切り出す（placedType を見る）
typeFailure :: Session -> TypeError -> (Text, Text)
typeFailure s err = placedType (explain s err)
-- ANCHOR_END: addGroup

-- ANCHOR: keyWarnings
-- ⚠️ The warnings for the keys of Maps written as names:  {url: ms}
--    is the text "url", even when a variable url is right there. own
--    is the names defined at the top; the arguments, the lets and the
--    names of the patterns around count too. (The prelude's names do
--    not: {length: 3} is fine.)
-- 名前の形で書いた Map のキーへの警告。{url: ms} は、すぐそこに変数 url
-- があっても、文字 "url"。own は、いちばん外で定義した名前。まわりの
-- 引数・let・パターンの名前も数える（プレリュードの名前は数えない。
-- {length: 3} はよい）
keyWarnings :: Set.Set Name -> Expr -> [Text]
keyWarnings own = go own Nothing
  where
    go seen place e = case e of
      -- One entry of a Map: (key, value). A value that uses the
      -- variable itself ({radius: toJson radius}) means the text.
      -- Map の1項目。変数そのものを使う値（{radius: toJson radius}）なら、
      -- 文字のつもり
      Tuple [key, value]
        | Just (loc, x) <- nameKey key, x `Set.member` seen
        , not (x `Set.member` freeVars value) ->
            warnKey (maybe place Just loc) x : go seen place value
      At loc inner   -> go seen (Just loc) inner
      Lam x body     -> go (Set.insert x seen) place body
      Let x a body   -> go (Set.insert x seen) place a
                          <> go (Set.insert x seen) place body
      Case x alts    -> go seen place x <> concat
        [ go (seen <> patternVars p) place b | (p, b) <- alts ]
      _              -> concatMap (go seen place) (children e)

-- 🏷️ A key written as a name, with its place:  $key.name "url"
-- 名前の形で書いたキーと、その場所
nameKey :: Expr -> Maybe (Maybe Loc, Name)
nameKey e = case e of
  At loc (App (Var k) (Lit (LString x))) | k == keyName -> Just (Just loc, x)
  App (Var k) (Lit (LString x)) | k == keyName -> Just (Nothing, x)
  _ -> Nothing

-- 🖨️ 「url というキーは文字の "url" で、変数 url ではありません」
warnKey :: Maybe Loc -> Name -> Text
warnKey place x =
  maybe "" (\loc -> renderLoc loc <> ": ") place
    <> "warning: the key " <> x <> " is the text \"" <> x
    <> "\", not the variable " <> x <> " (write {[" <> x
    <> "]: …} for the variable, or {\"" <> x <> "\": …} to keep the text)"

-- 🌿 The expressions right inside an expression.
-- 式のすぐ内側の式
children :: Expr -> [Expr]
children e = case e of
  Lit _        -> []
  Var _        -> []
  Lam _ b      -> [b]
  App f a      -> [f, a]
  Let _ a b    -> [a, b]
  If c a b     -> [c, a, b]
  BinOp _ l r  -> [l, r]
  Tuple es     -> es
  List es      -> es
  Case x alts  -> x : map snd alts
  Record fs    -> map snd fs
  Field x _    -> [x]
  Update x fs  -> x : map snd fs
  At _ x       -> [x]
  Annot _ x    -> [x]
-- ANCHOR_END: keyWarnings

-- ANCHOR: computeLabelled
-- ⚙️ Step 5: compute every value together, as computeAll does: each
--    may use the others, in any order. The first error is told, in the
--    way of its definition.
-- 手順5：computeAll と同じく、値を全部まとめて計算する。どれも、ほかを
-- どの順で使ってもよい。最初のエラーを、その定義の言い方で知らせる
computeLabelled :: Env -> [Pending] -> Either Text Env
computeLabelled env pending =
  let env'    = Map.union
                  (Map.fromList [ (x, valueOf r) | (x, r, _) <- results ])
                  env
      results = [ (x, eval env' e, say) | (x, e, say) <- pending ]
      valueOf = either (const (VInt 0)) id
  in case [ say err | (_, Left err, say) <- results ] of
       msg : _ -> Left msg
       []      -> Right env'
-- ANCHOR_END: computeLabelled

-- ANCHOR: defineInfix
-- ➕ infixl 7 addPrice as <+>: from now on, the parser reads <+>.
-- これからは、パーサーが <+> を読む
defineInfix :: Session -> Text -> Fixity -> Session
defineInfix s o f =
  s { sessionOperators = Map.insert o f (sessionOperators s) }
-- ANCHOR_END: defineInfix
-- ANCHOR_END: load
-- ANCHOR_END: all
