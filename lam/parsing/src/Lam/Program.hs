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
  , runMain
  , printText
  , addDecls
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Lam.Builtins
import Lam.Class
import Lam.Data
import Lam.Eval
import Lam.Float (floatBuiltins)
import Lam.Format (formatBuiltins)
import Lam.Io (Handler, Stop (..), ioBuiltins, ioOf, runAction, stopBuiltins)
import Lam.Json (jsonBuiltins)
import Lam.Files (filesBuiltins)
import Lam.Bytes (bytesBuiltins)
import Lam.Regex (regexBuiltins)
import Lam.Parsing (parsingBuiltins)
import Lam.Stdlib (stdlibBuiltins)
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
  }

-- ANCHOR: allBuiltins
-- 🧰 The builtins of every chapter so far.
-- ここまでの章の、組み込みの関数ぜんぶ
allBuiltins :: [Builtin]
allBuiltins =
  builtins <> floatBuiltins <> jsonBuiltins <> stdlibBuiltins
    <> formatBuiltins <> ioBuiltins <> stopBuiltins <> filesBuiltins
    <> bytesBuiltins <> regexBuiltins <> parsingBuiltins
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
defineTyped session x sig e = defineWith session x e $ do
  let table = sessionData session
  (declared, names) <- annotationScheme table sig
  inferAnnotated table (sessionTypes session) x names declared e
-- ANCHOR_END: defineTyped

-- ANCHOR: evaluate
-- 🧮 Check the type of an expression, then compute it. When the value
--    is IO, the handler does it, like GHCi: show what it gives, but
--    show nothing for ().
-- 式の型を確かめ、それから計算する。値が IO なら、GHCi と同じく、
-- ハンドラーでやってみせる。くれた値を見せるが、() なら何も見せない
evaluate :: Monad m => Handler m -> Session -> Expr -> m Ending
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
-- 🚪 main, the way into a program, has one of three shapes:
--      main : IO ()
--      main : [String] -> IO ()                        (the arguments)
--      main : [String] -> Map String String -> IO ()   (and the
--                                                       environment)
--    and each may give IO (Result e ()) in place of IO (). The number
--    of arrows of its type decides the shape.
-- プログラムの入り口 main は、3つの形のどれか。どれも IO () の代わりに
-- IO (Result e ()) でもよい。型の矢印の数で、形を決める
data MainShape = MainPlain | MainArgs | MainArgsEnv
  deriving (Eq, Show)

mainShape :: Scheme -> MainShape
mainShape (Forall _ _ t) = case arrows t of
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
  where
    call f = foldl App (Var f)

-- 🙈 The hidden names that startMain uses (Lam.Prelude puts them in).
-- startMain が使う隠した名前（Lam.Prelude が入れる）
mainBind, mainArgs, mainEnv :: Name
mainBind = "$main.bind"
mainArgs = "$main.args"
mainEnv  = "$main.env"
-- ANCHOR_END: startMain

-- 🚪 Do main. Its type must fit its shape, with IO () or
--    IO (Result e ()) at the end: then an Err stops the program, with
--    its words (as Rust's  fn main() -> Result<(), E>).
--    How it ended: to the end, an error, or exit n.
-- main をやる。型は形に合い、最後は IO () か IO (Result e ()) で
-- なければならない。後者なら、Err はその文でプログラムを止める（Rust の
-- fn main() -> Result<(), E> と同じ）。答えは終わり方：最後まで、
-- エラー、または exit n
runMain :: Monad m => Handler m -> Session -> m Ending
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
defineClass session decl
  | Just m <- Map.lookup (classDeclName decl) (sessionHomes session) =
      Left ("in the class " <> classDeclName decl <> ": type error: "
            <> renderTypeError (ForeignType "class" (classDeclName decl) m))
  | otherwise =
  case declareClass (sessionData session) (sessionTypes session) decl of
    -- 「クラス Describe で：型のエラー：……」
    Left err -> Left ("in the class " <> classDeclName decl
                      <> ": type error: " <> renderTypeError err)
    Right (table, types, values) -> do
      env <- computeAll (sessionValues session) values
      Right ( session
                { sessionTypes = Map.fromList types
                                   `Map.union` sessionTypes session
                , sessionValues = env
                , sessionData   = table }
            , T.intercalate "\n"
                [ m <> " : " <> renderScheme sc | (m, sc) <- types ] )

-- 🧩 Add an instance: its methods and its dictionary are values (with
--    names a program cannot write). Nothing new gets a type.
-- インスタンスを足す。メソッドと辞書は値になる（プログラムからは
-- 書けない名前）。型の付く新しい名前は、ない
defineInstance :: Session -> InstanceDecl -> Either Text (Session, Text)
defineInstance session decl =
  case declareInstance (sessionData session) (sessionTypes session) decl of
    -- 「インスタンス Describe Shape で：型のエラー：……」
    Left err -> Left ("in the instance " <> instClass decl <> " "
                      <> renderTypeExpr 2 (instType decl) <> ": type error: "
                      <> renderTypeError err)
    Right (table, values, label) -> do
      env <- computeAll (sessionValues session) values
      Right (session { sessionValues = env, sessionData = table }, label)

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
  -- ANCHOR: addClasses
  DefClass c : rest   -> next (defineClass s c) rest
  DefInstance i : rest -> next (defineInstance s i) rest
  -- ANCHOR_END: addClasses
  -- ANCHOR: addInfix
  DefInfix _ o f : rest -> next (Right (defineInfix s o f, T.empty)) rest
  -- ANCHOR_END: addInfix
  where
    next done rest = do
      (s', _) <- done
      addDecls s' rest
-- ANCHOR_END: addDecls

-- ANCHOR: defineInfix
-- ➕ infixl 7 addPrice as <+>: from now on, the parser reads <+>.
-- これからは、パーサーが <+> を読む
defineInfix :: Session -> Text -> Fixity -> Session
defineInfix s o f =
  s { sessionOperators = Map.insert o f (sessionOperators s) }
-- ANCHOR_END: defineInfix
-- ANCHOR_END: load
-- ANCHOR_END: all
