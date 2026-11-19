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
  , evaluate
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
import Lam.Json (jsonBuiltins)
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
    <> formatBuiltins
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
    Left err -> Left (inDefinition
                  ("type error: " <> renderTypeError (explain session err)))
    Right (sc, e) ->
      let env'   = Map.insert x v (sessionValues session)
          result = eval env' e
          v      = either (const (VInt 0)) id result
      in case result of
           Left err -> Left (inDefinition ("error: " <> renderError err))
           Right _  -> Right
             ( session
                 { sessionTypes  = Map.insert x sc (sessionTypes session)
                 , sessionValues = env' }
             , x <> " : " <> renderUsual sc )
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
    Left err -> Left ("type error: " <> renderTypeError (explain session err))
    -- ANCHOR: evaluateOpen
    -- A dictionary still needed: total [] has no type to choose with.
    -- まだ辞書が要る：total [] には、選ぶための型がない
    Right (Forall _ ns t, _)
      | (_, c) : _ <- dictNeeds ns ->
          Left ("type error: " <> renderTypeError (AmbiguousClass c t))
    -- ANCHOR_END: evaluateOpen
    Right (t, e') -> case eval (sessionValues session) e' of
      Left err -> Left ("error: " <> renderError err)
      -- A number of any type is shown as an Int: 1 + 2 is 3 : Int.
      -- どの型でもよい数は、Int にして見せる
      Right v  -> Right (renderValue v <> " : "
                           <> renderScheme (defaultNumbers t))
-- ANCHOR_END: evaluate

-- ANCHOR: explain
-- 🙈 An unknown name that is a hidden constructor: say so.
--    "unknown name: JDec" becomes "JDec is hidden by the module Json".
-- 知らない名前が、隠された構成子なら、そう言う
explain :: Session -> TypeError -> TypeError
explain session err = case err of
  UnknownName x        | Just m <- hiddenIn x -> Hidden x m
  UnknownConstructor c | Just m <- hiddenIn c -> Hidden c m
  _ -> err
  where
    hiddenIn x = Map.lookup x (sessionHidden session)
-- ANCHOR_END: explain

-- ANCHOR: printText
-- 🖨️ :print e  writes a String as it is, and a [String] one item a
--    line: no quotes and no \n, so that lined-up text can be seen.
--    (Lam itself cannot write to the screen yet.)
-- :print e は、String をそのまま、[String] を1行に1つずつ書く。引用符も
-- \n もないので、そろえた文字を目で見られる（Lam そのものは、まだ
-- 画面に書けない）
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
