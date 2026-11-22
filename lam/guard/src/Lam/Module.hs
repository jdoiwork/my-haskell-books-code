-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Modules: a file shows some names (module ... exposing), and other
--   files import them (import ... as ... (names)).
-- モジュール：ファイルが名前のいくつかを見せ（module ... exposing）、
-- ほかのファイルがそれを import する（import ... as ... (名前)）。
module Lam.Module
  ( Files (..)
  , noFiles
  , diskFiles
  , makeModule
  , importInto
  , standardModule
  , loadProgram
  , runProgram
  , importAt
  , readProgram
  ) where

import Control.Monad (foldM, unless, when)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (ExceptT, runExceptT, throwError, liftEither)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import System.Directory (doesFileExist)
import System.FilePath (normalise, takeDirectory, (<.>), (</>))
import Text.Megaparsec (errorBundlePretty)

import Lam.Builtins (Builtin (..))
import Lam.Eval (Request (..))
import Lam.Io (Handler)
import Lam.Parser (fixitiesOf, program, programHead, runLamParser)
import Lam.Sugar (builtinOperator, operatorFunction)
import Lam.Program
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: makeModule
-- 📦 Make a module from a file that has been checked and computed:
--    pick, from what it declared, the names its exposing list shows.
--    builtins are the names written in Haskell that belong to it
--    (insert, for Map).
-- 型を確かめて計算し終えたファイルから、モジュールを作る：宣言した
-- もののうち、exposing の一覧が見せる名前を選ぶ。builtins は、Haskell で
-- 書いた、そのモジュールの名前（Map なら insert）
makeModule
  :: Bool -> FilePath -> Header -> [Name] -> [Decl] -> Session
  -> Either Text Module
makeModule standard file (Header loc m exposing) builtin decls inside = do
  let ownValues = [ x | DefValue x _ <- decls ] <> builtin
      -- Its types, aliases and classes, each with what (..) shows:
      -- the constructors of a type, the methods of a class.
      -- 型・型の別名・クラスと、(..) が見せるもの（型なら構成子、
      -- クラスならメソッド）
      ownTypes  = [ (dataName d, map fst (dataCons d)) | DefData d <- decls ]
               <> [ (aliasName a, []) | DefAlias a <- decls ]
               <> [ (classDeclName c, map fst (classMethods c))
                  | DefClass c <- decls ]
  (values, shown) <- case exposing of
    ExposeAll -> Right (ownValues, Map.fromList ownTypes)
    ExposeSome items -> do
      picked <- traverse (pick ownValues ownTypes) items
      Right ( [ x | Left x <- picked, not (T.null x) ]
            , Map.fromList [ tc | Right tc <- picked ] )
  let shownParts = concat (Map.elems shown)
      exports    = Map.fromList
        [ (x, (sc, v))
        | x <- nub (values <> shownParts)
        , Just sc <- [Map.lookup x (sessionTypes inside)]
        , Just v  <- [Map.lookup x (sessionValues inside)] ]
      table      = sessionData inside
      -- The instances declared here are the ones nobody else declared.
      -- ここで宣言したインスタンスは、ほかの誰も宣言していないもの
      known      = sessionInstanceHomes inside
      own        = Map.fromList
        [ (k, m) | k <- Map.keys (instances table)
                 , not (Map.member k known) ]
  operators <- shownOperators exposing (values <> shownParts)
  Right Module
    { moduleName      = m
    , moduleFile      = file
    , moduleStandard  = standard
    , moduleExports   = exports
    , moduleCons      = Map.restrictKeys (constructors table)
                          (Set.fromList shownParts)
    , moduleShown     = Map.fromList
        [ (t, (cs, filter (`notElem` cs) all'))
        | (t, cs) <- Map.toList shown
        , Just all' <- [lookup t ownTypes] ]
    , moduleTypes     = map fst ownTypes
    , moduleData      = table
    , moduleInstances = own `Map.union` known
    , moduleDicts     = Map.filterWithKey (\x _ -> "$" `T.isPrefixOf` x)
                          (sessionValues inside)
    , moduleOperators = operators
    }
  where
    -- ANCHOR: shownOperators
    -- The operators it shows: all with (..), or the ones in the list.
    -- An operator of your own goes with its function: a file that
    -- uses <+> calls Receipt.addPrice, so addPrice must be shown too.
    -- 見せる演算子：(..) なら全部、そうでなければ一覧にあるもの。自分の
    -- 演算子は、関数と一緒に行く。<+> を使うファイルは Receipt.addPrice
    -- を呼ぶので、addPrice も見せないといけない
    declared = Map.fromList [ (o, f) | DefInfix _ o f <- decls ]
    shownOperators exposing' values = do
      picked <- case exposing' of
        ExposeAll        -> Right declared
        ExposeSome items -> Map.fromList <$> traverse pickOperator
                              [ o | ExposeOperator o <- items ]
      case [ (o, f) | (o, f) <- Map.toList picked, not (fixityBuiltin f)
                    , fixityTarget f `notElem` values ] of
        -- 「Receipt は <+> を見せますが、呼ぶ関数 addPrice を見せていません」
        (o, f) : _ -> Left (renderLoc loc <> ": error: the module " <> m
                            <> " exposes (" <> o <> "), but not "
                            <> fixityTarget f <> ", the function it calls")
        [] -> Right picked
    pickOperator o = case Map.lookup o declared of
      Just f  -> Right (o, f)
      -- 「Receipt は (<+>) を見せると言っていますが、宣言していません」
      Nothing -> Left (renderLoc loc <> ": error: the module " <> m
                       <> " exposes (" <> o <> "), but does not declare it")
    -- ANCHOR_END: shownOperators
    -- One item of the list: a value (Left), or a type or a class with
    -- the parts it shows (Right).
    -- 一覧の1つ：値（Left）か、型かクラスと、見せる部品（Right）
    pick ownValues ownTypes item = case item of
      ExposeValue x
        | x `elem` ownValues -> Right (Left x)
        | x `elem` concatMap snd ownTypes -> Right (Left x)
        | otherwise          -> notDefined x
      ExposeType t which -> case lookup t ownTypes of
        Nothing -> notDefined t
        Just cs -> either notDefined (\ds -> Right (Right (t, ds)))
                     (shownOf cs which)
      -- Operators are picked by shownOperators.
      -- 演算子は shownOperators が選ぶ
      ExposeOperator _ -> Right (Left "")
    -- 「Receipt は total を見せると言っていますが、total を決めていません」
    notDefined x = Left (renderLoc loc <> ": error: the module " <> m
                         <> " exposes " <> x <> ", but does not define it")
-- ANCHOR_END: makeModule

-- ANCHOR: importInto
-- 📥 Import a module into a session. Every name it shows comes with
--    the module's name (or the name after as) in front:  Json.get,
--    J.get. The names in the list come without it, too:  get.
-- モジュールを、セッションに import する。見せる名前は、どれも前に
-- モジュールの名前（as があれば、その名前）が付く。かっこの一覧に書いた
-- 名前は、付けずにも使える
importInto :: Session -> Import -> Module -> Either Text Session
importInto session imp m = do
  bare <- case importList imp of
    Nothing                 -> Right []
    Just ExposeAll          -> Right (Map.keys exports)
    Just (ExposeSome items) -> concat <$> traverse pick items
  let prefix  = fromMaybe (moduleName m) (importAlias imp)
      -- (as it is written, the name inside the module)
      -- （書くときの形、モジュールの中の名前）
      visible = [ (prefix <> "." <> x, x) | x <- Map.keys exports ]
             <> [ (x, x) | x <- bare ]
      withEach :: (Name -> Maybe a) -> Map Name a
      withEach = eachVisible visible
      hidden =
        [ (prefix <> "." <> c, moduleName m)
        | (_, (_, cs)) <- Map.toList (moduleShown m), c <- cs ]
        <> [ (c, moduleName m)
           | (_, (shownCs, cs)) <- Map.toList (moduleShown m)
           , any (`elem` bare) shownCs, c <- cs ]
  checkClashes session imp m visible
  operators <- importedOperators
  let table = sessionData session
      inner = moduleData m
  Right session
    { sessionTypes   = withEach (fmap fst . (`Map.lookup` exports))
                         `Map.union` sessionTypes session
    , sessionValues  = withEach (fmap snd . (`Map.lookup` exports))
                         `Map.union` moduleDicts m
                         `Map.union` sessionValues session
    -- ANCHOR: importTables
    -- The types, aliases, classes and instances go in whole: they have
    -- no module's name in front, and an instance works everywhere.
    -- 型・型の別名・クラス・インスタンスは、まるごと入れる。前に
    -- モジュールの名前が付かず、インスタンスはどこでも効くから
    , sessionData    = table
        { dataTypes    = dataTypes table `Map.union` dataTypes inner
        , aliases      = aliases table `Map.union` aliases inner
        , classes      = classes table `Map.union` classes inner
        , instances    = instances table `Map.union` instances inner
        , constructors = withEach (`Map.lookup` moduleCons m)
                           `Map.union` constructors table }
    -- ANCHOR_END: importTables
    , sessionModules = Map.insert (moduleName m) m (sessionModules session)
    , sessionOrigins = Map.fromList [ (v, moduleName m) | (v, _) <- visible ]
                         `Map.union` Map.fromList
                           [ ("(" <> o <> ")", moduleName m)
                           | o <- Map.keys operators ]
                         `Map.union` sessionOrigins session
    , sessionHidden  = Map.fromList hidden `Map.union` sessionHidden session
    , sessionHomes   = Map.fromList [ (t, moduleName m) | t <- moduleTypes m ]
                         `Map.union` sessionHomes session
    , sessionInstanceHomes = sessionInstanceHomes session
                               `Map.union` moduleInstances m
    , sessionOperators = operators `Map.union` sessionOperators session
    }
  where
    exports = moduleExports m
    -- ANCHOR: importedOperators
    -- The operators in the list, (<+>), or all of them with (..).
    -- Each calls its function by the name the file sees:
    -- Receipt.addPrice (or R.addPrice, with as R).
    -- 一覧にある演算子（(<+>)）か、(..) なら全部。どれも、ファイルから
    -- 見える名前で関数を呼ぶ：Receipt.addPrice（as R なら R.addPrice）
    importedOperators = do
      picked <- case importList imp of
        Nothing                 -> Right []
        Just ExposeAll          -> Right (Map.toList (moduleOperators m))
        Just (ExposeSome items) -> traverse pickOperator
                                     [ o | ExposeOperator o <- items ]
      let seen = fromMaybe (moduleName m) (importAlias imp)
          renamed f
            | fixityBuiltin f = f
            | otherwise = f { fixityTarget = seen <> "." <> fixityTarget f }
      case [ (o, h) | (o, _) <- picked
                    , Just h <- [Map.lookup ("(" <> o <> ")")
                                   (sessionOrigins session)]
                    , h /= moduleName m ] of
        -- 「演算子 <+> が、Money と Receipt の両方から来ています」
        (o, h) : _ -> Left $ errorAt imp
          ("the operator " <> o <> " comes from both " <> h <> " and "
           <> moduleName m)
        [] -> Right (Map.fromList [ (o, renamed f) | (o, f) <- picked ])
    pickOperator o = case Map.lookup o (moduleOperators m) of
      Just f  -> Right (o, f)
      Nothing -> notShown ("(" <> o <> ")")
    -- ANCHOR_END: importedOperators
    -- The names one item of the list takes, without the module's name.
    -- 一覧の1つが、名前を付けずに持ってくる名前
    pick item = case item of
      ExposeValue x
        | Map.member x exports -> Right [x]
        | otherwise            -> notShown x
      ExposeType t which -> case Map.lookup t (moduleShown m) of
        Nothing -> notShown t
        Just (cs, _) -> either notShown Right (shownOf cs which)
      ExposeOperator _ -> Right []
    -- 「モジュール Json は JDec を見せていません」
    notShown :: Name -> Either Text a
    notShown x = Left $ errorAt imp ("the module " <> moduleName m
                                     <> " does not expose " <> x)

-- 🧱 The constructors (or methods) an item of the list shows, out of
--    cs. Left is the first one that is not in cs.
-- 一覧の1つが見せる構成子（メソッド）を、cs の中から。Left は、cs に
-- ない最初の1つ
shownOf :: [Name] -> Shown -> Either Name [Name]
shownOf cs which = case which of
  NoConstructors       -> Right []
  AllConstructors      -> Right cs
  TheseConstructors ds -> case filter (`notElem` cs) ds of
    []    -> Right ds
    d : _ -> Left d

-- 👀 Each visible name (as written, the name inside the module), with
--    what look finds for it.
-- 見える名前ごとに、look が見つけたものを
eachVisible :: [(Name, Name)] -> (Name -> Maybe a) -> Map Name a
eachVisible visible look =
  Map.fromList [ (v, a) | (v, x) <- visible, Just a <- [look x] ]

-- 🚩 file.lam:3:1: error: ...
errorAt :: Import -> Text -> Text
errorAt imp msg = renderLoc (importLoc imp) <> ": error: " <> msg
-- ANCHOR_END: importInto

-- ANCHOR: clashes
-- 💥 The same name, type or instance from two modules: say so,
--    instead of picking one.
-- 2つのモジュールから同じ名前・型・インスタンス：どちらかを選ばずに、
-- エラーにする
checkClashes
  :: Session -> Import -> Module -> [(Name, Name)] -> Either Text ()
checkClashes session imp m visible = do
  case [ (v, o) | (v, _) <- visible
                , Just o <- [Map.lookup v (sessionOrigins session)]
                , o /= moduleName m ] of
    (v, o) : _ -> Left $ errorAt imp ("the name " <> v <> " comes from both "
                          <> o <> " and " <> moduleName m)
    [] -> Right ()
  case [ (t, h) | t <- moduleTypes m
                , Just h <- [Map.lookup t (sessionHomes session)]
                , h /= moduleName m ] of
    (t, h) : _ -> Left $ errorAt imp ("the type " <> t <> " comes from both "
                          <> h <> " and " <> moduleName m)
    [] -> Right ()
  -- ANCHOR: instanceClash
  -- An instance known here already, from another place.
  -- ここで知っているインスタンスが、別のところから来た
  case [ (c, t, home)
       | ((c, t), home) <- Map.toList (moduleInstances m)
       , Map.member (c, t) (instances (sessionData session))
       , Map.lookup (c, t) (sessionInstanceHomes session) /= Just home ] of
    (c, t, home) : _ ->
      let first = fromMaybe "this file"
                    (Map.lookup (c, t) (sessionInstanceHomes session))
      in Left $ errorAt imp ("the instance " <> c <> " " <> t
                             <> " comes from both " <> first <> " and " <> home)
    [] -> Right ()
  -- ANCHOR_END: instanceClash
-- ANCHOR_END: clashes

-- ANCHOR: standardModule
-- 📚 A module of the standard library, from its Lam source. Its
--    builtins (Map.insert for Map) can be used inside without the
--    name in front (insert), and shown like its own definitions.
-- 標準ライブラリのモジュールを、Lam のソースから作る。そのモジュールの
-- 組み込み（Map なら Map.insert）は、中では前の名前なしで（insert）
-- 使え、自分の定義と同じように見せられる
standardModule :: Session -> Name -> Text -> Either Text Module
standardModule session m source = do
  let file  = "<" <> T.unpack m <> ">"
      mine  = [ (baseName (builtinName b), b)
              | b <- allBuiltins
              , (m <> ".") `T.isPrefixOf` builtinName b ]
      start = session
        { sessionTypes  = Map.fromList [ (x, builtinType b) | (x, b) <- mine ]
                            `Map.union` sessionTypes session
        , sessionValues = Map.fromList
                            [ (x, builtinValue b) | (x, b) <- mine ]
                            `Map.union` sessionValues session }
  prog <- readProgram True (sessionOperators start) file source
  header <- maybe (Left (T.pack file <> ": no module line")) Right
              (programHeader prog)
  -- No places inside: an error points at the program that called it.
  -- 中に場所は持たない。エラーは、呼んだプログラムの場所を指す
  -- ANCHOR: namedOperators
  -- Each builtin operator gets its function, made from its own tree:
  -- add is \$l $r -> $l + $r. (and, or, compose are written in Lam.)
  -- 組み込みの演算子には、その木から作った関数を付ける。add は
  -- \$l $r -> $l + $r（and・or・compose は Lam で書いてある）
  let decls   = map stripDecl (programDecls prog)
      defined = [ x | DefValue x _ <- decls ]
      named   = [ DefValue t (operatorFunction o)
                | DefInfix _ _ f <- decls, fixityBuiltin f
                , let t = fixityTarget f, t `notElem` defined
                , Just o <- [builtinOperator t] ]
  -- ANCHOR_END: namedOperators
  inside <- addDecls start (decls <> named)
  makeModule True file header (map fst mine) (decls <> named) inside
-- ANCHOR_END: standardModule

-- ANCHOR: readProgram
-- 📖 Read a file, knowing the operators it imported. The first
--    reading collects its infix lines, the second reads it all with
--    every operator. In a standard module, an operator whose function
--    has a tree of its own (+ is add) is a builtin one.
-- import した演算子を知っていて、ファイルを読む。1回目に infix の行を
-- 集め、2回目に、全部の演算子でファイル全体を読む。標準のモジュールで、
-- 呼ぶ関数が自分の木を持つ演算子（+ は add）は、組み込みのもの
readProgram :: Bool -> Operators -> FilePath -> Text -> Either Text Program
readProgram standard known file source = do
  own <- fixitiesOf file source
  let ops = Map.fromList [ (o, mark f) | (_, o, f) <- own ] `Map.union` known
  prog <- either (Left . T.pack . errorBundlePretty) Right
            (runLamParser ops program file source)
  pure prog { programDecls = map markDecl (programDecls prog) }
  where
    mark f = f { fixityBuiltin = standard
                   && isJust (builtinOperator (fixityTarget f)) }
    markDecl d = case d of
      DefInfix loc o f -> DefInfix loc o (mark f)
      _                -> d
-- ANCHOR_END: readProgram

-- ANCHOR: files
-- 📂 How to read the files of modules: the real disk, or a Map of
--    files in the tests. searchPath is the folders of LAM_PATH.
-- モジュールのファイルの読み方：本物のディスクか、テストなら Map の
-- ファイル。searchPath は LAM_PATH のフォルダー
data Files m = Files
  { readSource :: FilePath -> m (Maybe Text)
  , searchPath :: [FilePath]
  }

-- 🚫 No files at all (a program without imports of its own).
-- ファイルは1つもない（自分の import がないプログラム）
noFiles :: Applicative m => Files m
noFiles = Files (const (pure Nothing)) []

-- 💽 The real disk, with the folders of LAM_PATH.
-- 本物のディスク。LAM_PATH のフォルダーも探す
diskFiles :: [FilePath] -> Files IO
diskFiles folders = Files readIfThere folders
  where
    readIfThere path = do
      there <- doesFileExist path
      if there then Just <$> Utf8.readFile path else pure Nothing
-- ANCHOR_END: files

-- ANCHOR: loading
-- 🧳 While loading: an error stops everything (ExceptT), and each
--    module file is loaded only once (the Map, by its path).
-- 読みこみの途中：エラーなら全部止まる（ExceptT）。モジュールの
-- ファイルは、それぞれ1度だけ読む（道で引く Map）
type Loading m = ExceptT Text (StateT (Map FilePath Module) m)

-- ANCHOR: loadProgram
-- 📜 Load a program into a session: its imports (and theirs), then
--    its declarations. The expression at the end (if any) is given
--    back, not computed yet: it may be IO (see runProgram).
--    base is what each module file starts from: the prelude.
-- プログラムをセッションに読みこむ：import（とその先の import）、宣言。
-- 最後の式（あれば）は、計算せずに返す（IO かもしれない。runProgram を
-- 見る）。base は、モジュールのファイルが始まるところ（プレリュード）
loadProgram
  :: Monad m
  => Files m -> Session -> Session -> FilePath -> Text
  -> m (Either Text (Session, Maybe Expr))
loadProgram files base session file source =
  evalStateT (runExceptT go) Map.empty
  where
    go = do
      (prog, done) <- loadFile files base session [file] file source
      pure (done, programFinal prog)
-- ANCHOR_END: loadProgram

-- 📄 Read one file into a session, and bring in what it imports.
--    stack is the files being loaded now (to find imports that go
--    around).
-- ファイルを1つセッションに読み、import するものを持ってくる。stack は、
-- いま読んでいる途中のファイル（ぐるりと回る import を見つけるため）
loadFile
  :: Monad m
  => Files m -> Session -> Session -> [FilePath] -> FilePath -> Text
  -> Loading m (Program, Session)
loadFile files base session stack file source = do
  (_, imports) <- liftEither $
    either (Left . T.pack . errorBundlePretty) Right
      (runLamParser Map.empty programHead file source)
  withImports <- foldM (bringIn files base stack file) session imports
  prog <- liftEither
            (readProgram False (sessionOperators withImports) file source)
  done <- liftEither (addDecls withImports (programDecls prog))
  liftEither (checkTargets done (programDecls prog))
  pure (prog, done)

-- ANCHOR: checkTargets
-- 🎯 Each infix line of the file calls a function the file knows.
-- ファイルの infix の行は、どれもファイルが知っている関数を呼ぶ
checkTargets :: Session -> [Decl] -> Either Text ()
checkTargets session decls =
  case [ (loc, o, f) | DefInfix loc o f <- decls, not (fixityBuiltin f)
                     , not (Map.member (fixityTarget f)
                                       (sessionTypes session)) ] of
    -- 「<+> は addPrice を呼びますが、addPrice はありません」
    (loc, o, f) : _ -> Left (renderLoc loc <> ": error: " <> o <> " calls "
                             <> fixityTarget f <> ", but there is no "
                             <> fixityTarget f)
    [] -> Right ()
-- ANCHOR_END: checkTargets

-- 📥 One import line: find the module, then import it.
-- import の1行：モジュールを見つけ、import する
bringIn
  :: Monad m
  => Files m -> Session -> [FilePath] -> FilePath -> Session -> Import
  -> Loading m Session
bringIn files base stack from session imp = do
  m <- findModule files base stack from imp
  liftEither (importInto session imp m)
-- ANCHOR_END: loading

-- ANCHOR: findModule
-- 🔎 Find a module: a standard one first, then  Name.lam  next to the
--    file that imports it, then in each folder of LAM_PATH.
-- モジュールを探す：まず標準のもの。次に、import したファイルの隣の
-- Name.lam、それから LAM_PATH のフォルダーを順に
findModule
  :: Monad m
  => Files m -> Session -> [FilePath] -> FilePath -> Import
  -> Loading m Module
findModule files base stack from imp =
  case Map.lookup name (sessionModules base) of
    Just m | moduleStandard m -> pure m
    _ -> look places
  where
    name   = importName imp
    places = [ normalise (dir </> T.unpack name <.> "lam")
             | dir <- takeDirectory from : searchPath files ]
    look [] = throwError $ errorAt imp
      ("no module " <> name <> " (looked for "
       <> T.intercalate ", " (map T.pack places) <> ")")
    look (path : rest) = do
      cached <- lift (gets (Map.lookup path))
      maybe (load path rest) pure cached
    -- Not loaded yet: read the file, if it is there.
    -- まだ読んでいない：ファイルがあれば読む
    load path rest = do
      found <- lift (lift (readSource files path))
      case found of
        Nothing -> look rest
        Just source -> do
          -- ANCHOR: cycle
          -- Already being loaded: the imports go around in a ring.
          -- もう読んでいる途中：import が輪になっている
          when (path `elem` stack) $
            throwError $ errorAt imp ("the imports go around in a ring: "
                    <> T.intercalate " -> "
                         (map T.pack (reverse (path : stack))))
          -- ANCHOR_END: cycle
          m <- loadModule files base (path : stack) path name source
          lift (modify (Map.insert path m))
          pure m

-- 📦 Load a module file: the same as a program, but it starts from
--    the prelude, it must start with  module Name exposing (...), and
--    must not end with an expression.
-- モジュールのファイルを読む：プログラムと同じだが、プレリュードから
-- 始め、module Name exposing (...) で始め、最後に式を置かない
loadModule
  :: Monad m
  => Files m -> Session -> [FilePath] -> FilePath -> Name -> Text
  -> Loading m Module
loadModule files base stack path name source = do
  (prog, inside) <- loadFile files base base stack path source
  header <- case programHeader prog of
    Nothing -> throwError
      (T.pack path <> ":1:1: error: a module starts with the line  module "
       <> name <> " exposing (...)")
    Just h@(Header loc m _) -> do
      unless (m == name) $
        throwError (renderLoc loc <> ": error: the file " <> T.pack path
                    <> " must be the module " <> name <> ", not " <> m)
      pure h
  when (isJust (programFinal prog)) $
    throwError (T.pack path <> ": error: a module cannot end with an "
                <> "expression (only a program can)")
  liftEither (makeModule False path header [] (programDecls prog) inside)
-- ANCHOR_END: findModule

-- ANCHOR: runProgram
-- 🚪 Run a loaded program: show the expression at the end (if there
--    is one; an IO is done), then do main (if there is one). Showing
--    is a request to the screen too, so it comes out in its place.
--    The files of the modules were all read before: running reads
--    only what the program asks for.
-- 読みこんだプログラムを動かす：最後の式（あれば。IO ならやる）を見せ、
-- それから main（あれば）をやる。見せるのも画面への頼みごとなので、
-- 出る順番がそろう。モジュールのファイルは、もう全部読んである。
-- 動かすときに読むのは、プログラムが頼んだものだけ
runProgram
  :: Monad m => Handler m -> Session -> Maybe Expr -> m Ending
runProgram handle session final = do
  shown <- maybe (pure (Finished "")) (evaluate handle session) final
  case shown of
    Finished out -> do
      unless (T.null out) $ () <$ handle (PutStr (out <> "\n"))
      if Map.member "main" (sessionTypes session)
        then runMain handle session
        else pure (Finished "")
    other -> pure other
-- ANCHOR_END: runProgram

-- ANCHOR: importAt
-- ⌨️ One import typed into the REPL: the files are looked for from
--    the folder the REPL runs in.
-- REPL に打った import の1行：ファイルは、REPL を動かしたフォルダーから探す
importAt
  :: Monad m
  => Files m -> Session -> Session -> Import -> m (Either Text Session)
importAt files base session imp =
  evalStateT (runExceptT (bringIn files base [] "<repl>" session imp))
             Map.empty
-- ANCHOR_END: importAt
-- ANCHOR_END: all
