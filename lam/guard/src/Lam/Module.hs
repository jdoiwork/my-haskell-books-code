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
  , makeModule
  , importInto
  , standardModule
  , addDecls
  , loadProgram
  , importAt
  , runProgram
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
import System.FilePath (normalise, takeDirectory, (<.>), (</>))
import Text.Megaparsec (errorBundlePretty)

import Lam.Builtins (Builtin (..))
import Lam.Io (Handler)
import Lam.Parser (program, runLamParser)
import Lam.Program
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: makeModule
-- 📦 Make a module from a file that has been checked and computed:
--    pick, from what it defined, the names its exposing list shows.
--    builtins are the names written in Haskell that belong to it
--    (insert, for Map).
-- 型を確かめて計算し終えたファイルから、モジュールを作る：定義した
-- もののうち、exposing の一覧が見せる名前を選ぶ。builtins は、Haskell で
-- 書いた、そのモジュールの名前（Map なら insert）
makeModule
  :: Bool -> FilePath -> Header -> [Name] -> [Decl] -> Session
  -> Either Text Module
makeModule standard file (Header loc m exposing) builtin decls inside = do
  let ownValues = [ x | DefValue x _ <- decls ] <> builtin
      ownTypes  = [ (dataName d, map fst (dataCons d)) | DefData d <- decls ]
  (values, shown) <- case exposing of
    ExposeAll -> Right (ownValues, Map.fromList ownTypes)
    ExposeSome items -> do
      picked <- traverse (pick ownValues ownTypes) items
      Right ( [ x | Left x <- picked ]
            , Map.fromList [ tc | Right tc <- picked ] )
  let shownCons = concat (Map.elems shown)
      exports   = Map.fromList
        [ (x, (sc, v))
        | x <- nub (values <> shownCons)
        , Just sc <- [Map.lookup x (sessionTypes inside)]
        , Just v  <- [Map.lookup x (sessionValues inside)] ]
  Right Module
    { moduleName     = m
    , moduleFile     = file
    , moduleStandard = standard
    , moduleExports  = exports
    , moduleCons     = Map.restrictKeys (constructors (sessionData inside))
                         (Set.fromList shownCons)
    , moduleShown    = Map.fromList
        [ (t, (cs, filter (`notElem` cs) all'))
        | (t, cs) <- Map.toList shown
        , Just all' <- [lookup t ownTypes] ]
    , moduleTypes    = map fst ownTypes
    , moduleData     = sessionData inside
    }
  where
    -- One item of the list: a value (Left), or a type with the
    -- constructors it shows (Right).
    -- 一覧の1つ：値（Left）か、型と見せる構成子（Right）
    pick ownValues ownTypes item = case item of
      ExposeValue x
        | x `elem` ownValues -> Right (Left x)
        | otherwise          -> notDefined x
      ExposeType t which -> case lookup t ownTypes of
        Nothing -> notDefined t
        Just cs -> either notDefined (\ds -> Right (Right (t, ds)))
                     (shownOf cs which)
    -- 「Shapes は area を見せると言っていますが、area を決めていません」
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
  -- ANCHOR: clashes
  -- The same name from two modules: say so, instead of picking one.
  -- 2つのモジュールから同じ名前：どちらかを選ばずに、エラーにする
  case [ (v, o) | (v, _) <- visible
                , Just o <- [Map.lookup v (sessionOrigins session)]
                , o /= moduleName m ] of
    (v, o) : _ -> Left $ errorAt ("the name " <> v <> " comes from both "
                          <> o <> " and " <> moduleName m)
    [] -> Right ()
  case [ (t, h) | t <- moduleTypes m
                , Just h <- [Map.lookup t (sessionHomes session)]
                , h /= moduleName m ] of
    (t, h) : _ -> Left $ errorAt ("the type " <> t <> " comes from both "
                          <> h <> " and " <> moduleName m)
    [] -> Right ()
  -- ANCHOR_END: clashes
  let table = sessionData session
  Right session
    { sessionTypes   = withEach (fmap fst . (`Map.lookup` exports))
                         `Map.union` sessionTypes session
    , sessionValues  = withEach (fmap snd . (`Map.lookup` exports))
                         `Map.union` sessionValues session
    , sessionData    = table
        { dataTypes    = dataTypes table
                           `Map.union` dataTypes (moduleData m)
        , constructors = withEach (`Map.lookup` moduleCons m)
                           `Map.union` constructors table }
    , sessionModules = Map.insert (moduleName m) m (sessionModules session)
    , sessionOrigins = Map.fromList [ (v, moduleName m) | (v, _) <- visible ]
                         `Map.union` sessionOrigins session
    , sessionHidden  = Map.fromList hidden `Map.union` sessionHidden session
    , sessionHomes   = Map.fromList [ (t, moduleName m) | t <- moduleTypes m ]
                         `Map.union` sessionHomes session
    }
  where
    exports = moduleExports m
    -- The names one item of the list takes, without the module's name.
    -- 一覧の1つが、名前を付けずに持ってくる名前
    pick item = case item of
      ExposeValue x
        | Map.member x exports -> Right [x]
        | otherwise            -> notShown x
      ExposeType t which -> case Map.lookup t (moduleShown m) of
        Nothing -> notShown t
        Just (cs, _) -> either notShown Right (shownOf cs which)
    -- 「モジュール Json は JDec を見せていません」
    notShown x = Left $ errorAt ("the module " <> moduleName m
                                 <> " does not expose " <> x)
    errorAt msg = renderLoc (importLoc imp) <> ": error: " <> msg

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
-- ANCHOR_END: importInto

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
  prog <- either (Left . T.pack . errorBundlePretty) Right
            (runLamParser program file source)
  header <- maybe (Left (T.pack file <> ": no module line")) Right
              (programHeader prog)
  inside <- addDecls start (programDecls prog)
  makeModule True file header (map fst mine) (programDecls prog) inside
-- ANCHOR_END: standardModule

-- ANCHOR: addDecls
-- 📜 Add declarations to a session, in order.
-- 宣言を、順にセッションに足す
addDecls :: Session -> [Decl] -> Either Text Session
addDecls = foldM addOne
  where
    addOne s decl = fst <$> case decl of
      DefValue x e -> define s x e
      DefData d    -> defineData s d
-- ANCHOR_END: addDecls

-- ANCHOR: files
-- 📂 How to read the files of modules: the real disk, or the fake
--    world's files in the tests. searchPath is the folders of LAM_PATH.
-- モジュールのファイルの読み方：本物のディスクか、テストなら偽物の世界
-- のファイル。searchPath は LAM_PATH のフォルダー
data Files m = Files
  { readSource :: FilePath -> m (Maybe Text)
  , searchPath :: [FilePath]
  }

-- 🚫 No files at all (a program without imports of its own).
-- ファイルは1つもない（自分の import がないプログラム）
noFiles :: Applicative m => Files m
noFiles = Files (const (pure Nothing)) []
-- ANCHOR_END: files

-- ANCHOR: loading
-- 🧳 While loading: an error stops everything (ExceptT), and each
--    module file is loaded only once (the Map, by its path).
-- 読みこみの途中：エラーなら全部止まる（ExceptT）。モジュールの
-- ファイルは、それぞれ1度だけ読む（道で引く Map）
type Loading m = ExceptT Text (StateT (Map FilePath Module) m)

-- 📜 Load a program: its imports (and theirs), then its declarations.
--    The last expression (if there is one) is given back, not
--    computed yet: it may be IO.
-- プログラムを読みこむ：import（とその先の import）、それから宣言。
-- 最後の式（あれば）は、計算せずに返す（IO かもしれないので）
loadProgram
  :: Monad m
  => Files m -> Session -> FilePath -> Text
  -> m (Either Text (Session, Maybe Expr))
loadProgram files base file source =
  evalStateT (runExceptT go) Map.empty
  where
    go = do
      (prog, session) <- loadFile files base [file] file source
      pure (session, programFinal prog)

-- 📄 Read one file, and bring in what it imports. stack is the files
--    being loaded now (to find imports that go around).
-- ファイルを1つ読み、import するものを持ってくる。stack は、いま読んで
-- いる途中のファイル（ぐるりと回る import を見つけるため）
loadFile
  :: Monad m
  => Files m -> Session -> [FilePath] -> FilePath -> Text
  -> Loading m (Program, Session)
loadFile files base stack file source = do
  prog <- liftEither $ either (Left . T.pack . errorBundlePretty) Right
            (runLamParser program file source)
  session <- foldM (bringIn files base stack file) base (programImports prog)
  done <- liftEither (addDecls session (programDecls prog))
  pure (prog, done)

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
    look [] = throwError $ errorAt ("no module " <> name <> " (looked for "
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
            throwError $ errorAt ("the imports go around in a ring: "
                    <> T.intercalate " -> "
                         (map T.pack (reverse (path : stack))))
          -- ANCHOR_END: cycle
          m <- loadModule files base (path : stack) path name source
          lift (modify (Map.insert path m))
          pure m
    errorAt msg = renderLoc (importLoc imp) <> ": error: " <> msg

-- 📦 Load a module file: the same as a program, but it must start
--    with  module Name exposing (...), and must not end with an
--    expression.
-- モジュールのファイルを読む：プログラムと同じだが、module Name
-- exposing (...) で始め、最後に式を置かない
loadModule
  :: Monad m
  => Files m -> Session -> [FilePath] -> FilePath -> Name -> Text
  -> Loading m Module
loadModule files base stack path name source = do
  (prog, inside) <- loadFile files base stack path source
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

-- ANCHOR: importAt
-- ⌨️ One import typed into the REPL: the files are looked for from
--    the folder the REPL runs in.
-- REPL に打った import の1行：ファイルは、REPL を動かしたフォルダーから探す
importAt
  :: Monad m => Files m -> Session -> Import -> m (Either Text Session)
importAt files session imp =
  evalStateT (runExceptT (bringIn files session [] "<repl>" session imp))
             Map.empty
-- ANCHOR_END: importAt

-- ANCHOR: runProgram
-- 🚪 Run a program file: load it (with its modules), show the last
--    expression (if there is one), then do main (if there is one), in
--    one of its three shapes (checkMain). The answer says how it ended.
-- プログラムのファイルを動かす：（モジュールも一緒に）読みこみ、最後の
-- 式（あれば）を見せ、それから main（あれば）を、3つの形のどれかでやる
-- （checkMain）。答えは、どう終わったか
runProgram
  :: Monad m
  => Handler m -> Files m -> Session -> FilePath -> Text -> m Ending
runProgram handle files session file source = do
  loaded <- loadProgram files session file source
  case loaded of
    Left err -> pure (Errored err)
    Right (s, final) -> do
      shown <- maybe (pure (Finished "")) (evaluate handle s) final
      case (shown, Map.lookup "main" (sessionTypes s)) of
        (Finished out, Nothing) -> pure (Finished out)
        (Finished out, Just sc) -> case checkMain sc of
          Left err -> pure (Errored ("in main: type error: "
                                       <> renderTypeError err))
          Right shape -> do
            ending <- evaluate handle s (startMain shape)
            pure $ case ending of
              Finished more -> Finished (out <> more)
              other         -> other
        (other, _) -> pure other
-- ANCHOR_END: runProgram
-- ANCHOR_END: all
