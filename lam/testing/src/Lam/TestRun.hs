-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | lam test: load each test file, take its tests (a list of Test),
--   walk the tree, and show what passed and what failed. With --seeds,
--   the same tests are walked again with each seed of the fake world.
-- lam test：テストのファイルを読み、その tests（Test のリスト）を取り、
-- 木をたどって、通ったものと失敗したものを見せる。--seeds なら、偽物の世界の
-- 種ごとに、同じテストをもう一度たどる。
module Lam.TestRun
  ( Seeds (..)
  , TestArgs (..)
  , readTestArgs
  , testUsage
  , Outcome (..)
  , Report (..)
  , testsOf
  , walk
  , runSeeds
  , renderReport
  , testFile
  , fileWarnings
  , findTestFiles
  , runTests
  ) where

import Control.Monad (filterM, forM)
import Data.List (isSuffixOf, nub, sort)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO (stderr)
import Text.Read (readMaybe)

import Lam.Builtins (maybeValue)
import Lam.Eval
import Lam.Module (Files, loadProgram)
import Lam.Program (Session (..))
import Lam.Syntax (Name)
import Lam.Types (renderScheme)
-- ANCHOR_END: head

-- ANCHOR: args
-- 🌱 Which seeds of the fake world to use. InOrder: none (the jobs of
--    the same moment go in the order they started). OneSeed 657: that
--    seed. ManySeeds 40: the seeds 618, 619, ..., 657.
-- 偽物の世界の、どの種を使うか。InOrder は種なし（同じ時刻の仕事は
-- 始めた順）。OneSeed 657 はその種。ManySeeds 40 は 618 から 657 まで
data Seeds = InOrder | OneSeed Integer | ManySeeds Integer
  deriving (Eq, Show)

-- The words after  lam test : the seeds, and the files or folders.
-- lam test の後ろの語：種と、ファイルかフォルダー
data TestArgs = TestArgs
  { testSeeds :: Seeds
  , testPaths :: [FilePath]
  }
  deriving (Eq, Show)

-- 🚩 lam test [--seed n | --seeds n] [files or folders...]. The
--    second is LAM_SEED: a seed for when the words give none (a flag
--    wins over it, as with lam).
-- 2つ目は LAM_SEED：語が種をくれないときの種（lam と同じく、フラグが勝つ）
readTestArgs :: [String] -> Maybe String -> Either Text TestArgs
readTestArgs given fromEnv = do
  start <- case fromEnv of
    Nothing -> Right InOrder
    Just n  -> OneSeed <$> number n
  go (TestArgs start []) given
  where
    go acc ws = case ws of
      [] -> Right acc { testPaths = reverse (testPaths acc) }
      "--seed" : n : rest  -> seeded OneSeed n rest
      "--seeds" : n : rest -> seeded ManySeeds n rest
      w : rest
        | Just n <- valueOf "--seed=" w  -> seeded OneSeed n rest
        | Just n <- valueOf "--seeds=" w -> seeded ManySeeds n rest
        -- 「lam test の知らないフラグ」
        | take 1 w == "-" ->
            Left ("unknown flag for lam test: " <> T.pack w)
        | otherwise       -> go acc { testPaths = w : testPaths acc } rest
      where
        seeded make n rest = do
          k <- number n
          go acc { testSeeds = make k } rest
    -- --seed=657 gives Just "657".
    -- --seed=657 なら Just "657"
    valueOf flag w = T.unpack <$> T.stripPrefix flag (T.pack w)
    -- 「数ではありません」
    number n =
      maybe (Left ("not a number: " <> T.pack n)) Right (readMaybe n)

-- 「使い方：lam test [--seed n | --seeds n] [ファイル・フォルダー…]」
testUsage :: Text
testUsage = T.unlines
  [ "usage: lam test [--seed n | --seeds n] [files or folders...]"
  , "  --seed n     run the tests with the fake world's seed n"
  , "  --seeds k    run them with the seeds 618 to 618 + k - 1"
  , "  LAM_SEED=n   the same as --seed n" ]
-- ANCHOR_END: args

-- ANCHOR: outcome
-- ✅ How one test ended: passed, or failed and why.
-- テスト1つの終わり方：通った、か、失敗したとわけ
data Outcome = Passed | FailedWith Text
  deriving (Eq, Show)
-- ANCHOR_END: outcome

-- ANCHOR: testsOf
-- 🔎 The value of tests, after checking its type is [Test].
-- tests の値。型が [Test] であることを確かめてから
testsOf :: Session -> Either Text Value
testsOf session =
  case ( Map.lookup "tests" (sessionTypes session)
       , Map.lookup "tests" (sessionValues session) ) of
    (Just sc, Just v)
      | renderScheme sc == "[Test]" -> Right v
      -- 「tests は Test のリストでなければなりません」
      | otherwise -> Left ("tests must be a list of Test, but it is "
                           <> renderScheme sc)
    -- 「tests がありません」
    _ -> Left "no tests here: write  tests = [ Test.equal ... ]"
-- ANCHOR_END: testsOf

-- ANCHOR: walk
-- 🌳 Walk the tree of tests: each test with its names from the top
--    (group > test), and how it ended. A test made from the seed gets
--    the seed here; if computing it fails, that is a failure, too.
-- テストの木をたどる：テストごとに、上からの名前（組 > テスト）と、
-- 終わり方。種から作るテストには、ここで種を渡す。その計算が失敗しても、
-- 失敗したことにする
walk :: Maybe Integer -> [Name] -> Value -> [([Name], Outcome)]
walk seed path v = case v of
  VList ts -> concatMap (walk seed path) ts
  VCon "Pass" _ [VString n] -> [(path <> [n], Passed)]
  VCon "Fail" _ [VString n, VString why] -> [(path <> [n], FailedWith why)]
  VCon "Group" _ [VString n, ts] -> walk seed (path <> [n]) ts
  VCon "Seeded" _ [VString n, f] ->
    case apply f (maybeValue (VInt <$> seed)) of
      Right ts -> walk seed (path <> [n]) ts
      Left err -> [(path <> [n], FailedWith (uncurry (<>) (placedEval err)))]
  other -> [(path, FailedWith ("not a test: " <> renderValue other))]
-- ANCHOR_END: walk

-- ANCHOR: report
-- 📋 One test, over all the seeds tried: its names, and the seeds it
--    failed with (with why). With no seed, the seed is Nothing.
-- 試した種のぜんぶにわたる、テスト1つ：名前と、失敗した種（とわけ）。
-- 種なしなら Nothing
data Report = Report
  { reportNames :: [Name]
  , reportFails :: [(Maybe Integer, Text)]
  }
  deriving (Eq, Show)

-- 🌱 Walk the tests once for each seed, and put the same test together.
-- 種ごとにテストを1回ずつたどり、同じテストをまとめる
runSeeds :: Seeds -> Value -> (Int, [Report])
runSeeds seeds tests = (length tried, map report names)
  where
    tried = case seeds of
      InOrder     -> [Nothing]
      OneSeed n   -> [Just n]
      ManySeeds k -> [ Just n | n <- [618 .. 618 + k - 1] ]
    runs = [ (s, walk s [] tests) | s <- tried ]
    names = unique [ n | (_, rs) <- runs, (n, _) <- rs ]
    report n = Report n
      [ (s, why) | (s, rs) <- runs, (m, FailedWith why) <- rs, m == n ]
    unique = foldr (\x seen -> x : filter (/= x) seen) []
-- ANCHOR_END: report

-- ANCHOR: renderReport
-- 🖨️ What lam test writes for one file:
--      words_test.lam
--        ✓ count > counts each word
--        ✗ count > ignores case
--            expected: ...
-- lam test が、ファイル1つについて書くもの
renderReport :: FilePath -> Seeds -> Int -> [Report] -> Text
renderReport file seeds tried reports =
  T.unlines (header : concatMap one reports)
  where
    header = T.pack file <> case seeds of
      ManySeeds k -> " (seeds 618 to " <> showT (618 + k - 1) <> ")"
      OneSeed n   -> " (seed " <> showT n <> ")"
      InOrder     -> ""
    one r = case reportFails r of
      [] -> ["  ✓ " <> path r]
      fails@((seed, why) : _) ->
        ("  ✗ " <> path r)
          : manyLine fails
          <> seedLine seed
          <> map (indent <>) (T.lines why)
          <> rerun seed
    path r = T.intercalate " > " (reportNames r)
    -- Under "with the seed 618:", a little further in.
    -- 「with the seed 618:」の下は、もう少し字下げする
    indent = case seeds of
      ManySeeds _ -> "        "
      _           -> "      "
    -- With many seeds: how many failed, and which.
    -- 種がたくさんなら：いくつ失敗したか、どれか
    manyLine fails = case seeds of
      ManySeeds _ ->
        [ "      failed with " <> showT (length fails) <> " of "
            <> showT tried <> " seeds: "
            <> listed [ n | (Just n, _) <- fails ] ]
      _ -> []
    seedLine seed = case (seeds, seed) of
      (ManySeeds _, Just n) -> ["      with the seed " <> showT n <> ":"]
      _                     -> []
    rerun seed = case (seeds, seed) of
      (ManySeeds _, Just n) ->
        [ "      again:  lam test --seed " <> showT n <> " " <> T.pack file ]
      _ -> []
    listed ns
      | length ns > 8 = T.intercalate ", " (map showT (take 8 ns)) <> ", ..."
      | otherwise     = T.intercalate ", " (map showT ns)
    showT :: Show a => a -> Text
    showT = T.pack . show
-- ANCHOR_END: renderReport

-- ANCHOR: testFile
-- 📄 One test file: load it (its imports, too), take its tests, and
--    run them with the seeds. The answer is the text to show, and how
--    many tests passed and failed (Left: the file itself is broken).
-- テストのファイル1つ：読み（import も）、tests を取り、種で動かす。
-- 答えは、見せる文字と、通ったテストと失敗したテストの数（Left なら、
-- ファイルそのものがこわれている）
testFile :: Files IO -> Session -> Seeds -> FilePath
         -> IO (Either Text (Text, Int, Int))
testFile files session seeds file = do
  source <- Utf8.readFile file
  loaded <- loadProgram files session session file source
  -- ANCHOR: testWarnings
  -- The warnings of the file (and of its imports) first, to the
  -- standard error, as when lam runs a file. The tests still run.
  -- ファイル（と import したもの）の警告を、先に標準エラーへ。lam が
  -- ファイルを動かすときと同じ。テストは、それでも動かす
  case loaded of
    Right (s, _) -> mapM_ (TIO.hPutStrLn stderr) (fileWarnings s)
    Left _       -> pure ()
  -- ANCHOR_END: testWarnings
  pure $ do
    (session', _) <- loaded
    tests <- either (\e -> Left (T.pack file <> ": " <> e)) Right
               (testsOf session')
    let (tried, reports) = runSeeds seeds tests
        failed = length (filter (not . null . reportFails) reports)
    Right ( renderReport file seeds tried reports
          , length reports - failed, failed )
-- ANCHOR_END: testFile

-- ⚠️ The warnings of a loaded test file, each once.
-- 読んだテストのファイルの警告。同じものは1度だけ
fileWarnings :: Session -> [Text]
fileWarnings = nub . sessionWarnings

-- ANCHOR: findTestFiles
-- 🗂️ The test files: the files named, and in each folder named (the
--    folder we are in, if none), every file ending in _test.lam, in
--    the folders below, too.
-- テストのファイル：名前を挙げたファイルと、挙げたフォルダー（なければ
-- いまのフォルダー）の下の、_test.lam で終わるファイル全部
findTestFiles :: [FilePath] -> IO [FilePath]
findTestFiles paths =
  concat <$> forM (if null paths then ["."] else paths) one
  where
    one p = do
      folder <- doesDirectoryExist p
      if folder then below p else pure [p]
    below dir = do
      names <- sort . filter visible <$> listDirectory dir
      let full = [ if dir == "." then n else dir </> n | n <- names ]
      dirs  <- filterM doesDirectoryExist full
      files <- filterM doesFileExist full
      deeper <- concat <$> mapM below dirs
      pure ([ f | f <- files, "_test.lam" `isSuffixOf` f ] <> deeper)
    visible n = take 1 n /= "." && n /= "dist-newstyle"
-- ANCHOR_END: findTestFiles

-- ANCHOR: runTests
-- 🏃 lam test: every file, then the sum. The answer is the exit code:
--    0 if every test passed, 1 if not (or if a file is broken).
-- lam test：ファイルを全部、それから合計。答えは終了コード：全部通れば 0、
-- そうでなければ（か、こわれたファイルがあれば）1
runTests :: Files IO -> Session -> TestArgs -> IO Int
runTests files session args = do
  paths <- findTestFiles (testPaths args)
  done <- forM paths $ \file -> do
    r <- testFile files session (testSeeds args) file
    case r of
      Left err -> TIO.putStrLn err >> pure (0, 0, 1 :: Int)
      Right (text, ok, bad) -> TIO.putStr text >> pure (ok, bad, 0)
  let passed = sum [ a | (a, _, _) <- done ]
      failed = sum [ b | (_, b, _) <- done ]
      broken = sum [ c | (_, _, c) <- done ]
  -- 「n 個のテスト：m 個が通り、k 個が失敗した」
  TIO.putStrLn $ T.pack (show (passed + failed))
    <> (if passed + failed == 1 then " test: " else " tests: ")
    <> T.pack (show passed) <> " passed, " <> T.pack (show failed)
    <> " failed"
    <> (if broken == 0 then ""
        else ", " <> T.pack (show broken)
               <> (if broken == 1 then " broken file" else " broken files"))
  pure (if failed + broken == 0 then 0 else 1)
-- ANCHOR_END: runTests
-- ANCHOR_END: all
