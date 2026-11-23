-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | The builtins for tests and debugging: Test.run runs an IO action
--   in a fake world (a pure function: IO is only data), and Debug.log
--   shows a value on the standard error, on its way through.
-- テストとデバッグの組み込み：Test.run は IO の頼みごとを偽物の世界で動かす
-- （純粋な関数。IO はただのデータだから）。Debug.log は、通りがかった値を
-- 標準エラーに見せる。
module Lam.Testing
  ( testingBuiltins
  , worldType
  , ranType
  ) where

import Control.Monad.State.Strict (runState)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.IO (hFlush, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (mkStdGen)
import Text.Read (readMaybe)

import Lam.Builtins
import Lam.Clock (millisOf)
import Lam.Eval
import Lam.Guard (Options (..), Permissions, allowOnly, readOptions)
import Lam.Io (ioOf, runAction)
import Lam.Server (webRequest)
import Lam.Types
import Lam.World (FakeWorld (..), emptyWorld, errorsOf, fakeWorld,
                  outputOf, putFile)
-- ANCHOR_END: head

-- ANCHOR: types
-- 🌍 The world a test gives (Test.World in Lam):
--      { stdin : [String], args : [String], env : Map String String,
--        files : Map String String, web : Map String String,
--        seed : Maybe Int, requests : [String],
--        commands : Map String { code : Int, out : String, err : String },
--        clock : String, delays : Map String Int, allow : [String] }
-- テストが渡す世界
worldType :: Type
worldType = TRecord (Map.fromList
  [ ("stdin", tList tString), ("args", tList tString)
  , ("env", texts), ("files", texts), ("web", texts)
  , ("seed", TCon "Maybe" [TInt])
  , ("requests", tList tString)
  , ("commands", tMap tString answerType)
  , ("clock", tString)
  , ("delays", tMap tString TInt)
  , ("allow", tList tString) ]) Nothing
  where
    texts = tMap tString tString

-- 🐚 What a fake command answers: its exit code, and what it writes.
-- 偽物のコマンドの答え：終了コードと、書くもの
answerType :: Type
answerType = TRecord (Map.fromList
  [ ("code", TInt), ("out", tString), ("err", tString) ]) Nothing

-- 🧾 What came out of the run (Test.Ran a in Lam):
--      { stdout : String, stderr : String, exit : Int,
--        files : Map String String, value : Maybe a,
--        served : [{ method, path : String, status : Int, body }],
--        ran : [String], elapsed : Int }
-- 動かした結果
ranType :: Type -> Type
ranType a = TRecord (Map.fromList
  [ ("stdout", tString), ("stderr", tString), ("exit", TInt)
  , ("files", tMap tString tString), ("value", TCon "Maybe" [a])
  , ("served", tList servedType), ("ran", tList tString)
  , ("elapsed", TInt) ]) Nothing

-- 🌐 One request the server answered.
-- サーバーが答えた頼み1つ
servedType :: Type
servedType = TRecord (Map.fromList
  [ ("method", tString), ("path", tString), ("status", TInt)
  , ("body", tString) ]) Nothing
-- ANCHOR_END: types

-- ANCHOR: table
-- 🧰 The rows for the table of builtins.
-- 組み込みの関数の表の行
testingBuiltins :: [Builtin]
testingBuiltins =
  [ prim2 "Test.run" (worldType ~> tIO a ~> ranType a) runInFake
  , prim2 "Debug.log" (tString ~> a ~> a) debugLog ]
  where
    a = TVar 0
-- ANCHOR_END: table

-- ANCHOR: runInFake
-- 🎭 Make the fake world from the record, run the action there (the
--    same scheduler as the Haskell tests, behind the same guard as
--    lam's --allow-... flags), and put what came out in a record.
--    Nothing here touches the real world.
-- レコードから偽物の世界を作り、そこで頼みごとを動かし（Haskell のテストと
-- 同じスケジューラー。lam の --allow-… と同じ見張りの向こう）、出てきた
-- ものをレコードにする。本物の世界には、どこも触れない
runInFake :: Value -> Value -> Either EvalError Value
runInFake w io = do
  (world, allowed) <- fakeFrom w
  action <- ioOf io
  let handler = allowOnly allowed fakeWorld
      (done, after) = runState (runAction handler action) world
      (code, said, value) = case done of
        Right v           -> (0, "", Just v)
        Left (Exited n)   -> (n, "", Nothing)
        -- An error goes to the standard error, as lam would write it.
        -- エラーは、lam が書くのと同じく標準エラーへ
        Left (Failed err) ->
          (1, uncurry (<>) (placedEval err) <> "\n", Nothing)
  Right (VRecord (Map.fromList
    [ ("stdout",  VString (outputOf after))
    , ("stderr",  VString (errorsOf after <> said))
    , ("exit",    VInt (fromIntegral code))
    , ("files",   textMap (VString . decodeUtf8Lenient <$> worldFiles after))
    , ("value",   maybeValue value)
    , ("served",  VList (map servedValue (reverse (worldServed after))))
    , ("ran",     VList (map (VString . commandText)
                           (reverse (worldRan after))))
    , ("elapsed", VInt (worldClock after - worldClock world)) ]))
  where
    -- "git describe --tags": the name and the words, as the keys of
    -- commands are written.
    -- 名前と語。commands のキーと同じ書き方
    commandText c = T.unwords (commandName c : commandArgs c)
    servedValue ex = VRecord (Map.fromList
      [ ("method", VString (exMethod ex)), ("path", VString (exPath ex))
      , ("status", VInt (fromIntegral (exStatus ex)))
      , ("body", VString (exBody ex)) ])
-- ANCHOR_END: runInFake

-- ANCHOR: fakeFrom
-- 🧱 The fields of the record, put into a FakeWorld, and what the
--    allow list lets through. A seed mixes the order of the jobs whose
--    clocks are the same.
-- レコードのフィールドを FakeWorld に入れ、allow の一覧が通すものも返す。
-- 種があれば、時計が同じ仕事の順番を混ぜる
fakeFrom :: Value -> Either EvalError (FakeWorld, Permissions)
fakeFrom w = do
  stdin    <- field "stdin" >>= strings
  args     <- field "args" >>= strings
  env      <- field "env" >>= texts
  files    <- field "files" >>= texts
  web      <- field "web" >>= texts
  seed     <- field "seed"
  requests <- field "requests" >>= strings >>= traverse incoming
  commands <- field "commands" >>= mapOf >>= traverse answer . unkey
  clock    <- field "clock" >>= string >>= time
  delays   <- field "delays" >>= mapOf >>= traverse whole . unkey
  allow    <- field "allow" >>= strings
  allowed  <- case readOptions (map T.unpack allow) Nothing of
    -- Only the keys: --allow-... and --deny-all.
    -- 鍵だけ：--allow-… と --deny-all
    Right (opts, []) | onlyKeys opts -> Right (permissions opts)
    Right (_, w1 : _) -> bad ("not a flag of allow: " <> T.pack w1)
    Right _           -> bad "allow takes only --allow-... and --deny-all"
    Left why          -> bad why
  let start = emptyWorld
        { worldInput    = stdin
        , worldArgs     = args
        , worldEnv      = env
        , worldWeb      = Map.map (\body -> (200, body)) web
        , worldShuffle  = shuffled seed
        , worldIncoming = requests
        , worldCommands = Map.mapKeys T.words commands
        , worldClock    = clock
        , worldStart    = clock
        , worldDelays   = delays }
  Right ( Map.foldrWithKey putFile start (encodeUtf8 <$> files)
        , allowed )
  where
    field x = case w of
      VRecord m | Just fv <- Map.lookup x m -> Right fv
      _ -> wrong "Test.run"
    strings v = list v >>= traverse string
    texts v = mapOf v >>= traverse string . unkey
    unkey m = Map.fromList [ (k, v) | (Key (VString k), v) <- Map.toList m ]
    whole v = case v of
      VInt n -> Right n
      _      -> wrong "Test.run"
    answer v = case v of
      VRecord m
        | Just (VInt c) <- Map.lookup "code" m
        , Just (VString o) <- Map.lookup "out" m
        , Just (VString e) <- Map.lookup "err" m ->
            Right (fromIntegral c, o, e)
      _ -> wrong "Test.run"
    shuffled seed = case seed of
      VCon "Just" _ [VInt n] -> Just (mkStdGen (fromIntegral n))
      _                      -> Nothing
    -- "" is the fake clock of every fake world.
    -- "" なら、どの偽物の世界とも同じ偽の時計
    time t
      | T.null t = Right (worldClock emptyWorld)
      | otherwise = case iso8601ParseM (T.unpack t) :: Maybe UTCTime of
          Just u  -> Right (millisOf u)
          -- 「時刻が読めません」
          Nothing -> bad ("not a time: " <> t)
    -- "300 POST /todos {...}": when it comes (ms), the method, the path,
    -- and the body (the rest), as in the .req files.
    -- 来る時刻（ミリ秒）、メソッド、道、本文（残り）。.req と同じ形
    incoming l = case T.words l of
      ms : method : target : _ | Just at <- readMaybe (T.unpack ms) ->
        let body = T.strip (T.drop (T.length (T.unwords [ms, method, target]))
                              (T.strip l))
        in Right (at, webRequest method target body)
      -- 「頼みの行が読めません」
      _ -> bad ("not a request: " <> l)
    bad = Left . BuiltinFailed "Test.run"
    onlyKeys opts = not (tracing opts || dryRunning opts)
                      && randomSeed opts == Nothing

-- 🗝️ A map with String keys, as a Lam value.
-- 文字のキーの Map を、Lam の値に
textMap :: Map.Map Text Value -> Value
textMap m = VMap (Map.mapKeys (Key . VString) m)
-- ANCHOR_END: fakeFrom

-- ANCHOR: debugLog
-- 🐛 Debug.log "total" t: write  total: 42  on the standard error, and
--    give t back. Like Haskell's Debug.Trace, it breaks the rule that a
--    pure function does nothing else: only for looking, never to keep.
--    The value is written when Lam computes this call (Lam is strict,
--    so in the order of the program).
-- 標準エラーに「total: 42」と書いて、t を返す。Haskell の Debug.Trace と
-- 同じく、「純粋な関数はほかに何もしない」という決まりを破る。見るため
-- だけのもので、残しておくものではない。書くのは、Lam がこの呼び出しを
-- 計算するとき（Lam は正格なので、プログラムの順に）
debugLog :: Value -> Value -> Either EvalError Value
debugLog label v = do
  name <- string label
  unsafePerformIO $ do
    TIO.hPutStrLn stderr (name <> ": " <> renderValue v)
    hFlush stderr
    pure (Right v)
{-# NOINLINE debugLog #-}
-- ANCHOR_END: debugLog
-- ANCHOR_END: all
