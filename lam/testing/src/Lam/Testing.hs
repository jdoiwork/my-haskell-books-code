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
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (mkStdGen)

import Lam.Builtins
import Lam.Eval
import Lam.Io (ioOf, runAction)
import Lam.Types
import Lam.World (FakeWorld (..), emptyWorld, errorsOf, fakeWorld,
                  outputOf, putFile)
-- ANCHOR_END: head

-- ANCHOR: types
-- 🌍 The world a test gives:
--      { stdin : [String], args : [String], env : Map String,
--        files : Map String, web : Map String, seed : Maybe Int }
-- テストが渡す世界
worldType :: Type
worldType = TRecord (Map.fromList
  [ ("stdin", tList tString), ("args", tList tString)
  , ("env", tMap tString), ("files", tMap tString)
  , ("web", tMap tString), ("seed", TCon "Maybe" [TInt]) ]) Nothing

-- 🧾 What came out of the run:
--      { stdout : String, stderr : String, exit : Int,
--        files : Map String, value : Maybe a }
-- 動かした結果
ranType :: Type -> Type
ranType a = TRecord (Map.fromList
  [ ("stdout", tString), ("stderr", tString), ("exit", TInt)
  , ("files", tMap tString), ("value", TCon "Maybe" [a]) ]) Nothing
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
--    same scheduler as the Haskell tests), and put what came out in a
--    record. Nothing here touches the real world.
-- レコードから偽物の世界を作り、そこで頼みごとを動かし（Haskell のテストと
-- 同じスケジューラー）、出てきたものをレコードにする。本物の世界には、
-- どこも触れない
runInFake :: Value -> Value -> Either EvalError Value
runInFake w io = do
  world  <- fakeFrom w
  action <- ioOf io
  let (done, after) = runState (runAction fakeWorld action) world
      (code, said, value) = case done of
        Right v           -> (0, "", Just v)
        Left (Exited n)   -> (n, "", Nothing)
        -- An error goes to the standard error, as lam would write it.
        -- エラーは、lam が書くのと同じく標準エラーへ
        Left (Failed err) ->
          (1, uncurry (<>) (placedEval err) <> "\n", Nothing)
  Right (VRecord (Map.fromList
    [ ("stdout", VString (outputOf after))
    , ("stderr", VString (errorsOf after <> said))
    , ("exit",   VInt (fromIntegral code))
    , ("files",  VMap (VString <$> worldFiles after))
    , ("value",  maybeValue value) ]))
-- ANCHOR_END: runInFake

-- ANCHOR: fakeFrom
-- 🧱 The fields of the record, put into a FakeWorld. A seed mixes the
--    order of the jobs whose clocks are the same.
-- レコードのフィールドを、FakeWorld に入れる。種があれば、時計が同じ
-- 仕事の順番を混ぜる
fakeFrom :: Value -> Either EvalError FakeWorld
fakeFrom w = do
  stdin <- fieldOf "stdin" w >>= list >>= traverse string
  args  <- fieldOf "args" w >>= list >>= traverse string
  env   <- fieldOf "env" w >>= texts
  files <- fieldOf "files" w >>= texts
  web   <- fieldOf "web" w >>= texts
  seed  <- fieldOf "seed" w
  shuffle <- case seed of
    VCon "Just" _ [VInt n] -> Right (Just (mkStdGen (fromIntegral n)))
    _                      -> Right Nothing
  let start = emptyWorld
        { worldInput   = stdin
        , worldArgs    = args
        , worldEnv     = env
        , worldWeb     = Map.map (\body -> (200, body)) web
        , worldShuffle = shuffle }
  Right (Map.foldrWithKey putFile start files)
  where
    fieldOf x v = case v of
      VRecord m | Just fv <- Map.lookup x m -> Right fv
      _ -> wrong "Test.run"
    texts v = case v of
      VMap m -> traverse string m
      _      -> wrong "Test.run"
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
