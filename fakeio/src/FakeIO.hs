-- ANCHOR: all
-- ANCHOR: imports
-- | An IO look-alike: a function from the world before to a result
--   and the world after.
-- IO もどき：前の世界を受け取って、結果と、あとの世界を返す関数
module FakeIO
  ( World (..)
  , newWorld
  , FakeIO (..)
  , runFakeIO
  , lookupEnv
  , isEOF
  , getLine
  , putStr
  , putStrLn
  , showScreen
  ) where

import Prelude hiding (getLine, putStr, putStrLn)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Screen
-- ANCHOR_END: imports

-- ANCHOR: world
-- 🌍 Everything outside the program that it can see or change.
-- プログラムの外にあって、プログラムが見たり変えたりできるもの全部
data World = World
  { environment :: Map String String
  , stdinLines  :: [String]
  , screen      :: Screen
  }
  deriving (Show, Eq)

-- 🌱 A new world: environment variables and the lines that will be
--    typed in.
-- 新しい世界：環境変数と、これから打ちこまれる行たち
newWorld :: [(String, String)] -> [String] -> World
newWorld vars typed = World (Map.fromList vars) typed blankScreen
-- ANCHOR_END: world

-- ANCHOR: fakeio
-- 🎭 The look-alike: take the world, give back a result and the
--    world after.
-- もどき：世界を受け取り、結果と、あとの世界を返す
newtype FakeIO a = FakeIO (World -> (a, World))

runFakeIO :: FakeIO a -> World -> (a, World)
runFakeIO (FakeIO step) world = step world

-- 🔗 Joining: run the first step, hand its world to the next.
-- つなぐ：1つ目を動かし、その「あとの世界」を次に渡す
instance Monad FakeIO where
  FakeIO first >>= next = FakeIO $ \world ->
    let (x, world') = first world
    in runFakeIO (next x) world'

instance Applicative FakeIO where
  pure x = FakeIO (\world -> (x, world))
  fs <*> xs = do
    f <- fs
    x <- xs
    pure (f x)

instance Functor FakeIO where
  fmap f xs = xs >>= (pure . f)
-- ANCHOR_END: fakeio

-- ANCHOR: env
-- 🔑 Read an environment variable. Nobody changes them here.
-- 環境変数を読む。この世界では、だれも書きかえない
lookupEnv :: String -> FakeIO (Maybe String)
lookupEnv name =
  FakeIO (\world -> (Map.lookup name (environment world), world))
-- ANCHOR_END: env

-- ANCHOR: stdin
-- ⌨️ Is there nothing more to read?
-- もう読むものがない？
isEOF :: FakeIO Bool
isEOF = FakeIO (\world -> (null (stdinLines world), world))

-- ⌨️ Read one line, and it is gone from the input. A terminal
--    shows what you type (echo), so the line appears on the screen.
--    Like the real getLine, it is an error when nothing is left.
-- 1行読む。読んだ行は、入力から消える。ターミナルは打った文字を
-- 画面に映す（エコー）ので、その行は画面にも出る。
-- 本物の getLine と同じく、何も残っていなければエラー
getLine :: FakeIO String
getLine = FakeIO $ \world ->
  case stdinLines world of
    line : rest ->
      let shown = writeString (line <> "\n") (screen world)
      in (line, world { stdinLines = rest, screen = shown })
    [] -> error "getLine: end of file"
-- ANCHOR_END: stdin

-- ANCHOR: stdout
-- 🖨️ Write to the screen.
-- 画面に書く
putStr :: String -> FakeIO ()
putStr text = FakeIO $ \world ->
  ((), world { screen = writeString text (screen world) })

putStrLn :: String -> FakeIO ()
putStrLn text = putStr (text <> "\n")

-- 🖼️ How the screen of a world looks.
-- その世界の画面の見た目
showScreen :: World -> String
showScreen world = render (screen world)
-- ANCHOR_END: stdout
-- ANCHOR_END: all
