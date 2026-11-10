-- ANCHOR: all
-- ANCHOR: imports
-- | The few things our programs ask of the outside world, so the
--   same program runs with the real IO and with the look-alike.
-- プログラムが外の世界に頼む、いくつかのこと。同じプログラムを、
-- 本物の IO でも、もどきでも動かせるように
module Console
  ( Console (..)
  ) where

import qualified System.Environment as Env
import qualified System.IO as IO

import qualified FakeIO as Fake
import FakeIO (FakeIO)
-- ANCHOR_END: imports

-- ANCHOR: class
-- 🎛️ Read the environment, read a line, write text.
-- 環境変数を読む、1行読む、文字を書く
class Monad m => Console m where
  lookupEnv :: String -> m (Maybe String)
  isEOF     :: m Bool
  getLine   :: m String
  putStr    :: String -> m ()

-- 🌐 The real world.
-- 本物の世界
instance Console IO where
  lookupEnv = Env.lookupEnv
  isEOF     = IO.isEOF
  getLine   = IO.getLine
  putStr text = IO.putStr text >> IO.hFlush IO.stdout

-- 🎭 The look-alike world.
-- もどきの世界
instance Console FakeIO where
  lookupEnv = Fake.lookupEnv
  isEOF     = Fake.isEOF
  getLine   = Fake.getLine
  putStr    = Fake.putStr
-- ANCHOR_END: class
-- ANCHOR_END: all
