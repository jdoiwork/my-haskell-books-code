-- ANCHOR: all
-- | A global variable, made with unsafePerformIO. Works, but avoid it.
-- unsafePerformIO で作るグローバル変数。動くけれど、避けたい形
module Global
  ( visitors
  , countVisitor
  ) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- ANCHOR: global
-- 🌍 One box for the whole program. NOINLINE keeps it to one box.
-- プログラム全体で1つの箱。NOINLINE で、箱が1つのままになる
{-# NOINLINE visitors #-}
visitors :: IORef Int
visitors = unsafePerformIO (newIORef 0)

-- 🙋 Anyone, anywhere, can count a visitor.
-- だれでも、どこからでも、訪問者を数えられる
countVisitor :: IO Int
countVisitor = atomicModifyIORef' visitors (\n -> (n + 1, n + 1))
-- ANCHOR_END: global
-- ANCHOR_END: all
