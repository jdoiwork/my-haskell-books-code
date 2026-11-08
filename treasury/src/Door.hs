-- ANCHOR: all
-- 🚪 The vault door: open it, work inside, and close it again.
-- 宝物庫の扉：開けて、中で働いて、また閉める
-- ANCHOR: imports
module Door where

import Control.Exception

import Vault (VaultAccident (..))
-- ANCHOR_END: imports

-- ANCHOR: door
-- 🔓 Open and close the door (we only write it down here).
-- 扉を開ける・閉める（ここでは、記録を出すだけ）
openDoor :: IO ()
openDoor = putStrLn "door: open"

closeDoor :: IO ()
closeDoor = putStrLn "door: closed"

-- 🪙 Count the coins. Halfway through, the goblin alarm rings.
-- 金貨を数える。途中で、ゴブリンの警報が鳴る
countCoins :: IO ()
countCoins = do
  putStrLn "counting: 1, 2, 3, ..."
  throwIO GoblinAlarm
  putStrLn "counting: done"
-- ANCHOR_END: door

-- ANCHOR: careless
-- 🙈 Open, work, close. (Careless: an accident skips the close.)
-- 開けて、働いて、閉める（うっかり版：事故が起きると、閉めない）
shiftCareless :: IO () -> IO ()
shiftCareless work = do
  openDoor
  work
  closeDoor
-- ANCHOR_END: careless

-- ANCHOR: finally
-- 🔒 Whatever happens in the work, close the door afterwards.
-- 仕事の中で何が起きても、あとで扉を閉める
shiftFinally :: IO () -> IO ()
shiftFinally work = do
  openDoor
  work `finally` closeDoor
-- ANCHOR_END: finally

-- ANCHOR: bracket
-- 🔒 The same, with bracket_: open, close, and the work in between.
-- bracket_ で同じことを：開ける、閉める、その間の仕事
inVault :: IO a -> IO a
inVault work = bracket_ openDoor closeDoor work
-- ANCHOR_END: bracket
-- ANCHOR_END: all
