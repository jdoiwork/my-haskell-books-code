-- ANCHOR: all
-- | Effects as data: a program asks for dice rolls and says things,
--   and a handler decides what those mean.
-- エフェクトをデータに：プログラムはサイコロを頼み、話す。
-- それが何を意味するかは、ハンドラーが決める
module Modern.Effects
  ( Quest (..)
  , roll
  , say
  , attack
  , battle
  , runScriptedDraft
  , runScripted
  , runIO
  ) where

-- ANCHOR: imports
import Control.Monad (ap, replicateM)
import System.Random (randomRIO)
-- ANCHOR_END: imports

-- ANCHOR: quest
-- 📜 A program is a list of requests. Each request carries
--    "what to do next" once it gets its answer.
-- プログラムは、頼みごとの並び。頼みごとは、答えをもらったあとの
-- 「続き」を持っている
data Quest a
  = Done a
  | Roll Int (Int -> Quest a)
  | Say String (Quest a)

-- 🎲 Ask for a roll of an n-sided die.
-- n 面のサイコロを頼む
roll :: Int -> Quest Int
roll sides = Roll sides Done

-- 💬 Ask to say something.
-- 何かを言うように頼む
say :: String -> Quest ()
say text = Say text (Done ())
-- ANCHOR_END: quest

-- ANCHOR: monad
-- 🔗 Joining programs: pass "what comes after" down to the end.
-- プログラムをつなぐ：「そのあと」を、いちばん奥まで渡していく
instance Monad Quest where
  Done x      >>= next = next x
  Roll n k    >>= next = Roll n (\r -> k r >>= next)
  Say t after >>= next = Say t (after >>= next)

instance Applicative Quest where
  pure  = Done
  (<*>) = ap

instance Functor Quest where
  fmap f q = q >>= (pure . f)
-- ANCHOR_END: monad

-- ANCHOR: program
-- ⚔️ One attack: roll 20 to hit, then 6 for damage.
-- 1回の攻撃：20 面で当たり判定、6 面でダメージ
attack :: Quest Int
attack = do
  hit <- roll 20
  if hit >= 10
    then do
      damage <- roll 6
      say ("hit for " <> show damage)
      pure damage
    else do
      say "missed"
      pure 0

-- 🗡️ Three attacks in a row; the total damage.
-- 3回続けて攻撃。ダメージの合計
battle :: Quest Int
battle = do
  damages <- replicateM 3 attack
  pure (sum damages)
-- ANCHOR_END: program

-- ANCHOR: draft
-- 🧪 The first draft: it believes the script as it is.
-- 最初の下書き：台本の出目を、そのまま信じる
runScriptedDraft :: [Int] -> Quest a -> ([String], a)
runScriptedDraft _        (Done x)      = ([], x)
runScriptedDraft (r : rs) (Roll _ k)    = runScriptedDraft rs (k r)
runScriptedDraft []       (Roll _ k)    = runScriptedDraft [] (k 1)
runScriptedDraft rs       (Say t after) =
  let (rest, x) = runScriptedDraft rs after
  in (t : rest, x)
-- ANCHOR_END: draft

-- ANCHOR: handlers
-- 🧪 For tests: the rolls come from a list, the words are collected.
--    A roll from the list is fitted onto the die (16 on a d6 is 4).
-- テスト用：出目はリストから、言ったことは集める。
-- リストの出目は、サイコロの目に収める（6 面で 16 なら 4）
runScripted :: [Int] -> Quest a -> ([String], a)
runScripted _        (Done x)      = ([], x)
runScripted (r : rs) (Roll n k)    = runScripted rs (k (fit n r))
runScripted []       (Roll _ k)    = runScripted [] (k 1)
runScripted rs       (Say t after) =
  let (rest, x) = runScripted rs after
  in (t : rest, x)

-- 🎯 Fit any number onto the faces 1 .. n.
-- どんな数も、1 から n の目に収める
fit :: Int -> Int -> Int
fit n r = 1 + (r - 1) `mod` n

-- 🎰 For real: random rolls, words on the screen.
-- 本番用：本物の乱数で振り、画面に表示する
runIO :: Quest a -> IO a
runIO (Done x)      = pure x
runIO (Roll n k)    = do
  r <- randomRIO (1, n)
  runIO (k r)
runIO (Say t after) = do
  putStrLn t
  runIO after
-- ANCHOR_END: handlers
-- ANCHOR_END: all
