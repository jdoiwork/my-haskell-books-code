-- ANCHOR: all
-- | A small supervisor, after Erlang/OTP: children that fall over are
--   started again, by a rule written as a pure function.
-- Erlang/OTP にならった、小さな監督役：倒れた子を、また始める。
-- その決まりは、純粋な関数で書く
-- ANCHOR: header
module MiniMq.Supervisor
  ( Child (..)
  , Strategy (..)
  , Intensity (..)
  , Plan (..)
  , Decision (..)
  , decide
  , TooManyRestarts (..)
  , DuplicateChild (..)
  , supervise
  ) where

import Control.Concurrent.Async (async, cancel, waitCatchSTM)
import Control.Concurrent.STM (atomically)
import Control.Exception (Exception, finally, mask, throwIO)
import Data.Foldable (asum, for_)
import Data.IORef
import Data.List (group, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
-- ANCHOR_END: header

-- ANCHOR: types
-- 👶 A child: a name, and work that should go on forever.
-- 子：名前と、ずっと続くはずの仕事
data Child = Child
  { childName :: Text
  , work      :: IO ()
  }

-- 🧭 Who starts again when one child falls.
-- 1人が倒れたとき、誰が始め直すか
data Strategy
  = OneForOne    -- ^ only the one that fell / 倒れた子だけ
  | OneForAll    -- ^ everyone / 全員
  | RestForOne   -- ^ the one that fell, and those started after it
                 --   倒れた子と、そのあとに始めた子たち
  deriving (Show, Eq)

-- 📏 At most `restarts` restarts within `seconds`. One more, and the
--    supervisor gives up and falls over itself.
-- seconds 秒のうちに、restarts 回まで始め直す。それを超えたら、
-- 監督役はあきらめて、自分が倒れる
data Intensity = Intensity
  { restarts :: Int
  , seconds  :: Double
  }
  deriving (Show, Eq)

-- 📋 The whole plan: how to restart, how often, and whom.
-- 計画の全部：どう始め直すか、何回まで、誰を
data Plan = Plan
  { strategy  :: Strategy
  , intensity :: Intensity
  , children  :: [Child]
  }
-- ANCHOR_END: types

-- ANCHOR: decide
-- ⚖️ The rule, as a pure function. Given the time now, the names of the
--    children (in the order they started), who fell, and the times of
--    the recent restarts (newest first), say what to do. Whom to start
--    again comes in the order they started.
-- 決まりを、純粋な関数で。今の時刻、子の名前（始めた順）、倒れた子、
-- 最近の始め直しの時刻（新しい順）から、どうするかを決める。
-- 始め直す子も、始めた順に並べて返す
data Decision = Restart [Text] | GiveUp
  deriving (Show, Eq)

decide :: Strategy -> Intensity -> Double -> [Text] -> Text -> [Double]
       -> (Decision, [Double])
decide how limit now names fallen history
  | length recent >= restarts limit = (GiveUp, recent)
  | otherwise                       = (Restart again, now : recent)
  where
    recent = takeWhile (> now - seconds limit) history
    again  = case how of
      OneForOne  -> [fallen]
      OneForAll  -> names
      RestForOne -> dropWhile (/= fallen) names
-- ANCHOR_END: decide

-- ANCHOR: supervise
-- 🏳️ What a supervisor throws when it gives up. Its own supervisor
--    (if any) sees it fall, like any other child.
-- 監督役があきらめたときに投げるもの。その上の監督役（がいれば）には、
-- ほかの子と同じように、倒れたと見える
newtype TooManyRestarts = TooManyRestarts Text
  deriving Show

instance Exception TooManyRestarts

-- 🪪 What a supervisor throws, before starting anyone, when two
--    children share a name. The name is how it tells children apart.
-- 2人の子が同じ名前だったときに、誰も始めずに投げるもの。
-- 監督役は、名前で子を見分ける
newtype DuplicateChild = DuplicateChild Text
  deriving Show

instance Exception DuplicateChild

-- 👀 Start every child, then wait for one to fall, ask `decide`, and
--    start again whoever it says. A supervisor is just an `IO ()`,
--    so it can be the child of another supervisor: a tree.
-- 子を全員始め、誰かが倒れるのを待ち、decide に聞いて、言われた子を
-- 始め直す。監督役もただの IO () なので、ほかの監督役の子になれる。木になる
supervise :: Plan -> IO ()
supervise plan = do
  -- Two children with one name: refuse the plan before starting anyone.
  -- 同じ名前の子が2人いたら、誰も始めずに、計画を断る
  case [name | name : _ : _ <- group (sort names)] of
    name : _ -> throwIO (DuplicateChild name)
    []       -> pure ()
  running <- newIORef Map.empty
  let start name = for_ (Map.lookup name jobs) $ \job ->
        -- Start and write down under `mask`: no exception can come in
        -- between. The child itself runs unmasked (`restore`).
        -- 始めることと書き留めることは、mask の中で1つに。そのあいだに
        -- 例外は割りこめない。子の仕事そのものは、mask を外して動く
        mask $ \restore -> do
          thread <- async (restore job)
          modifyIORef running (Map.insert name thread)
      -- Cancel first, then cross out: until it has stopped, the child
      -- stays on the list, so the clean-up below can still find it.
      -- 先に止めて、それから消す。止まるまでは一覧に残るので、
      -- 下の後始末から見つけられる
      stop name = do
        threads <- readIORef running
        for_ (Map.lookup name threads) cancel
        modifyIORef running (Map.delete name)
      watch history = do
        threads <- readIORef running
        -- Wait for any child at once: `asum` tries each in turn (STM's
        -- `orElse`), and the first one that has ended gives its name.
        -- 全員を同時に待つ。asum が1人ずつ試し（STM の orElse）、
        -- 最初に終わっていた子が、名前を返す
        fallen <- atomically $
          asum [name <$ waitCatchSTM t | (name, t) <- Map.toList threads]
        now <- getMonotonicTime
        case decide (strategy plan) (intensity plan) now names fallen
                    history of
          (GiveUp, _) -> throwIO (TooManyRestarts fallen)
          (Restart again, history') -> do
            -- Stop in the reverse order, start in the first order:
            -- a child never loses the ones it leans on while it runs.
            -- 逆の順に止めて、元の順に始める。子が動いているあいだ、
            -- 頼りにしている前の子は、いつもいる
            for_ (reverse again) stop
            for_ again start
            watch history'
  -- However the supervisor ends, stop every child, last started first.
  -- 監督役がどう終わっても、子を全員止める。あとに始めた子から先に
  (for_ names start >> watch [])
    `finally` for_ (reverse names) stop
  where
    names = map childName (children plan)
    jobs  = Map.fromList [(childName c, work c) | c <- children plan]
-- ANCHOR_END: supervise
-- ANCHOR_END: all
