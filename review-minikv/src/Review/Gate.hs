-- ANCHOR: all
-- | The guild's gate: let each adventurer in at most `limit` times in
--   every window of `windowSeconds` seconds (a rate limiter).
-- ギルドの門番：どの冒険者も、windowSeconds 秒ごとに limit 回まで通す
-- （レート制限）
-- ANCHOR: head
module Review.Gate
  ( Rule (..)
  , gateKey
  , hit
  , allowAll
  , allow
  ) where

import Data.List (mapAccumL)
import Data.Text (Text)

import MiniKv.Client (Connection)
import MiniKv.Store (Store, apply, emptyStore)
import MiniKv.Typed (ClientError, run)
import qualified MiniKv.Typed as Typed
import MiniKv.Types
-- ANCHOR_END: head

-- ANCHOR: rule
-- 📏 How many times, in how many seconds.
-- 何秒のあいだに、何回まで
data Rule = Rule
  { limit         :: Int
  , windowSeconds :: Int
  }
  deriving (Show, Eq)

-- 🔑 Where the count of one adventurer lives.
-- 1人の冒険者の回数を、しまっておくキー
gateKey :: Text -> Key
gateKey who = "gate:" <> who
-- ANCHOR_END: rule

-- ANCHOR: hit
-- 🚪 Step 1: one knock on the gate, at time `now`. Count it with INCR;
--    the first knock of a window also sets the deadline with EXPIRE.
-- ステップ1：時刻 now に、門を1回たたく。INCR で数え、
-- 窓の最初の1回なら、EXPIRE で期限も決める
hit :: Rule -> Millis -> Text -> Store -> (Bool, Store)
hit rule now who store0 =
  case apply now (Incr key) store0 of
    (Number 1, store1) ->
      (True, snd (apply now (Expire key (windowSeconds rule)) store1))
    (Number n, store1) -> (n <= limit rule, store1)
    (_,        store1) -> (False, store1)
  where
    key = gateKey who
-- ANCHOR_END: hit

-- ANCHOR: allowAll
-- 🎞️ Knock at each of these times, in order; which were let in?
-- それぞれの時刻に、順に門をたたく。どれが通れた？
allowAll :: Rule -> Text -> [Millis] -> [Bool]
allowAll rule who times = snd (mapAccumL knock emptyStore times)
  where
    knock store now = swap (hit rule now who store)
    swap (a, b) = (b, a)
-- ANCHOR_END: allowAll

-- ANCHOR: allow
-- 🌐 Step 3: the same gate, over the network, with the typed client.
-- ステップ3：同じ門番を、ネットワークの向こうで、型に厳しいクライアントで
allow :: Connection -> Rule -> Text -> IO (Either ClientError Bool)
allow conn rule who = do
  counted <- run conn (Typed.Incr key)
  case counted of
    Left err -> pure (Left err)
    Right 1  -> do
      _ <- run conn (Typed.Expire key (windowSeconds rule))
      pure (Right True)
    Right n  -> pure (Right (n <= limit rule))
  where
    key = Typed.Key (gateKey who) :: Typed.Key Int
-- ANCHOR_END: allow
-- ANCHOR_END: all
