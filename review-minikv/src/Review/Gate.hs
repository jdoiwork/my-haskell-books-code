-- ANCHOR: all
-- | The guild's gate: let each adventurer in at most `limit` times in
--   every window of `windowSeconds` seconds (a rate limiter).
-- ギルドの門番：どの冒険者も、windowSeconds 秒ごとに limit 回まで通す
-- （レート制限）
-- ANCHOR: head
module Review.Gate
  ( Rule (..)
  , gateKey
  , counterKey
  , hit
  , allowAll
  , allow
  , allowMending
  ) where

import Data.List (mapAccumL)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import MiniKv.Client (Connection, request)
import MiniKv.Store (Store, apply, emptyStore)
import MiniKv.Typed (ClientError, run)
import qualified MiniKv.Typed as Typed
import MiniKv.Types
import MiniKv.Wire (Wire (..))
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

-- 🔢 The same key for the typed client: it holds a number. Keys are
--    made in this one place, so the promise is written only once.
-- 型に厳しいクライアント用の、同じキー：数をしまう。キーを作るのは
-- ここ1か所なので、約束を書くのも1回だけ
counterKey :: Text -> Typed.Key Int
counterKey who = Typed.Key (gateKey who)

-- 🧐 A rule the gate can keep: at least one knock, at least one second.
-- 門番が守れる決まり：1回以上、1秒以上
sane :: Rule -> Bool
sane rule = limit rule >= 1 && windowSeconds rule >= 1
-- ANCHOR_END: rule

-- ANCHOR: hit
-- 🚪 Step 1: one knock on the gate, at time `now`. Count it with INCR;
--    the first knock of a window also sets the deadline with EXPIRE.
--    If the deadline cannot be set (EXPIRE is refused), the knock is
--    not counted at all: a count without a deadline would stay for
--    ever. When in doubt, the gate stays shut.
-- ステップ1：時刻 now に、門を1回たたく。INCR で数え、
-- 窓の最初の1回なら、EXPIRE で期限も決める。期限を決められなかったら
-- （EXPIRE が断られたら）、数えたこともなかったことにする：期限のない
-- 数は、いつまでも残ってしまう。迷ったら、門は開けない
hit :: Rule -> Millis -> Text -> Store -> (Bool, Store)
hit rule now who store0
  | not (sane rule) = (False, store0)
  | otherwise = case apply now (Incr key) store0 of
      (Number 1, store1) ->
        case apply now (Expire key (windowSeconds rule)) store1 of
          (Number 1, store2) -> (True, store2)
          _                  -> (False, store0)
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
--    The answer of EXPIRE is read, too: if the deadline was not set,
--    the count is taken back (DEL) and nobody is let in.
-- ステップ3：同じ門番を、ネットワークの向こうで、型に厳しいクライアントで。
-- EXPIRE の答えも読む：期限を決められなかったら、数を取り消して（DEL）、
-- 通さない
allow :: Connection -> Rule -> Text -> IO (Either ClientError Bool)
allow conn rule who
  | not (sane rule) = pure (Right False)
  | otherwise = do
      counted <- run conn (Typed.Incr key)
      case counted of
        Left err -> pure (Left err)
        Right 1  -> do
          timed <- run conn (Typed.Expire key (windowSeconds rule))
          case timed of
            Right True -> pure (Right True)
            refused    -> do
              _ <- run conn (Typed.Del key)
              pure (False <$ refused)
        Right n  -> pure (Right (n <= limit rule))
  where
    key = counterKey who
-- ANCHOR_END: allow

-- ANCHOR: mend
-- 🩹 Step 4: a gate that mends a count left without a deadline (the
--    client fell between INCR and EXPIRE). After counting, it asks for
--    the TTL; -1 means "no deadline", so it sets one now. One more
--    trip, but the server stays as it is.
-- ステップ4：期限のないまま残った数（INCR と EXPIRE のあいだで
-- クライアントが落ちた）を、直す門番。数えたあとで TTL を聞き、
-- -1（期限なし）なら、いま期限を付ける。往復は1回増えるが、
-- サーバーは変えなくて済む
allowMending :: Connection -> Rule -> Text -> IO (Either ClientError Bool)
allowMending conn rule who
  | not (sane rule) = pure (Right False)
  | otherwise = do
      counted <- run conn (Typed.Incr key)
      case counted of
        Left err -> pure (Left err)
        Right n  -> do
          -- The typed client has no TTL command, so we ask in plain words.
          -- 型に厳しいクライアントに TTL はないので、ふつうの言葉で聞く
          left <- request conn ["TTL", encodeUtf8 (gateKey who)]
          timed <- case left of
            WInt (-1) -> run conn (Typed.Expire key (windowSeconds rule))
            _         -> pure (Right True)
          case timed of
            Right True -> pure (Right (n <= limit rule))
            refused    -> do
              _ <- run conn (Typed.Del key)
              pure (False <$ refused)
  where
    key = counterKey who
-- ANCHOR_END: mend
-- ANCHOR_END: all
