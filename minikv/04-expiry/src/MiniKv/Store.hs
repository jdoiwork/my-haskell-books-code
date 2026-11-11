-- ANCHOR: all
-- | The store and the pure core, now with deadlines. The core is told
--   the time; it never reads a clock.
-- ストアと純粋な核。期限が付いた。核は時刻を教わるだけで、時計は読まない
module MiniKv.Store
  ( Store
  , emptyStore
  , apply
  , applyAll
  , sweep
  , storedValues
  , storedKeys
  , readInt
  ) where

import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import MiniKv.Types

-- ANCHOR: store
-- 🗄️ The values, and the time each key should be forgotten.
-- 値と、それぞれのキーを忘れる時刻
data Store = Store
  { values    :: !(Map Key Value)
  , deadlines :: !(Map Key Millis)
  }
  deriving (Show, Eq)

emptyStore :: Store
emptyStore = Store Map.empty Map.empty

-- 👀 All the values and keys (for tests).
-- 値とキーの全部（テスト用）
storedValues :: Store -> [Value]
storedValues = Map.elems . values

storedKeys :: Store -> [Key]
storedKeys = Map.keys . values
-- ANCHOR_END: store

-- ANCHOR: expire
-- ⌛ Forget a key if its time has come (checked when the key is used).
-- 時刻が来ていたら、キーを忘れる（キーが使われるときに確かめる）
dropIfExpired :: Millis -> Key -> Store -> Store
dropIfExpired now key store = case Map.lookup key (deadlines store) of
  Just t | t <= now -> forget key store
  _                 -> store

-- 🧹 Forget every key whose time has come; say how many.
--    The server calls this now and then, for keys nobody touches.
-- 時刻が来たキーを全部忘れ、いくつ忘れたかを答える。
-- だれも触らないキーのために、サーバーがときどき呼ぶ
sweep :: Millis -> Store -> (Int, Store)
sweep now store =
  let due = [ k | (k, t) <- Map.toList (deadlines store), t <= now ]
  in  (length due, foldr forget store due)

-- 🗑️ Forget a key and its deadline.
-- キーと、その期限を忘れる
forget :: Key -> Store -> Store
forget key (Store vs ds) = Store (Map.delete key vs) (Map.delete key ds)
-- ANCHOR_END: expire

-- ANCHOR: getters
-- 🔍 Read a key as a list (or a set, a hash, a string); another kind
--    is an error.
-- キーをリスト（集合、ハッシュ、文字列）として読む。別の種類ならエラー
asList :: Key -> Store -> Either KvError (Seq Text)
asList key store = case Map.lookup key (values store) of
  Nothing       -> Right Seq.empty
  Just (List l) -> Right l
  Just other    -> Left (WrongType key KList (kindOf other))

asSet :: Key -> Store -> Either KvError (Set.Set Text)
asSet key store = case Map.lookup key (values store) of
  Nothing          -> Right Set.empty
  Just (Members s) -> Right s
  Just other       -> Left (WrongType key KSet (kindOf other))

asHash :: Key -> Store -> Either KvError (Map Text Text)
asHash key store = case Map.lookup key (values store) of
  Nothing       -> Right Map.empty
  Just (Hash h) -> Right h
  Just other    -> Left (WrongType key KHash (kindOf other))

asString :: Key -> Store -> Either KvError (Maybe Text)
asString key store = case Map.lookup key (values store) of
  Nothing      -> Right Nothing
  Just (Str s) -> Right (Just s)
  Just other   -> Left (WrongType key KString (kindOf other))
-- ANCHOR_END: getters

-- ANCHOR: put
-- 💾 Write a value back. An empty collection is forgotten, deadline
--    and all.
-- 値を書き戻す。空の入れ物は、期限ごと忘れる
put :: Key -> Value -> Store -> Store
put key value store
  | isEmpty value = forget key store
  | otherwise     = store { values = Map.insert key value (values store) }
  where
    isEmpty (List l)    = Seq.null l
    isEmpty (Members s) = Set.null s
    isEmpty (Hash h)    = Map.null h
    isEmpty (Str _)     = False
-- ANCHOR_END: put

-- ANCHOR: apply
-- ⚙️ The pure core, told the time `now`. First, forget the keys this
--    command touches if their time has come; then do the command.
-- 純粋な核。今の時刻 now を教わる。まず、このコマンドが触るキーの
-- 時刻が来ていたら忘れ、それからコマンドを動かす
apply :: Millis -> Command -> Store -> (Reply, Store)
apply now command store0 =
  let store = foldr (dropIfExpired now) store0 (touched command store0)
  in  run now command store

-- 🔑 The keys a command looks at (KEYS looks at all of them).
-- コマンドが見るキー（KEYS は全部を見る）
touched :: Command -> Store -> [Key]
touched command store = case command of
  Keys -> Map.keys (values store)
  _    -> keyOf command
  where
    keyOf c = case c of
      Get k -> [k]; Set k _ -> [k]; Exists k -> [k]; Incr k -> [k]
      IncrBy k _ -> [k]; LPush k _ -> [k]; RPush k _ -> [k]
      LPop k -> [k]; RPop k -> [k]; LRange k _ _ -> [k]; LLen k -> [k]
      SAdd k _ -> [k]; SRem k _ -> [k]; SMembers k -> [k]
      SIsMember k _ -> [k]; HSet k _ _ -> [k]; HGet k _ -> [k]
      HGetAll k -> [k]; TypeOf k -> [k]; Expire k _ -> [k]
      Ttl k -> [k]; Persist k -> [k]; Del ks -> ks; Keys -> []
-- ANCHOR_END: apply

-- ANCHOR: time
-- ⏳ The three time commands, and the rest as before.
-- 時刻の3つのコマンドと、残りは今までどおり
run :: Millis -> Command -> Store -> (Reply, Store)
run now command store = case command of
  Expire key seconds -> case deadlineAfter now seconds of
    Nothing -> (Failed (BadExpire key), store)
    Just t
      | Map.member key (values store) ->
          let ds = Map.insert key t (deadlines store)
          in  (Number 1, store { deadlines = ds })
      | otherwise -> (Number 0, store)
  Ttl key ->
    let present  = Map.member key (values store)
        deadline = Map.lookup key (deadlines store)
    in  case (present, deadline) of
          (False, _)      -> (Number (-2), store)   -- no key / キーがない
          (True, Nothing) -> (Number (-1), store)   -- no deadline / 期限なし
          (True, Just t)  -> (Number ((t - now + 999) `div` 1000), store)
  Persist key -> case Map.lookup key (deadlines store) of
    Just _  ->
      let ds = Map.delete key (deadlines store)
      in  (Number 1, store { deadlines = ds })
    Nothing -> (Number 0, store)
  Set key text ->
    -- SET starts afresh: the old deadline goes away.
    -- SET は、まっさらから：古い期限は消える
    let cleared = store { deadlines = Map.delete key (deadlines store) }
    in  (Ok, put key (Str text) cleared)
  _ -> valueCommand command store
-- ANCHOR_END: time

-- ANCHOR: value
-- 🧺 The commands about values (as in the earlier chapters).
-- 値についてのコマンド（前の章までと同じ）
valueCommand :: Command -> Store -> (Reply, Store)
valueCommand command store = case command of
  Get key ->
    answer (maybe Nil Bulk <$> asString key store)
  Del keys ->
    -- Count what is really gone: a key given twice is removed once.
    -- 本当に消えた数を数える：同じキーが2回あっても、消えるのは1つ
    let store' = foldr forget store keys
    in  (Number (Map.size (values store) - Map.size (values store')), store')
  Exists key ->
    (Number (if Map.member key (values store) then 1 else 0), store)
  Incr key ->
    valueCommand (IncrBy key 1) store
  IncrBy key n ->
    case asString key store of
      Left err -> (Failed err, store)
      Right current ->
        case maybe (Just 0) readInt current of
          Nothing -> (Failed (NotAnInteger key), store)
          Just old
            | overflows old n -> (Failed (Overflow key), store)
            | otherwise ->
                let new = old + n
                in  (Number new, put key (Str (T.pack (show new))) store)
  LPush key items ->
    change (asList key store) $ \l ->
      let l' = Seq.fromList (reverse items) >< l
      in  (Number (length l'), put key (List l') store)
  RPush key items ->
    change (asList key store) $ \l ->
      let l' = l >< Seq.fromList items
      in  (Number (length l'), put key (List l') store)
  LPop key ->
    change (asList key store) $ \l -> case l of
      x :<| rest -> (Bulk x, put key (List rest) store)
      Empty      -> (Nil, store)
  RPop key ->
    change (asList key store) $ \l -> case l of
      rest :|> x -> (Bulk x, put key (List rest) store)
      Empty      -> (Nil, store)
  LRange key from to ->
    answer (Many . toList . slice from to <$> asList key store)
  LLen key ->
    answer (Number . length <$> asList key store)
  SAdd key items ->
    change (asSet key store) $ \s ->
      let s' = foldr Set.insert s items
      in  (Number (Set.size s' - Set.size s), put key (Members s') store)
  SRem key items ->
    change (asSet key store) $ \s ->
      let s' = foldr Set.delete s items
      in  (Number (Set.size s - Set.size s'), put key (Members s') store)
  SMembers key ->
    answer (Many . Set.toList <$> asSet key store)
  SIsMember key item ->
    answer (Number . fromEnum . Set.member item <$> asSet key store)
  HSet key field text ->
    change (asHash key store) $ \h ->
      let isNew = not (Map.member field h)
          h'    = Map.insert field text h
      in  (Number (fromEnum isNew), put key (Hash h') store)
  HGet key field ->
    answer (maybe Nil Bulk . Map.lookup field <$> asHash key store)
  HGetAll key ->
    let pairs h = concatMap (\(f, v) -> [f, v]) (Map.toList h)
    in  answer (Many . pairs <$> asHash key store)
  TypeOf key ->
    let found = Map.lookup key (values store)
        name  = maybe "none" (kindName . kindOf) found
    in  (Bulk name, store)
  Keys ->
    (Many (Map.keys (values store)), store)
  -- Handled by `run` before we get here. / ここへ来る前に run が扱う
  Set {}     -> (Nil, store)
  Expire {}  -> (Nil, store)
  Ttl {}     -> (Nil, store)
  Persist {} -> (Nil, store)
  where
    answer :: Either KvError Reply -> (Reply, Store)
    answer (Left err)    = (Failed err, store)
    answer (Right reply) = (reply, store)
    change :: Either KvError v -> (v -> (Reply, Store)) -> (Reply, Store)
    change (Left err) _ = (Failed err, store)
    change (Right v)  k = k v
-- ANCHOR_END: value

-- 📏 LRANGE's indexes (negative counts from the end).
-- LRANGE の位置（マイナスは後ろから）
slice :: Int -> Int -> Seq a -> Seq a
slice from to xs =
  let n     = Seq.length xs
      fix i = if i < 0 then n + i else i
      start = max 0 (fix from)
      stop  = min (n - 1) (fix to)
  in  if start > stop then Seq.empty
                      else Seq.take (stop - start + 1) (Seq.drop start xs)

-- 🔢 Read a stored text as an Int: an optional "-" and decimal digits
--    only, and within Int. Anything else (" 7 ", "0x10", a number too big
--    for Int) is not an integer, as in Redis.
-- しまった文字列を Int として読む：頭の "-" と10進の数字だけで、Int に入るもの。
-- それ以外（" 7 "、"0x10"、Int に入らない大きな数）は、Redis と同じく数ではない
readInt :: Text -> Maybe Int
readInt t
  | T.null digits || not (T.all isDigit digits) = Nothing
  | n < toInteger (minBound :: Int) || n > toInteger (maxBound :: Int) = Nothing
  | otherwise = Just (fromInteger n)
  where
    digits = fromMaybe t (T.stripPrefix "-" t)
    n      = read (T.unpack t) :: Integer

-- ANCHOR: deadlineAfter
-- ⏰ The time `seconds` after `now`, or Nothing if it would go round
--    the Int ring (the same watch as INCR's `overflows`).
-- now の seconds 秒後の時刻。Int の輪を回ってしまうなら Nothing
-- （INCR の overflows と同じ見張り）
deadlineAfter :: Millis -> Int -> Maybe Millis
deadlineAfter now seconds
  | seconds > limit || seconds < negate limit = Nothing
  | overflows now ms                          = Nothing
  | otherwise                                 = Just (now + ms)
  where
    limit = maxBound `div` 1000
    ms    = seconds * 1000
-- ANCHOR_END: deadlineAfter

-- 💰 Would old + n go round the Int ring?
-- old + n は、Int の輪を回ってしまう？
overflows :: Int -> Int -> Bool
overflows old n =
  (n > 0 && old > maxBound - n) || (n < 0 && old < minBound - n)

kindName :: Kind -> Text
kindName KString = "string"
kindName KList   = "list"
kindName KSet    = "set"
kindName KHash   = "hash"

-- ANCHOR: applyAll
-- ▶️ Run many commands at the same time `now`.
-- たくさんのコマンドを、同じ時刻 now に動かす
applyAll :: Millis -> [Command] -> Store -> ([Reply], Store)
applyAll _   []       store = ([], store)
applyAll now (c : cs) store =
  let (reply, store')    = apply now c store
      (replies, store'') = applyAll now cs store'
  in  (reply : replies, store'')
-- ANCHOR_END: applyAll
-- ANCHOR_END: all
