-- ANCHOR: all
-- | The store and the pure core: one command in, one reply and a new
--   store out. No sockets, no threads, no clock here.
-- ストアと純粋な核：コマンドを1つ受け取り、答えと新しいストアを返す。
-- ソケットもスレッドも時計も、ここにはない
-- ANCHOR: head
module MiniKv.Store
  ( Store
  , emptyStore
  , apply
  , applyAll
  , storedValues
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
-- ANCHOR_END: head

-- ANCHOR: store
-- 🗄️ The whole database: a Map from keys to values.
-- データベースの全体：キーから値への Map
newtype Store = Store (Map Key Value)
  deriving (Show, Eq)

emptyStore :: Store
emptyStore = Store Map.empty

-- 👀 All the values (for tests).
-- 値の全部（テスト用）
storedValues :: Store -> [Value]
storedValues (Store m) = Map.elems m
-- ANCHOR_END: store

-- ANCHOR: getters
-- 🔍 Read a key as a list. Missing is an empty list; another kind is
--    an error. The other three readers work the same way.
-- キーをリストとして読む。なければ空のリスト、別の種類ならエラー。
-- ほかの3つの読み手も、同じ形
asList :: Key -> Store -> Either KvError (Seq Text)
asList key (Store m) = case Map.lookup key m of
  Nothing       -> Right Seq.empty
  Just (List l) -> Right l
  Just other    -> Left (WrongType key KList (kindOf other))

asSet :: Key -> Store -> Either KvError (Set.Set Text)
asSet key (Store m) = case Map.lookup key m of
  Nothing          -> Right Set.empty
  Just (Members s) -> Right s
  Just other       -> Left (WrongType key KSet (kindOf other))

asHash :: Key -> Store -> Either KvError (Map Text Text)
asHash key (Store m) = case Map.lookup key m of
  Nothing       -> Right Map.empty
  Just (Hash h) -> Right h
  Just other    -> Left (WrongType key KHash (kindOf other))

asString :: Key -> Store -> Either KvError (Maybe Text)
asString key (Store m) = case Map.lookup key m of
  Nothing      -> Right Nothing
  Just (Str s) -> Right (Just s)
  Just other   -> Left (WrongType key KString (kindOf other))
-- ANCHOR_END: getters

-- ANCHOR: put
-- 💾 Write a value back. An empty collection is removed, so that the
--    store never holds an empty list, set or hash.
-- 値を書き戻す。空の入れ物は消すので、空のリスト・集合・ハッシュは残らない
put :: Key -> Value -> Store -> Store
put key value (Store m)
  | isEmpty value = Store (Map.delete key m)
  | otherwise     = Store (Map.insert key value m)
  where
    isEmpty (List l)    = Seq.null l
    isEmpty (Members s) = Set.null s
    isEmpty (Hash h)    = Map.null h
    isEmpty (Str _)     = False
-- ANCHOR_END: put

-- ANCHOR: apply
-- ⚙️ The pure core. Every command, every kind: the compiler checks
--    that we did not forget one.
-- 純粋な核。どのコマンドも、どの種類も。
-- 忘れていないかは、コンパイラーが見張る
apply :: Command -> Store -> (Reply, Store)
apply command store@(Store m) = case command of
  Get key ->
    answer (maybe Nil Bulk <$> asString key store)
  Set key text ->
    (Ok, put key (Str text) store)
  Del keys ->
    -- Count what is really gone: a key given twice is removed once.
    -- 本当に消えた数を数える：同じキーが2回あっても、消えるのは1つ
    let m' = foldr Map.delete m keys
    in  (Number (Map.size m - Map.size m'), Store m')
  Exists key ->
    (Number (if Map.member key m then 1 else 0), store)
  Incr key ->
    apply (IncrBy key 1) store
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
    (Bulk (maybe "none" (kindName . kindOf) (Map.lookup key m)), store)
  Keys ->
    (Many (Map.keys m), store)
  where
    -- A read-only answer: the store does not change.
    -- 読むだけの答え：ストアは変わらない
    answer :: Either KvError Reply -> (Reply, Store)
    answer (Left err)    = (Failed err, store)
    answer (Right reply) = (reply, store)
    -- A change that first needs a value of the right kind. The type
    -- signature lets it work for lists, sets and hashes alike.
    -- 正しい種類の値が要る変更。型注釈があるので、リストにも集合にも
    -- ハッシュにも使える
    change :: Either KvError v -> (v -> (Reply, Store)) -> (Reply, Store)
    change (Left err) _ = (Failed err, store)
    change (Right v)  k = k v
-- ANCHOR_END: apply

-- ANCHOR: helpers
-- 📏 LRANGE's indexes: from and to are inclusive; negative counts
--    from the end (-1 is the last).
-- LRANGE の位置：from も to も含む。マイナスは後ろから（-1 が最後）
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

-- 💰 Would old + n go round the Int ring? (the chapter "What numbers
--    really are")
-- old + n は、Int の輪を回ってしまう？（「数の正体」の章）
overflows :: Int -> Int -> Bool
overflows old n =
  (n > 0 && old > maxBound - n) || (n < 0 && old < minBound - n)

-- 🏷️ The names used by the TYPE command.
-- TYPE コマンドで使う名前
kindName :: Kind -> Text
kindName KString = "string"
kindName KList   = "list"
kindName KSet    = "set"
kindName KHash   = "hash"

-- ▶️ Run many commands, keeping all the replies.
-- たくさんのコマンドを動かし、答えを全部とっておく
applyAll :: [Command] -> Store -> ([Reply], Store)
applyAll []       store = ([], store)
applyAll (c : cs) store =
  let (reply, store')    = apply c store
      (replies, store'') = applyAll cs store'
  in  (reply : replies, store'')
-- ANCHOR_END: helpers
-- ANCHOR_END: all
