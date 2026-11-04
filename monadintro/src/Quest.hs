-- ANCHOR: all
-- 🗺️ A type of our own, and the three "licences" that let it use do.
-- 自分の型と、do を使えるようにする3つの「資格」
module Quest (Quest (..), step, journey) where

-- ANCHOR: type
-- 🗺️ A quest either fails (with where it failed) or gets through
--    with a result.
-- 依頼は、失敗する（どこで失敗したかを持って）か、結果を持って進むか
data Quest a
  = Failed String
  | Cleared a
  deriving (Show)
-- ANCHOR_END: type

-- ANCHOR: instances
-- 🧺 Functor: change what is inside, keeping the box.
-- Functor：箱はそのままで、中身を変える
instance Functor Quest where
  fmap _ (Failed place) = Failed place
  fmap f (Cleared x)    = Cleared (f x)

-- 🤝 Applicative: put a plain value in the box (pure),
--    and combine boxes that do not depend on each other (<*>).
-- Applicative：ふつうの値を箱に入れる（pure）、
-- おたがいに頼らない箱を組み合わせる（<*>）
instance Applicative Quest where
  pure x = Cleared x
  Failed place <*> _ = Failed place
  Cleared f    <*> q = fmap f q

-- 🔗 Monad: hand what is inside to the next step (>>=).
-- Monad：中身を、次の手順に渡す（>>=）
instance Monad Quest where
  Failed place >>= _    = Failed place
  Cleared x    >>= next = next x
-- ANCHOR_END: instances

-- ANCHOR: journey
-- ⚔️ One step of the journey: lose some HP, fail if it runs out.
-- 旅の1歩：HP が減る。尽きたら失敗
step :: String -> Int -> Int -> Quest Int
step place damage hp
  | hp > damage = Cleared (hp - damage)
  | otherwise   = Failed place

-- 🗺️ Forest, then cave, then tower: do works for our own type now.
-- 森、洞窟、塔の順に：自分の型でも do が使える
journey :: Int -> Quest Int
journey hp = do
  hp1 <- step "forest" 30 hp
  hp2 <- step "cave"   40 hp1
  step "tower" 50 hp2
-- ANCHOR_END: journey
-- ANCHOR_END: all
