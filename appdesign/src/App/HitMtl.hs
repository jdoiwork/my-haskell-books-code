-- ANCHOR: all
-- | Reader and State stacked, with mtl: no lift.
-- Reader と State を重ねる。mtl で：lift はいらない
module App.HitMtl (hit) where

-- ANCHOR: hit
-- The classes MonadReader and MonadState (and the same types)
-- MonadReader・MonadState の型クラス（と、同じ型）
-- ANCHOR: imports
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State  (State, get, put)
-- ANCHOR_END: imports

-- ⚔️ Read the difficulty (Reader), then lower the goblin's HP (State).
-- 難しさを読んで（Reader）、ゴブリンの HP を減らす（State）
hit :: Int -> ReaderT Int (State Int) ()
hit damage = do
  level <- ask
  hp    <- get
  put (hp - damage * level)
-- ANCHOR_END: hit
-- ANCHOR_END: all
