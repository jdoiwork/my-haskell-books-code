-- ANCHOR: all
-- | Reader and State stacked, with transformers: lift by hand.
-- Reader と State を重ねる。transformers で：lift を手で書く
module App.HitTrans (hit) where

-- ANCHOR: hit
-- The types and functions for each layer
-- 層ごとの型と関数
-- ANCHOR: imports
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.State  (State, get, put)
-- ANCHOR_END: imports

-- ⚔️ Read the difficulty (Reader), then lower the goblin's HP (State).
-- 難しさを読んで（Reader）、ゴブリンの HP を減らす（State）
hit :: Int -> ReaderT Int (State Int) ()
hit damage = do
  level <- ask
  hp    <- lift get
  lift (put (hp - damage * level))
-- ANCHOR_END: hit
-- ANCHOR_END: all
