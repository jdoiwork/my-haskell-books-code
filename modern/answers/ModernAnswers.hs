-- ANCHOR: all
-- | Answers to the training of Part XII.
-- 第XIII部のトレーニングの解答
module ModernAnswers
  ( Element6 (..)
  , beats6
  , notBoth
  , Door (..)
  , tryOpen
  , runLoaded
  , rollsNeeded
  , pair
  , PNCounter
  , pnIncrement
  , decrement
  , pnValue
  , totalRewardTool
  , applyOpOneEach
  ) where

import Control.Concurrent.Async (concurrently)

import qualified Data.Map.Strict as Map

import qualified Data.Text as T

import Modern.BoardMcp (loadBoard)
import Modern.Crdt (GCounter (..), counterValue, increment)
import Modern.McpKit (Tool (..), ToolResult (..))
import qualified Modern.QuestBoard as QB
import Modern.Effects (Quest (..))
import Modern.Elements (Element (..), beats)
import Modern.Scouts (Report, scout)

-- ANCHOR: element6
-- 🌗 "Language map" step 2: light and dark beat each other.
-- 「言語の地図」ステップ2：光と闇は、おたがいに強い
data Element6 = Basic Element | Light | Dark
  deriving (Show, Eq)

beats6 :: Element6 -> Element6 -> Bool
beats6 (Basic a) (Basic d) = beats a d
beats6 Light     Dark      = True
beats6 Dark      Light     = True
beats6 _         _         = False
-- ANCHOR_END: element6

-- ANCHOR: notBoth
-- ✅ "Language map" step 3: the Lean theorem as a property.
--    Two elements never beat each other both ways.
-- 「言語の地図」ステップ3：Lean の定理を、性質として。
-- 2つの属性が、おたがいに強いことはない
notBoth :: Element -> Element -> Bool
notBoth a d = not (beats a d && beats d a)
-- ANCHOR_END: notBoth

-- ANCHOR: door
-- 🚪 "ADTs everywhere" step 2: only a locked door has a key name.
-- 「代数的データ型」ステップ2：鍵の名前を持つのは、鍵のかかった扉だけ
data Door = Open | Closed | Locked String
  deriving (Show, Eq)

-- 🗝️ Try to open with a key. The right key opens a locked door.
-- 鍵で開けてみる。合う鍵なら、鍵のかかった扉も開く
tryOpen :: String -> Door -> Door
tryOpen _   Open          = Open
tryOpen _   Closed        = Open
tryOpen key (Locked need)
  | key == need = Open
  | otherwise   = Locked need
-- ANCHOR_END: door

-- ANCHOR: loaded
-- 🎲 "Effects" step 2: a loaded die that always shows its top face.
-- 「エフェクト」ステップ2：いつもいちばん大きい目が出る、いかさまサイコロ
runLoaded :: Quest a -> ([String], a)
runLoaded (Done x)      = ([], x)
runLoaded (Roll n k)    = runLoaded (k n)
runLoaded (Say t after) =
  let (rest, x) = runLoaded after
  in (t : rest, x)
-- ANCHOR_END: loaded

-- ANCHOR: rollsNeeded
-- 🔢 "Effects" step 3: how many rolls, with the loaded die?
-- 「エフェクト」ステップ3：いかさまサイコロで、何回振る？
rollsNeeded :: Quest a -> Int
rollsNeeded (Done _)      = 0
rollsNeeded (Roll n k)    = 1 + rollsNeeded (k n)
rollsNeeded (Say _ after) = rollsNeeded after
-- ANCHOR_END: rollsNeeded

-- ANCHOR: pair
-- 🤝 "Structured concurrency" step 2: two scouts at once, as a pair.
-- 「構造化された並行性」ステップ2：2人を同時に送り出して、組で受け取る
pair :: Report -> IO (String, String)
pair report =
  concurrently (scout report "slime" 50) (scout report "goblin" 100)
-- ANCHOR_END: pair

-- ANCHOR: pn
-- ➕➖ "CRDT" step 2: a counter that can go down, made of two
--    counters that only grow.
-- 「CRDT」ステップ2：減らせるカウンター。増えるだけのカウンター2つで作る
data PNCounter = PNCounter GCounter GCounter
  deriving (Show, Eq)

instance Semigroup PNCounter where
  PNCounter p1 n1 <> PNCounter p2 n2 = PNCounter (p1 <> p2) (n1 <> n2)

instance Monoid PNCounter where
  mempty = PNCounter (GCounter Map.empty) (GCounter Map.empty)

pnIncrement :: String -> PNCounter -> PNCounter
pnIncrement dev (PNCounter p n) = PNCounter (increment dev p) n

decrement :: String -> PNCounter -> PNCounter
decrement dev (PNCounter p n) = PNCounter p (increment dev n)

pnValue :: PNCounter -> Int
pnValue (PNCounter p n) = counterValue p - counterValue n
-- ANCHOR_END: pn

-- ANCHOR: totalReward
-- 💰 "Growing an MCP server" step 2: one more tool, the total reward
--    of the open quests. Only the list of tools changes.
-- 「MCP サーバーを育てる」ステップ2：募集中の依頼の報酬の合計を答える道具。
-- 変わるのは、道具のリストだけ
totalRewardTool :: FilePath -> Tool
totalRewardTool path =
  Tool
    { toolName        = T.pack "total_reward"
    , toolDescription = T.pack "The total reward of the open quests, in gold."
    , toolArguments   = []
    , toolRequired    = []
    , toolRun         = \_ -> do
        loaded <- loadBoard path
        pure $ case loaded of
          Left err    -> Refuse err
          Right board ->
            let open = [ QB.questReward q
                       | q <- QB.quests board
                       , QB.questStage q == QB.Open ]
            in Answer (T.pack (show (sum open) <> " G"))
    }
-- ANCHOR_END: totalReward

-- ANCHOR: oneEach
-- 🧍 "Growing an MCP server" step 3: an adventurer takes one quest at a
--    time. A new rule goes into the pure core, in front of applyOp.
-- 「MCP サーバーを育てる」ステップ3：1人が引き受けられるのは、一度に1つまで。
-- 新しい決まりは、純粋な核の applyOp の手前に置く
applyOpOneEach :: QB.BoardOp -> QB.Board -> Either T.Text QB.Board
applyOpOneEach op board =
  case op of
    QB.Take _ who
      | any (busyWith (T.strip who)) (QB.quests board) ->
          -- 「(名前) は、もう別の依頼を引き受けています」
          Left (T.strip who <> T.pack " already has a quest")
    _ -> QB.applyOp op board
  where
    busyWith who q = QB.questStage q == QB.TakenBy who
-- ANCHOR_END: oneEach
-- ANCHOR_END: all
