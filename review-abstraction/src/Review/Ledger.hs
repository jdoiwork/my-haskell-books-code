-- ANCHOR: pragmas
{-# LANGUAGE TemplateHaskell #-}
-- ANCHOR_END: pragmas
-- ANCHOR: all
-- ANCHOR: module
-- | The guild's ledger: read event lines, apply them, and summarize.
-- ギルドの帳簿：出来事の行を読み、当てはめて、まとめる
module Review.Ledger
  ( Event (..)
  , Validation (..)
  , parseEvent
  , parseAll
  , Ledger (..)
  , gold
  , members
  , emptyLedger
  , applyEvent
  , runEvents
  , Summary (..)
  , summarize
  ) where
-- ANCHOR_END: module

-- ANCHOR: imports
import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Semigroup (Max (..), Sum (..))
import Text.Read (readMaybe)
-- ANCHOR_END: imports

-- ANCHOR: event
-- 📜 What can happen in the guild.
-- ギルドで起きること
data Event
  = Join String          -- a new member / 新しい仲間
  | Reward String Int    -- gold for a quest / 依頼の報酬
  | Fine String Int      -- gold taken away / 罰金
  deriving (Show, Eq)

-- 📋 Every problem, not just the first.
-- 最初のだけでなく、問題を全部
data Validation e a = Invalid e | Valid a
  deriving (Show, Eq)

instance Functor (Validation e) where
  fmap _ (Invalid e) = Invalid e
  fmap f (Valid a)   = Valid (f a)

instance Semigroup e => Applicative (Validation e) where
  pure = Valid
  Invalid a <*> Invalid b = Invalid (a <> b)
  Invalid a <*> Valid _   = Invalid a
  Valid f   <*> v         = fmap f v
-- ANCHOR_END: event

-- ANCHOR: parse
-- 🔍 Step 1: one line to an event. The line number goes into the message.
-- ステップ1：1行を出来事に。行番号を、理由に入れる
parseEvent :: (Int, String) -> Validation [String] Event
parseEvent (lineNo, line) = case words line of
  ["join", who]           -> Valid (Join who)
  ["reward", who, amount] -> Reward who <$> number amount
  ["fine", who, amount]   -> Fine who <$> number amount
  _                       -> problem "unknown event"
  where
    number text = case readMaybe text of
      Just n | n >= 0 -> Valid n
      _               -> problem ("bad amount " ++ text)
    -- It uses lineNo, so under GHC2024 it needs its own type signature.
    -- lineNo を使うので、GHC2024 では型注釈が要る
    problem :: String -> Validation [String] a
    problem why = Invalid ["line " ++ show lineNo ++ ": " ++ why]

-- 📚 Every line. traverse with Validation collects all the problems.
-- 全部の行。Validation で traverse すると、問題が全部集まる
parseAll :: [String] -> Validation [String] [Event]
parseAll lines' = traverse parseEvent (zip [1 ..] lines')
-- ANCHOR_END: parse

-- ANCHOR: ledgerType
-- 💰 The ledger: everyone's gold, and who the members are.
-- 帳簿：みんなのゴールドと、仲間の名前
data Ledger = Ledger
  { _gold    :: Map.Map String Int
  , _members :: Set.Set String
  }
  deriving (Show, Eq)

makeLenses ''Ledger

emptyLedger :: Ledger
emptyLedger = Ledger Map.empty Set.empty
-- ANCHOR_END: ledgerType

-- ANCHOR: ledger
-- ✍️ Step 2: apply one event, with State and lenses.
--    Only members get rewards and fines; gold never goes below 0.
-- ステップ2：出来事を1つ当てはめる（State とレンズで）。
-- 報酬と罰金は仲間だけ。ゴールドは 0 より下がらない
applyEvent :: Event -> State Ledger ()
applyEvent event = case event of
  Join who       -> members %= Set.insert who
  Reward who n   -> onlyMember who (gold . at who . non 0 += n)
  Fine who n     -> onlyMember who (gold . at who . non 0 %= (\g -> max 0 (g - n)))
  where
    -- Read the state first, then decide whether to write.
    -- 先に状態を読んでから、書くかどうかを決める
    onlyMember who change = do
      joined <- use members
      when (Set.member who joined) change

runEvents :: [Event] -> Ledger -> Ledger
runEvents events = execState (mapM_ applyEvent events)
-- ANCHOR_END: ledger

-- ANCHOR: summaryType
-- 📊 Step 3: one foldMap for the total, the richest amount, and how many
--    hold any gold.
-- ステップ3：foldMap を1回で、合計・いちばんのお金持ち・お金を持っている人数
data Summary = Summary { total :: Int, richest :: Int, holders :: Int }
  deriving (Show, Eq)
-- ANCHOR_END: summaryType

-- ANCHOR: summary
summarize :: Ledger -> Summary
summarize ledger =
  let (Sum t, Max r, Sum p) =
        foldMap (\g -> (Sum g, Max g, Sum 1)) (Map.elems (ledger ^. gold))
  in  Summary t (max 0 r) p
-- ANCHOR_END: summary
-- ANCHOR_END: all
