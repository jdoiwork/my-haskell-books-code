{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The quest board that an AI chat will use: the types and the pure core.
-- AI のチャットから使う依頼板：型と、純粋な核
module Modern.QuestBoard
  ( Stage (..)
  , Quest (..)
  , Board
  , emptyBoard
  , quests
  , BoardOp (..)
  , applyOp
  , applyAll
  , describeBoard
  ) where

-- ANCHOR: imports
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
-- ANCHOR_END: imports

-- ANCHOR: types
-- 🚦 Where a quest is.
-- 依頼が、いまどこにあるか
data Stage
  = Open
  | TakenBy Text
  | DoneBy Text
  deriving stock (Eq, Show)

-- 📜 One quest on the board.
-- 依頼板の依頼1つ
data Quest = Quest
  { questId     :: Int
  , questTitle  :: Text
  , questReward :: Int
  , questStage  :: Stage
  }
  deriving stock (Eq, Show)

-- 📋 The board: quests by number.
-- 依頼板：番号ごとの依頼
newtype Board = Board (Map Int Quest)
  deriving stock (Eq, Show)

emptyBoard :: Board
emptyBoard = Board Map.empty

quests :: Board -> [Quest]
quests (Board qs) = Map.elems qs
-- ANCHOR_END: types

-- ANCHOR: op
-- ✉️ What a chat can ask the board to do.
-- チャットが依頼板に頼めること
data BoardOp
  = Post Text Int
  | Take Int Text
  | Finish Int
  deriving stock (Eq, Show)
-- ANCHOR_END: op

-- ANCHOR: apply
-- ⚙️ Apply one op. A refusal says why, in words the AI can read.
-- Op を1つ当てはめる。断るときは、AI が読める言葉で理由を返す
applyOp :: BoardOp -> Board -> Either Text Board
applyOp op (Board qs) =
  case op of
    Post title reward
      -- 「題が空です」
      | T.null (T.strip title) -> Left "the title is empty"
      -- 「報酬は 0 以上にしてください」
      | reward < 0             -> Left "the reward must be 0 or more"
      | otherwise              ->
          let newId = 1 + maybe 0 fst (Map.lookupMax qs)
              quest = Quest newId (T.strip title) reward Open
          in Right (put quest)
    Take n who
      -- 「誰が引き受けますか？」
      | T.null (T.strip who) -> Left "who takes it?"
      | otherwise            -> do
          quest <- find n
          case questStage quest of
            Open -> Right (put quest {questStage = TakenBy (T.strip who)})
            -- 「依頼 #n は募集中ではありません」
            _    -> Left (label n <> " is not open")
    Finish n -> do
      quest <- find n
      case questStage quest of
        TakenBy who -> Right (put quest {questStage = DoneBy who})
        -- 「依頼 #n は、まだ誰も引き受けていません」
        Open        -> Left (label n <> " has not been taken yet")
        -- 「依頼 #n は、もう終わっています」
        DoneBy _    -> Left (label n <> " is already done")
  where
    put q = Board (Map.insert (questId q) q qs)
    find n =
      case Map.lookup n qs of
        Just quest -> Right quest
        -- 「依頼 #n はありません」
        Nothing    -> Left ("there is no quest #" <> tshow n)
    label n = "quest #" <> tshow n
-- ANCHOR_END: apply

-- ANCHOR: applyAll
-- 🔁 Apply ops one by one, skipping the refused ones (for the tests).
-- Op を順に当てはめる。断られたものは飛ばす（テスト用）
applyAll :: [BoardOp] -> Board -> Board
applyAll ops board = foldl (\b op -> fromRight b (applyOp op b)) board ops
-- ANCHOR_END: applyAll

-- ANCHOR: describe
-- 🧾 The board as text, for the AI to read.
-- AI が読む、依頼板のテキスト
describeBoard :: Board -> Text
describeBoard board
  -- 「依頼はまだありません」
  | null (quests board) = "No quests yet."
  | otherwise           = T.unlines (map line (quests board))
  where
    line q =
      "#" <> tshow (questId q) <> " [" <> stage (questStage q) <> "] "
        <> questTitle q <> " (" <> tshow (questReward q) <> " G)"
    stage Open          = "open"
    stage (TakenBy who) = "taken by " <> who
    stage (DoneBy who)  = "done by " <> who
-- ANCHOR_END: describe

-- ANCHOR: tshow
-- 🔤 show, but as Text.
-- show の答えを Text にする
tshow :: Show a => a -> Text
tshow = T.pack . show
-- ANCHOR_END: tshow

-- ANCHOR: json
-- 📦 An op from JSON. The "op" field is the name of the MCP tool,
--    so a tool's arguments become an op as they are.
-- JSON から Op を読む。"op" の欄は MCP の道具の名前なので、
-- 道具の引数が、そのまま Op になる
instance FromJSON BoardOp where
  parseJSON = withObject "op" $ \o -> do
    name <- o .: "op"
    case name :: Text of
      "post_quest"   -> Post   <$> o .: "title" <*> o .: "reward"
      "take_quest"   -> Take   <$> o .: "id"    <*> o .: "who"
      "finish_quest" -> Finish <$> o .: "id"
      _              -> fail ("unknown op: " <> T.unpack name)
-- ANCHOR_END: json

-- The board in a file: a list of quests.
-- ファイルの中の依頼板：依頼のリスト
instance ToJSON Board where
  toJSON board = toJSON (map questJson (quests board))
    where
      questJson q =
        object
          [ "id"     .= questId q
          , "title"  .= questTitle q
          , "reward" .= questReward q
          , "stage"  .= stageName (questStage q)
          , "by"     .= stageWho (questStage q)
          ]
      stageName Open        = "open" :: Text
      stageName (TakenBy _) = "taken"
      stageName (DoneBy _)  = "done"
      stageWho Open          = Nothing
      stageWho (TakenBy who) = Just who
      stageWho (DoneBy who)  = Just who

instance FromJSON Board where
  parseJSON v = do
    list <- parseJSON v
    qs <- traverse questFrom list
    pure (Board (Map.fromList [ (questId q, q) | q <- qs ]))
    where
      questFrom :: Object -> Parser Quest
      questFrom o = do
        name <- o .: "stage"
        who  <- o .:? "by"
        stage <- case (name :: Text, who) of
          ("open", _)        -> pure Open
          ("taken", Just w)  -> pure (TakenBy w)
          ("done", Just w)   -> pure (DoneBy w)
          _                  -> fail "a broken stage"
        Quest <$> o .: "id" <*> o .: "title" <*> o .: "reward" <*> pure stage
-- ANCHOR_END: all
