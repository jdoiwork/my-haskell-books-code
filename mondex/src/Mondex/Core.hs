-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- ANCHOR: module
-- 🧠 The pure core of mondex: what each command does to the bestiary.
--    No files, no screen: only values in, values out.
-- mondex の純粋な核：コマンドごとに、図鑑をどう変えるか
-- ファイルも画面も使わない。値を受け取って、値を返すだけ
module Mondex.Core
  ( Outcome (..)
  , runAction
  , listMonsters
  , formatMonster
  ) where

-- Sorting.
-- 並べ替え
import Data.List (sortOn)

-- JSON and text.
-- JSON とテキスト
import Data.Aeson (encode)
import Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

import Mondex.Command
  ( MondexAction (..)
  , MonlistRequest (..)
  , MonsterOrder (..)
  )
import Mondex.Monster (Monster (..))
-- ANCHOR_END: imports
-- ANCHOR_END: module

-- ANCHOR: outcome
-- 📦 What a command leaves behind: the lines to print, and the new
--    bestiary to save (Nothing when the bestiary did not change).
-- コマンドの結果：画面に出す行と、保存する新しい図鑑
-- （図鑑が変わらなかったときは Nothing）
data Outcome = Outcome
  { output  :: [Text]
  , changed :: Maybe [Monster]
  }
  deriving (Show, Eq)
-- ANCHOR_END: outcome

-- ANCHOR: runAction
-- 🧠 Run a command on the bestiary. Left: why it failed.
-- 図鑑にコマンドを実行する。Left：失敗した理由
runAction :: MondexAction -> [Monster] -> Either Text Outcome
runAction cmd monsters =
  case cmd of
    List opts -> Right (listMonsters opts monsters)
    Add monster
      | any (\m -> m.name == monster.name) monsters ->
          -- 「（名前）は、もう図鑑にいるよ」
          Left (monster.name <> " is already in the bestiary")
      | otherwise ->
          -- 「（名前）を追加した」
          Right (Outcome ["added " <> monster.name]
                         (Just (monsters ++ [monster])))
    Remove target
      | any (\m -> m.name == target) monsters ->
          -- 「（名前）を削除した」
          Right (Outcome ["removed " <> target]
                         (Just (filter (\m -> m.name /= target) monsters)))
      | otherwise ->
          -- 「（名前）は図鑑にいないよ」
          Left (target <> " is not in the bestiary")
-- ANCHOR_END: runAction

-- ANCHOR: listMonsters
-- 📋 Pick, sort and show the monsters. The bestiary does not change.
-- モンスターを選んで、並べて、表示する。図鑑は変わらない
listMonsters :: MonlistRequest -> [Monster] -> Outcome
listMonsters opts monsters =
  let picked = case opts.onlyElement of
        Just e  -> filter (\m -> m.element == e) monsters
        Nothing -> monsters
      sorted = case opts.order of
        ByName -> sortOn (\m -> m.name) picked
        ByHp   -> sortOn (\m -> m.hp) picked
      lines' = if opts.asJson
        then [TE.decodeUtf8 (BL.toStrict (encode sorted))]
        else map formatMonster sorted
  in  Outcome lines' Nothing
-- ANCHOR_END: listMonsters

-- ANCHOR: format
-- 🖨️ One line for a monster: Slime    HP 30   Water  jelly
-- モンスター1匹ぶんの行
formatMonster :: Monster -> Text
formatMonster m =
  T.justifyLeft 12 ' ' m.name
    <> "HP " <> T.justifyLeft 5 ' ' (T.pack (show m.hp))
    <> T.justifyLeft 7 ' ' (T.pack (show m.element))
    <> T.intercalate ", " m.drops
-- ANCHOR_END: format
-- ANCHOR_END: all
