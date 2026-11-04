-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- ANCHOR: module
module Main (main) where

-- JSON: reading files, and writing them in a pretty shape.
-- JSON：ファイルを読む道具と、きれいな形で書く道具
import Data.Aeson (eitherDecodeFileStrict, (.=))
import Data.Aeson.Encode.Pretty
  ( Config (..)
  , defConfig
  , encodePretty'
  , keyOrder
  )
import qualified Data.ByteString.Lazy as BL

-- Text.
-- テキスト
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

-- Arguments, files, and stopping with an error.
-- 引数、ファイル、エラーで止まる
import System.Directory   (doesFileExist)
import System.Environment (getArgs)
import System.Exit        (die)
import Text.Read          (readMaybe)

-- The bestiary and the JSON log.
-- 図鑑と JSON のログ
import JsonLog (LogLevel (..), logJson)
import Monster (Element (..), Monster (..))
-- ANCHOR_END: module

-- ANCHOR: main
-- 🚪 bestiary list FILE
--    bestiary add  FILE NAME HP ELEMENT
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["list", path] ->
      listCommand path
    ["add", path, newName, hpText, elementText] ->
      addCommand path newName hpText elementText
    _ ->
      usage
-- ANCHOR_END: main

-- ANCHOR: listCommand
-- 📋 bestiary list FILE: show every monster in the bestiary.
-- bestiary list FILE：図鑑のモンスターを全部表示する
listCommand :: FilePath -> IO ()
listCommand path = do
  monsters <- loadMonsters path
  mapM_ (TIO.putStrLn . formatMonster) monsters
-- ANCHOR_END: listCommand

-- ANCHOR: addCommand
-- ➕ bestiary add FILE NAME HP ELEMENT: add a monster to the bestiary.
-- bestiary add FILE NAME HP ELEMENT：図鑑にモンスターを足す
addCommand :: FilePath -> String -> String -> String -> IO ()
addCommand path newName hpText elementText = do
  monster <- case makeMonster newName hpText elementText of
    Right m  -> pure m
    Left err -> die err
  monsters <- loadMonsters path
  saveMonsters path (monsters ++ [monster])
-- ANCHOR: logAdd
  -- 「モンスターを追加した」
  logJson Info "monster added"
    [ "name"  .= monster.name
    , "count" .= (length monsters + 1)
    ]
-- ANCHOR_END: logAdd
-- ANCHOR_END: addCommand

-- ANCHOR: usage
-- ❓ Wrong arguments: show how to use bestiary, and stop.
-- 引数がおかしい：bestiary の使い方を出して止まる
usage :: IO ()
usage =
  -- 「使い方：……」
  die "usage: bestiary list FILE | bestiary add FILE NAME HP ELEMENT"
-- ANCHOR_END: usage

-- ANCHOR: load
-- 📖 Read the bestiary. No file yet means an empty bestiary.
--    A broken file stops the program with aeson's message.
-- 図鑑を読む。ファイルがまだなければ、空の図鑑
-- 壊れたファイルなら、aeson のメッセージを出して止まる
loadMonsters :: FilePath -> IO [Monster]
loadMonsters path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      result <- eitherDecodeFileStrict path
      case result of
        Right monsters -> pure monsters
        -- 「（ファイル名）が読めません：（理由）」
        Left err       -> die ("cannot read " ++ path ++ ": " ++ err)
-- ANCHOR_END: load

-- ANCHOR: save
-- 💾 Write the bestiary, pretty, with "name" first.
-- 図鑑を書く。きれいに整えて、"name" を先頭に
saveMonsters :: FilePath -> [Monster] -> IO ()
saveMonsters path monsters =
  let order  = keyOrder ["name", "hp", "element", "drops"]
      config = defConfig { confCompare = order }
  in  BL.writeFile path (encodePretty' config monsters <> "\n")
-- ANCHOR_END: save

-- ANCHOR: make
-- 🛠️ Make a monster from the command-line arguments.
-- コマンドラインの引数から、モンスターを作る
makeMonster :: String -> String -> String -> Either String Monster
makeMonster newName hpText elementText =
  case (readMaybe hpText, parseElement elementText) of
    -- 「HP は数で書いてください」
    (Nothing, _) -> Left ("HP must be a number: " ++ hpText)
    -- 「属性は Fire、Water、Grass のどれかです」
    (_, Nothing) -> Left ("element must be Fire, Water or Grass: "
                          ++ elementText)
    (Just n, Just e) ->
      Right Monster { name = T.pack newName, hp = n, element = e, drops = [] }

-- 🔥 Read an element from its name.
-- 名前から属性を読む
parseElement :: String -> Maybe Element
parseElement "Fire"  = Just Fire
parseElement "Water" = Just Water
parseElement "Grass" = Just Grass
parseElement _       = Nothing
-- ANCHOR_END: make

-- ANCHOR: format
-- 📋 One line for a monster: Slime   HP 30  Water  [jelly]
-- モンスター1匹ぶんの行
formatMonster :: Monster -> Text
formatMonster m =
  T.justifyLeft 8 ' ' m.name
    <> "HP " <> T.justifyLeft 4 ' ' (T.pack (show m.hp))
    <> T.justifyLeft 7 ' ' (T.pack (show m.element))
    <> "[" <> T.intercalate ", " m.drops <> "]"
-- ANCHOR_END: format
-- ANCHOR_END: all
