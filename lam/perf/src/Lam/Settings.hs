{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- | lam.json: the settings of a folder. For now, one thing: the
--   commands that may really run on a dry run.
--     {"dryRun": {"allow": ["git log", "git status"]}}
--   lam reads the lam.json next to the script by itself; --settings=FILE
--   gives another file.
-- lam.json：フォルダーの設定。いまは1つだけ：試しに動かすときにも、本当に
-- 動かしてよいコマンド。lam はスクリプトの隣の lam.json を自分で読む。
-- ほかのファイルは --settings=FILE で渡す
module Lam.Settings
  ( readSettings
  , settingsPath
  , loadSettings
  , allowedOnDryRun
  , commandWords
  ) where

import Control.Exception (IOException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.FilePath (takeDirectory, (</>))

import Lam.Eval
-- ANCHOR_END: head

-- ANCHOR: readSettings
-- 📖 The commands allowed on a dry run, read from the bytes of a
--    lam.json. Each one is cut at spaces into words: "git log" is
--    ["git", "log"]. A key lam does not know is a mistake, not
--    something quietly left out: a typo would make a setting vanish.
-- lam.json のバイト列から、試しに動かすときに許すコマンドを読む。どれも
-- 空白で語に切る："git log" は ["git", "log"]。lam の知らないキーは、
-- 黙って飛ばさず、まちがいにする（書きまちがえた設定が消えないように）
readSettings :: FilePath -> B.ByteString -> Either Text [[Text]]
readSettings path bytes = case A.eitherDecodeStrict bytes of
  Left why -> mistake ("not JSON: " <> T.pack why)
  Right (A.Object top) -> do
    known top ["dryRun"] ""
    case KM.lookup "dryRun" top of
      Nothing               -> Right []
      Just (A.Object dry)   -> do
        known dry ["allow"] "dryRun."
        maybe (Right []) allowList (KM.lookup "allow" dry)
      Just _ -> mistake "dryRun must be an object, like {\"allow\": [...]}"
  Right _ -> mistake "needs an object, like {\"dryRun\": {\"allow\": [...]}}"
  where
    -- path is outside: with MonoLocalBinds, the type is written.
    -- path は外のもの。MonoLocalBinds なので、型を書く
    mistake :: Text -> Either Text a
    mistake why = Left ("lam: " <> T.pack path <> ": " <> why)
    -- 「知らないキー "dryrun"（lam が知っているのは "dryRun"）」
    known obj keys at =
      case [ k | k <- KM.keys obj, K.toText k `notElem` keys ] of
      []    -> Right ()
      k : _ -> mistake ("unknown key \"" <> at <> K.toText k
                        <> "\" (lam knows " <> T.intercalate ", "
                        [ "\"" <> at <> k' <> "\"" | k' <- keys ] <> ")")
    allowList v = case v of
      A.Array xs -> traverse entry (V.toList xs)
      _          -> mistake "dryRun.allow must be a list of strings"
    entry v = case v of
      A.String s | not (null (T.words s)) -> Right (T.words s)
      A.String _ -> mistake "dryRun.allow has an empty command"
      _          -> mistake "dryRun.allow must be a list of strings"
-- ANCHOR_END: readSettings

-- ANCHOR: settingsPath
-- 📍 Which file to read: the one of --settings=FILE, or lam.json in
--    the folder of the script (the folder lam runs in, for the REPL
--    and a one-liner). True: it must be there.
-- どのファイルを読むか：--settings=FILE のファイルか、スクリプトの
-- フォルダーの lam.json（REPL とワンライナーなら、lam を動かしている
-- フォルダー）。True なら、なければまちがい
settingsPath :: Maybe FilePath -> Maybe FilePath -> (FilePath, Bool)
settingsPath given script = case (given, fmap takeDirectory script) of
  (Just file, _)                   -> (file, True)
  (Nothing, Just dir) | dir /= "." -> (dir </> "lam.json", False)
  (Nothing, _)                     -> ("lam.json", False)

-- 📖 Read the settings: the file of settingsPath. A lam.json that is
--    not there allows nothing; a file of --settings must be there.
-- 設定を読む：settingsPath のファイル。ない lam.json は何も許さない。
-- --settings のファイルは、なければまちがい
loadSettings :: Maybe FilePath -> Maybe FilePath -> IO (Either Text [[Text]])
loadSettings given script = do
  let (path, must) = settingsPath given script
  found <- try (B.readFile path)
  pure $ case (found, must) of
    (Right bytes, _) -> readSettings path bytes
    (Left (_ :: IOException), False) -> Right []
    (Left _, True) -> Left ("lam: " <> T.pack path <> ": cannot read it")
-- ANCHOR_END: settingsPath

-- ANCHOR: allowedOnDryRun
-- ✅ A command whose words start with one of the allowed lists. The
--    words are the command's own (Lam has no shell), so "git log; rm"
--    is not two commands: it is git with the words "log;" and "rm".
-- 語の列の頭が、許したリストのどれかと同じコマンド。語はコマンドその
-- ものの語（Lam はシェルを通さない）なので、"git log; rm" は2つの
-- コマンドではない。語 "log;" と "rm" を持つ git
allowedOnDryRun :: [[Text]] -> Request -> Bool
allowedOnDryRun allowed request = case commandWords request of
  Just ws -> any (`isPrefixOf` ws) allowed
  Nothing -> False

-- 🐚 The words of a command: its name, then its arguments.
-- コマンドの語：名前、それから引数
commandWords :: Request -> Maybe [Text]
commandWords request = case request of
  RunProcess c args _ -> Just (c : args)
  RunCommand c        -> Just (commandName c : commandArgs c)
  _                   -> Nothing
-- ANCHOR_END: allowedOnDryRun
-- ANCHOR_END: all
