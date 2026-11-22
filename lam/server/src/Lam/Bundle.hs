-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | lam compile app.lam -o app: one program file, with no lam needed.
--   The file is a copy of lam itself, with a bundle put at its end:
--   the script, the module files it imports, and the flags of lam.
--   When it starts, lam reads its own end; if a bundle is there, it
--   runs that instead of reading its arguments as lam's.
-- lam compile app.lam -o app：lam のいらない、実行ファイル1つ。中身は
-- lam そのものの写しで、最後に包み（バンドル）を付けたもの：スクリプト、
-- それが import するモジュールのファイル、lam のフラグ。動き始めると、
-- lam は自分の最後を読む。包みがあれば、引数を lam のものとして読む
-- 代わりに、包みを動かす
module Lam.Bundle
  ( Bundle (..)
  , encodeBundle
  , decodeBundle
  , trailerSize
  , writeProgram
  , readOwnBundle
  , readBundleOf
  , recording
  , bundleFiles
  ) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, modifyIORef')
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (readHex, showHex)
import System.Directory
  (Permissions (..), getPermissions, setPermissions)
import System.Environment (getExecutablePath)
import System.IO
  (Handle, IOMode (..), SeekMode (..), hFileSize, hSeek, withBinaryFile)
import Text.Read (readMaybe)

import Lam.Module (Files (..))
-- ANCHOR_END: head

-- ANCHOR: bundle
-- 📦 What goes at the end: the file to run, the flags of lam baked in
--    (--allow-net=localhost:8080, ...), the files the program read
--    when it was compiled (by the paths it asked for), and the folders
--    of LAM_PATH then (so the same paths are asked again).
-- 最後に付けるもの：動かすファイル、焼きこむ lam のフラグ、コンパイルの
-- ときにプログラムが読んだファイル（頼んだときの道で）、そのときの
-- LAM_PATH のフォルダー（同じ道を、もう一度頼むように）
data Bundle = Bundle
  { bundleMain    :: FilePath
  , bundleFlags   :: [String]
  , bundleSources :: [(FilePath, Text)]
  , bundlePath    :: [FilePath]
  } deriving (Eq, Show, Read)
-- ANCHOR_END: bundle

-- ANCHOR: trailer
-- 🏷️ The end of a program file: the bundle (Haskell's show, as UTF-8),
--    its size in 16 hex digits, and a mark. lam alone has no mark.
-- 実行ファイルの最後：包み（Haskell の show を UTF-8 で）、その大きさを
-- 16 桁の16進数で、そして印。ただの lam には、印がない
mark :: B.ByteString
mark = "\n#lam-bundle-1\n"

trailerSize :: Int
trailerSize = 16 + B.length mark

encodeBundle :: Bundle -> B.ByteString
encodeBundle b =
  let payload = TE.encodeUtf8 (T.pack (show b))
      size = showHex (B.length payload) ""
  in payload <> BC.pack (replicate (16 - length size) '0' <> size) <> mark

-- 🔍 The bundle at the end of these bytes, if there is one, and how
--    many bytes it takes.
-- このバイト列の最後にある包み（あれば）と、それが何バイトか
decodeBundle :: B.ByteString -> B.ByteString -> Maybe (Bundle, Int)
decodeBundle tailBytes payload
  | B.length tailBytes /= trailerSize = Nothing
  | B.drop 16 tailBytes /= mark = Nothing
  | otherwise = do
      (size, "") <- case readHex (BC.unpack (B.take 16 tailBytes)) of
        [found] -> Just found
        _       -> Nothing
      when (B.length payload /= size) Nothing
      b <- readMaybe (T.unpack (TE.decodeUtf8 payload))
      Just (b, size + trailerSize)
-- ANCHOR_END: trailer

-- ANCHOR: readOwn
-- 🪞 Read the end of a program file: only the last bytes, not all of
--    it (lam is big). Nothing if there is no bundle.
-- 実行ファイルの最後を読む：最後のバイトだけで、全部は読まない（lam は
-- 大きい）。包みがなければ Nothing
readBundleOf :: FilePath -> IO (Maybe (Bundle, Int))
readBundleOf path = do
  found <- try $ withBinaryFile path ReadMode $ \h -> do
    total <- hFileSize h
    if total < toInteger trailerSize then pure Nothing else do
      tailBytes <- readAt h (total - toInteger trailerSize) trailerSize
      case readHex (BC.unpack (B.take 16 tailBytes)) of
        [(size, "")] | B.drop 16 tailBytes == mark
                     , toInteger (size + trailerSize) <= total -> do
          payload <- readAt h (total - toInteger (size + trailerSize)) size
          pure (decodeBundle tailBytes payload)
        _ -> pure Nothing
  pure (either (\(_ :: IOException) -> Nothing) id found)
  where
    readAt :: Handle -> Integer -> Int -> IO B.ByteString
    readAt h at n = hSeek h AbsoluteSeek at >> B.hGet h n

-- 🪞 The bundle at the end of the program running now.
-- いま動いているプログラムの最後にある包み
readOwnBundle :: IO (Maybe Bundle)
readOwnBundle = do
  me <- getExecutablePath
  fmap fst <$> readBundleOf me
-- ANCHOR_END: readOwn

-- ANCHOR: writeProgram
-- ✍️ Write a program file: lam (without a bundle it may have, when
--    lam compile is run by a compiled program), then the new bundle.
--    It may be run, as lam may.
-- 実行ファイルを書く：lam（コンパイルしたプログラムが lam compile を
-- 動かしたときは、付いている包みを外す）、それから新しい包み。lam と
-- 同じく、動かしてよいファイルにする
writeProgram :: FilePath -> Bundle -> IO Int
writeProgram out b = do
  me <- getExecutablePath
  old <- readBundleOf me
  lam <- B.readFile me
  let plain = B.take (B.length lam - maybe 0 snd old) lam
      bytes = plain <> encodeBundle b
  B.writeFile out bytes
  p <- getPermissions out
  setPermissions out p { executable = True }
  pure (B.length bytes)
-- ANCHOR_END: writeProgram

-- ANCHOR: recording
-- 📝 Files that write down each file they gave (to put in the bundle).
-- 渡したファイルを、1つずつ書き留めるファイル（包みに入れるため）
recording :: IORef [(FilePath, Text)] -> Files IO -> Files IO
recording seen files = files { readSource = \path -> do
  found <- readSource files path
  mapM_ (\t -> modifyIORef' seen ((path, t) :)) found
  pure found }

-- 📂 The files of a bundle: only what is in it, with no disk.
-- 包みのファイル：中にあるものだけ。ディスクは見ない
bundleFiles :: Bundle -> Files IO
bundleFiles b = Files
  { readSource = \path -> pure (Map.lookup path table)
  , searchPath = bundlePath b }
  where
    table = Map.fromList (bundleSources b)
-- ANCHOR_END: recording
-- ANCHOR_END: all
