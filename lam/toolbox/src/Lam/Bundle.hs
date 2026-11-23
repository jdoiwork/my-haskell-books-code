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
  , includedFiles
  , withIncluded
  ) where

import Control.Exception (IOException, try)
import Control.Monad (filterM, forM, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, modifyIORef')
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (readHex, showHex)
import System.Directory
  ( Permissions (..), doesDirectoryExist, doesFileExist, getPermissions
  , listDirectory, setPermissions )
import System.Environment (getExecutablePath)
import System.FilePath (normalise, splitDirectories, (</>))
import System.IO
  (Handle, IOMode (..), SeekMode (..), hFileSize, hSeek, withBinaryFile)
import Text.Read (readMaybe)

import Lam.Builtins (con)
import Lam.Eval
import Lam.Io (Handler)
import Lam.Module (Files (..))
import Lam.World (cleanPath, textOf)
-- ANCHOR_END: head

-- ANCHOR: bundle
-- 📦 What goes at the end: the file to run, the flags of lam baked in
--    (--allow-net=localhost:8080, ...), the files the program read
--    when it was compiled (by the paths it asked for), the folders of
--    LAM_PATH then (so the same paths are asked again), the commands
--    lam.json allowed on a dry run, and the names and sizes of the
--    files of --include. Their bytes come after the bundle, as they
--    are: Haskell's show would make an image four times as big.
-- 最後に付けるもの：動かすファイル、焼きこむ lam のフラグ、コンパイルの
-- ときにプログラムが読んだファイル（頼んだときの道で）、そのときの
-- LAM_PATH のフォルダー（同じ道を、もう一度頼むように）、lam.json が
-- 試しに動かすときに許したコマンド、--include のファイルの名前と大きさ。
-- そのバイト列は、包みのあとにそのまま付ける（Haskell の show では、
-- 画像が4倍の大きさになる）
data Bundle = Bundle
  { bundleMain     :: FilePath
  , bundleFlags    :: [String]
  , bundleSources  :: [(FilePath, Text)]
  , bundlePath     :: [FilePath]
  , bundleAllowed  :: [[Text]]
  , bundleIncluded :: [(FilePath, Int)]
  } deriving (Eq, Show, Read)
-- ANCHOR_END: bundle

-- ANCHOR: trailer
-- 🏷️ The end of a program file: the bundle (Haskell's show, as UTF-8)
--    with its size in 16 hex digits in front, the bytes of the
--    included files, the size of all that in 16 hex digits, and a
--    mark. lam alone has no mark. The 2 in the mark: the second shape
--    (the first one had no included files).
-- 実行ファイルの最後：包み（Haskell の show を UTF-8 で。前に、その
-- 大きさを16桁の16進数で）、入れたファイルのバイト列、ここまでの大きさを
-- 16桁の16進数で、そして印。ただの lam には、印がない。印の 2 は2つ目の
-- 形（1つ目には、入れたファイルがなかった）
mark :: B.ByteString
mark = "\n#lam-bundle-2\n"

trailerSize :: Int
trailerSize = 16 + B.length mark

hex16 :: Int -> B.ByteString
hex16 n = let h = showHex n "" in BC.pack (replicate (16 - length h) '0' <> h)

readHex16 :: B.ByteString -> Maybe Int
readHex16 bytes = case readHex (BC.unpack bytes) of
  [(n, "")] | B.length bytes == 16 -> Just n
  _                                -> Nothing

-- 📦 The bundle and the included files, as bytes. The bundle keeps the
--    names and the sizes; the bytes follow, in the same order.
-- 包みと入れたファイルを、バイト列に。包みは名前と大きさを持ち、
-- バイト列は同じ順で後ろに続く
encodeBundle :: Bundle -> [(FilePath, B.ByteString)] -> B.ByteString
encodeBundle b files =
  let named = b { bundleIncluded = [ (p, B.length x) | (p, x) <- files ] }
      front = TE.encodeUtf8 (T.pack (show named))
      payload = hex16 (B.length front) <> front <> B.concat (map snd files)
  in payload <> hex16 (B.length payload) <> mark

-- 🔍 The bundle at the end of these bytes, if there is one, with the
--    included files, and how many bytes it all takes.
-- このバイト列の最後にある包み（あれば）と入れたファイル、全部で何
-- バイトか
decodeBundle
  :: B.ByteString -> B.ByteString
  -> Maybe (Bundle, [(FilePath, B.ByteString)], Int)
decodeBundle tailBytes payload
  | B.length tailBytes /= trailerSize = Nothing
  | B.drop 16 tailBytes /= mark = Nothing
  | otherwise = do
      size <- readHex16 (B.take 16 tailBytes)
      when (B.length payload /= size) Nothing
      frontSize <- readHex16 (B.take 16 payload)
      let (front, rest) = B.splitAt frontSize (B.drop 16 payload)
      b <- readMaybe (T.unpack (TE.decodeUtf8 front))
      files <- cut rest (bundleIncluded b)
      Just (b, files, size + trailerSize)
  where
    cut bytes named = case named of
      [] | B.null bytes -> Just []
      [] -> Nothing
      (p, n) : more -> do
        let (x, rest) = B.splitAt n bytes
        when (B.length x /= n) Nothing
        ((p, x) :) <$> cut rest more
-- ANCHOR_END: trailer

-- ANCHOR: readOwn
-- 🪞 Read the end of a program file: only the last bytes, not all of
--    it (lam is big). Nothing if there is no bundle.
-- 実行ファイルの最後を読む：最後のバイトだけで、全部は読まない（lam は
-- 大きい）。包みがなければ Nothing
readBundleOf
  :: FilePath -> IO (Maybe (Bundle, [(FilePath, B.ByteString)], Int))
readBundleOf path = do
  found <- try $ withBinaryFile path ReadMode $ \h -> do
    total <- hFileSize h
    if total < toInteger trailerSize then pure Nothing else do
      tailBytes <- readAt h (total - toInteger trailerSize) trailerSize
      case readHex16 (B.take 16 tailBytes) of
        Just size | B.drop 16 tailBytes == mark
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
readOwnBundle :: IO (Maybe (Bundle, [(FilePath, B.ByteString)]))
readOwnBundle = do
  me <- getExecutablePath
  fmap (\(b, files, _) -> (b, files)) <$> readBundleOf me
-- ANCHOR_END: readOwn

-- ANCHOR: writeProgram
-- ✍️ Write a program file: lam (without a bundle it may have, when
--    lam compile is run by a compiled program), then the new bundle.
--    It may be run, as lam may.
-- 実行ファイルを書く：lam（コンパイルしたプログラムが lam compile を
-- 動かしたときは、付いている包みを外す）、それから新しい包み。lam と
-- 同じく、動かしてよいファイルにする
writeProgram :: FilePath -> Bundle -> [(FilePath, B.ByteString)] -> IO Int
writeProgram out b files = do
  me <- getExecutablePath
  old <- readBundleOf me
  lam <- B.readFile me
  let plain = B.take (B.length lam - maybe 0 (\(_, _, n) -> n) old) lam
      bytes = plain <> encodeBundle b files
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

-- ANCHOR: includedFiles
-- 📥 lam compile --include public: the files under a folder (or one
--    file), each with its path cleaned (public/./a.png is
--    public/a.png). Left: the path is not there.
-- フォルダーの下のファイル（か、ファイル1つ）。道はきれいにする
-- （public/./a.png は public/a.png）。Left なら、その道はない
includedFiles :: FilePath -> IO (Either Text [(FilePath, B.ByteString)])
includedFiles top = do
  isFile <- doesFileExist top
  isDir  <- doesDirectoryExist top
  if isFile then Right . pure <$> one top
  else if isDir then Right <$> (walk top >>= traverse one)
  else pure (Left (T.pack top <> ": no such file or folder"))
  where
    one p = (,) (T.unpack (cleanPath (T.pack p))) <$> B.readFile p
    walk dir = do
      names <- listDirectory dir
      let paths = map (dir </>) (filter (\n -> take 1 n /= ".") names)
      files <- filterM doesFileExist paths
      dirs  <- filterM doesDirectoryExist paths
      deeper <- forM dirs walk
      pure (files <> concat deeper)
-- ANCHOR_END: includedFiles

-- ANCHOR: withIncluded
-- 🗂️ The included files, put in front of the disk: the program reads
--    them from inside itself. readFile and File.readBytes give them,
--    File.exists and File.isDir see them (and their folders). The
--    guards of the flags are put around this, so --allow-read=public
--    is still needed, as without lam compile. Anything else goes on
--    to the disk.
-- 入れたファイルを、ディスクの前に置く。プログラムは自分の中から読む。
-- readFile と File.readBytes が渡し、File.exists と File.isDir が
-- 見つける（そのフォルダーも）。フラグの見張りは、この外にかぶせるので、
-- lam compile しないときと同じく --allow-read=public が要る。ほかの
-- 頼みごとは、ディスクへ
withIncluded :: [(FilePath, B.ByteString)] -> Handler IO -> Handler IO
withIncluded files handle request = case request of
  ReadFile p   | Just b <- found p -> pure (textOf p b)
  ReadBytes p  | Just b <- found p -> pure (con "Ok" [VBytes b])
  Exists p     | inside p          -> pure (VBool True)
  IsDir p      | folder p          -> pure (VBool True)
  _                                -> handle request
  where
    table = Map.fromList files
    found p = Map.lookup (T.unpack (cleanPath p)) table
    inside p = maybe (folder p) (const True) (found p)
    folder p =
      let pieces = splitDirectories (normalise (T.unpack (cleanPath p)))
      in any (\(f, _) -> pieces `isFolderOf` splitDirectories f) files
    isFolderOf a b = length a < length b && take (length a) b == a
-- ANCHOR_END: withIncluded
-- ANCHOR_END: all
