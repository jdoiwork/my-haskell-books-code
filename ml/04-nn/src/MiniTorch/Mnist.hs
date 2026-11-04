-- ANCHOR: all
-- | Read MNIST's IDX files: a small header, then one byte per pixel.
-- MNIST の IDX ファイルを読む：小さな見出しのあとに、1点1バイト
module MiniTorch.Mnist
  ( parseImages
  , parseLabels
  , loadMnist
  ) where

import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Vector.Unboxed as VU

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)

-- ANCHOR: parse
-- 🔢 A 32-bit number stored big-endian at a position.
-- その位置に、大きい桁から並んだ 32 ビットの数
word32At :: ByteString -> Int -> Int
word32At bytes i =
  foldl (\acc k -> (acc `shiftL` 8) .|. fromIntegral (BS.index bytes (i + k)))
        0 [0 .. 3]

-- 🖼️ Images: magic 2051, count, rows, cols, then the pixels.
--    Each pixel 0..255 becomes 0..1; the result is [count, rows * cols].
-- 画像：2051、枚数、行、列、そのあとに点。点は 0〜1 にする
parseImages :: ByteString -> Either String NDArray
parseImages bytes
  | BS.length bytes < 16              = Left "too short for an IDX header"
  | word32At bytes 0 /= 2051          = Left "not an MNIST image file"
  | BS.length bytes /= 16 + n * size  = Left "the file is cut short"
  | otherwise = Right (Np.fromVector [n, size] pixels)
  where
    n      = word32At bytes 4
    size   = word32At bytes 8 * word32At bytes 12
    pixels = VU.generate (n * size)
               (\i -> fromIntegral (BS.index bytes (16 + i)) / 255)

-- 🏷️ Labels: magic 2049, count, then one byte per label.
-- ラベル：2049、個数、そのあとに1つ1バイト
parseLabels :: ByteString -> Either String [Int]
parseLabels bytes
  | BS.length bytes < 8               = Left "too short for an IDX header"
  | word32At bytes 0 /= 2049          = Left "not an MNIST label file"
  | BS.length bytes /= 8 + n          = Left "the file is cut short"
  | otherwise = Right (map fromIntegral (BS.unpack (BS.drop 8 bytes)))
  where
    n = word32At bytes 4
-- ANCHOR_END: parse

-- ANCHOR: load
-- 📂 Read one pair of files: images and labels.
-- 画像とラベルのファイルを1組読む
loadMnist :: FilePath -> FilePath -> IO (Either String (NDArray, [Int]))
loadMnist imageFile labelFile = do
  imageBytes <- BS.readFile imageFile
  labelBytes <- BS.readFile labelFile
  pure ((,) <$> parseImages imageBytes <*> parseLabels labelBytes)
-- ANCHOR_END: load
-- ANCHOR_END: all
