{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | A trial (2026-11-16): cut a text at the matches by calling the C
--   API of PCRE2 directly (Text.Regex.Pcre2.Foreign), instead of asking
--   PCRE2's replacing twice (Lam.Pcre). The C API tells where each match
--   is, so no marks are needed, and the text is searched once.
--   Not used by Lam yet: the tests check that it cuts the same as
--   Lam.Pcre.
-- 試し（2026-11-16）：PCRE2 の C の API（Text.Regex.Pcre2.Foreign）を直接
-- 呼んで、文字列を見つかったところで切る。Lam.Pcre のように置き換えに
-- 2回頼まない。C の API は場所を教えてくれるので、印は要らず、探すのも
-- 1回で済む。Lam はまだ使っていない。テストで、Lam.Pcre と同じに切れる
-- ことを確かめている。
module Lam.PcreFfi
  ( cutterFor
  ) where

import Control.Monad (forM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Foreign as TF
import Foreign
import Foreign.C.Types (CInt)
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.Pcre2.Foreign

import Lam.Pcre (Cut (..))

-- 🛠️ Compile a pattern once, and give back a function that cuts any
--    text at its matches (or what went wrong: a compile error, or an
--    error while matching, like "match limit exceeded").
-- パターンを1回だけコンパイルし、どんな文字列でも見つかったところで
-- 切る関数を返す（まちがいなら、何がまちがいか）
cutterFor :: Text -> Either Text (Text -> Either Text Cut)
cutterFor source = unsafePerformIO $ do
  compiled <- compileCode source
  pure $ case compiled of
    Left err   -> Left err
    Right code -> Right (\subject -> unsafePerformIO (cutWith code subject))

-- 🧱 pcre2_compile, in UTF mode.
-- UTF のモードでコンパイルする
compileCode :: Text -> IO (Either Text (ForeignPtr Pcre2_code))
compileCode source =
  TF.withCStringLen source $ \(ptr, len) ->
    alloca $ \errorCode ->
      alloca $ \errorOffset -> do
        code <- pcre2_compile (castPtr ptr) (fromIntegral len) pcre2_UTF
                  errorCode errorOffset nullPtr
        if code == nullPtr
          then Left <$> (peek errorCode >>= messageOf)
          else Right <$> newForeignPtr pcre2_code_finalizer code

-- ✂️ The loop of pcre2demo.c: search, remember where the match is,
--    and go on from its end. After an empty match, try once more at the
--    same place with NOTEMPTY_ATSTART | ANCHORED; if that fails, step
--    over ONE LETTER (1 to 4 bytes of UTF-8), not one byte.
-- pcre2demo.c と同じループ：探して、場所を覚え、終わりから続ける。
-- 空の一致のあとは、同じ場所でもう一度だけ（空はだめ・ここから）試し、
-- だめなら1文字（UTF-8 の1〜4バイト）進む。1バイトではない
cutWith :: ForeignPtr Pcre2_code -> Text -> IO (Either Text Cut)
cutWith codeFp subject =
  withForeignPtr codeFp $ \code -> do
    matchData <- pcre2_match_data_create_from_pattern code nullPtr
                   >>= newForeignPtr pcre2_match_data_finalizer
    withForeignPtr matchData $ \md ->
      BU.unsafeUseAsCStringLen bytes $ \(ptr, len) -> do
        let subj = castPtr ptr
            -- Text is always good UTF-8, so PCRE2 need not check the
            -- whole text again on every call (that made it 100 times
            -- slower).
            -- Text は必ず正しい UTF-8 なので、呼ぶたびに文字列全体を
            -- 調べなおさなくてよい（調べると、100倍遅かった）
            match offset afterEmpty =
              pcre2_match code subj (fromIntegral len) (fromIntegral offset)
                (pcre2_NO_UTF_CHECK .|. (if afterEmpty then retry else 0))
                md nullPtr
            retry = pcre2_NOTEMPTY_ATSTART .|. pcre2_ANCHORED
            go offset afterEmpty found = do
              rc <- match offset afterEmpty
              if rc == pcre2_ERROR_NOMATCH
                then
                  if not afterEmpty || offset >= len
                    then pure (Right (reverse found))
                    else go (offset + letterBytes offset) False found
                else if rc < 0
                  then Left <$> messageOf rc
                  else do
                    spans <- ovectorOf md
                    let (start, end) = case spans of
                          s : _ -> s
                          []    -> (offset, offset)
                    go end (start == end) (spans : found)
        result <- go 0 False []
        pure (fmap cutOf result)
  where
    bytes = TE.encodeUtf8 subject

    -- How many bytes the letter at this place takes in UTF-8.
    -- この場所の文字が、UTF-8 で何バイトか
    letterBytes at = case B.indexMaybe bytes at of
      Just b | b >= 0xF0 -> 4
             | b >= 0xE0 -> 3
             | b >= 0xC0 -> 2
      _                  -> 1

    slice (from, to)
      | from < 0  = ""                       -- an unset group / 使われなかった
      | otherwise = TE.decodeUtf8 (B.take (to - from) (B.drop from bytes))

    -- The pieces between the matches, and the matches with their groups.
    -- 見つかったものの間の切れ端と、見つかったもの（グループつき）
    cutOf matches =
      let wholes  = [ s | s : _ <- matches ]
          starts  = 0 : map snd wholes
          ends    = map fst wholes ++ [B.length bytes]
          pieces  = map slice (zip starts ends)
          found   = [ slice w :| map slice gs | w : gs <- matches ]
      in Cut pieces found

-- 📍 The (start, end) of the whole match and of each group, in bytes.
--    An unset group is (-1, -1).
-- 全体と、それぞれのグループの（始まり、終わり）。バイトで数える。
-- 使われなかったグループは (-1, -1)
ovectorOf :: Ptr Pcre2_match_data -> IO [(Int, Int)]
ovectorOf md = do
  count  <- pcre2_get_ovector_count md
  vector <- pcre2_get_ovector_pointer md
  forM [0 .. fromIntegral count - 1] $ \i -> do
    from <- peekElemOff vector (2 * i)
    to   <- peekElemOff vector (2 * i + 1)
    pure $ if from == pcre2_UNSET
      then (-1, -1)
      else (fromIntegral from, fromIntegral to)

-- 📣 The message of PCRE2 for an error number.
-- エラーの番号に対する PCRE2 の文
messageOf :: CInt -> IO Text
messageOf code =
  allocaBytes 256 $ \buffer -> do
    n <- pcre2_get_error_message code buffer 256
    if n < 0
      then pure (T.pack ("PCRE2 error " <> show code))
      else TE.decodeUtf8 <$> B.packCStringLen (castPtr buffer, fromIntegral n)
-- ANCHOR_END: all
