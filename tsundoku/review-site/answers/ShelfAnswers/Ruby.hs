-- ANCHOR: all
-- ANCHOR: head
-- | Training 1: count the characters of a page, leaving out ruby.
-- トレーニング1：ルビを数えずに、ページの字数を数える
module ShelfAnswers.Ruby (charsWithoutRuby) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Text.HTML.TagSoup (Tag (..), parseTags)

import Tsundoku.Import.Types (Chars (..))
-- ANCHOR_END: head

-- ANCHOR: ruby
-- 🔤 The text in <body>, without spaces, and without what is inside
--    <rt> (the ruby) and <rp> (the brackets for readers with no ruby).
-- <body> の中の文字。空白と、<rt>（ルビ）・<rp>（ルビを出せない画面のための
-- かっこ）の中は数えない
charsWithoutRuby :: Text -> Chars
charsWithoutRuby page =
  Chars (sum [ T.length (T.filter (not . isSpace) t)
             | TagText t <- skipRuby (inBody (parseTags page)) ])
  where
    inBody = drop 1 . dropWhile (not . isOpen "body")

    skipRuby (tag : rest)
      | isOpen "rt" tag = skipRuby (after "rt" rest)
      | isOpen "rp" tag = skipRuby (after "rp" rest)
      | otherwise       = tag : skipRuby rest
    skipRuby []         = []

    -- everything after the closing tag / 閉じるタグの後ろ
    after name = drop 1 . dropWhile (not . isClose name)

    isOpen  name (TagOpen n _) = T.toLower n == name
    isOpen  _    _             = False
    isClose name (TagClose n)  = T.toLower n == name
    isClose _    _             = False
-- ANCHOR_END: ruby
-- ANCHOR_END: all
