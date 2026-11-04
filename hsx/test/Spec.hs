{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Hsx
import Hsx.Parse

-- 🚨 The error of a template, if any.
-- テンプレートのエラー（あれば）
errorOf :: String -> Maybe ParseError
errorOf src = case parseHsx src of
  Left e  -> Just e
  Right _ -> Nothing

main :: IO ()
main = hspec $ do
  -- 「テンプレートを読む」
  describe "parseHsx" $ do
    -- 「タグと穴を木にする」
    it "reads tags and holes into a tree" $
      parseHsx "<p class=\"a\">Hi, {name}!</p>"
        `shouldBe` Right
          [ Element "p" [Attr "class" (Static "a")]
              [TextNode "Hi, ", Splice ["name"], TextNode "!"] ]

    -- 「閉じタグのまちがいを、位置付きで知らせる」
    it "reports a wrong closing tag with its place" $
      fmap renderError (errorOf "<p>\n  <b>x</p>")
        `shouldBe` Just
          ( "hsx: line 2, column 7: </p> does not match <b>"
              <> " opened at line 2, column 3" )

    -- 「閉じていないタグを知らせる」
    it "reports a tag that is never closed" $
      fmap errMessage (errorOf "<ul><li>a</li>")
        `shouldBe` Just "<ul> opened at line 1, column 1 is never closed"

    -- 「JSX と同じように空白を削る」
    it "trims whitespace like JSX" $
      parseHsx "<p>\n   Hello,\n   world\n</p>"
        `shouldBe` Right [Element "p" [] [TextNode "Hello, world"]]

  -- 「HTML を組み立てる」
  describe "hsx" $ do
    -- 「穴に値を入れる」
    it "fills holes" $ do
      let who = "Lambda" :: Text
          lv  = 12 :: Int
      renderHtml [hsx|<p>{who} (Lv {lv})</p>|]
        `shouldBe` "<p>Lambda (Lv 12)</p>"

    -- 「穴の中の文字はエスケープされる」
    it "escapes text in holes" $ do
      let evil = "<script>alert(1)</script>" :: Text
      renderHtml [hsx|<td>{evil}</td>|]
        `shouldBe` "<td>&lt;script&gt;alert(1)&lt;/script&gt;</td>"

    -- 「属性の値もエスケープされる」
    it "escapes attribute values" $ do
      let cls = "x\" onclick=\"steal()" :: Text
      renderHtml [hsx|<div class={cls}></div>|]
        `shouldBe` "<div class=\"x&quot; onclick=&quot;steal()\"></div>"

    -- 「関数の呼び出しも書ける」
    it "can call a function in a hole" $ do
      let xs = [1, 2, 3] :: [Int]
      renderHtml [hsx|<p>{length xs} items</p>|]
        `shouldBe` "<p>3 items</p>"

    -- 「Html のリストと Maybe を埋め込める」
    it "embeds lists and Maybe" $ do
      let items = [ [hsx|<li>{n}</li>|] | n <- ["a", "b" :: Text] ]
          none  = Nothing :: Maybe Html
      renderHtml [hsx|<ul>{items}{none}</ul>|]
        `shouldBe` "<ul><li>a</li><li>b</li></ul>"

    -- 「空要素と、値のない属性」
    it "writes void elements and bare attributes" $
      renderHtml [hsx|<p>a<br>b<input type="checkbox" checked/></p>|]
        `shouldBe` "<p>a<br>b<input type=\"checkbox\" checked></p>"

  -- 「エスケープ」
  describe "escapeText" $
    -- 「どんな文字でも、エスケープした後にタグの文字は残らない」
    prop "leaves no tag characters" $ \s ->
      let out = escapeText (T.pack s)
      in T.any (`elem` ("<>\"'" :: String)) out `shouldBe` False
