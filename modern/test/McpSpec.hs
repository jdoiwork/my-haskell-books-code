{-# LANGUAGE OverloadedStrings #-}
module McpSpec (spec) where

import Data.Aeson (Value, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Test.Hspec
import Test.QuickCheck

import Modern.Chunks
import Modern.Mcp

-- 📨 Send one line and decode the answer.
-- 1行送って、答えを読む
ask :: BL.ByteString -> Maybe Value
ask line = handleLine line >>= decode

spec :: Spec
spec = do
  describe "Modern.Chunks" $ do
    -- ANCHOR: chunkProps
    -- つなげると元どおり（下書きは、余りを捨ててしまう）
    it "the draft loses the rest (expected to fail)" $ expectFailure $
      property $ \(Positive n) xs ->
        concat (chunksDraft n xs) === (xs :: [Int])
    -- つなげると元どおり
    it "concat (chunks n xs) == xs" $ property $ \(Positive n) xs ->
      concat (chunks n xs) === (xs :: [Int])
    -- どの組も n 個以下で、空の組はない
    it "every group has 1 to n items" $ property $ \(Positive n) xs ->
      all (\g -> not (null g) && length g <= n) (chunks n (xs :: [Int]))
    -- ANCHOR_END: chunkProps
  describe "Modern.Mcp" $ do
    -- 版をそろえて、道具があると伝える
    it "initialize" $
      ask "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\
          \\"params\":{\"protocolVersion\":\"2025-06-18\"}}"
        `shouldBe` decode
          "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\
          \{\"protocolVersion\":\"2025-06-18\",\
          \\"capabilities\":{\"tools\":{}},\
          \\"serverInfo\":{\"name\":\"guild-mcp\",\"version\":\"0.1.0\"}}}"
    -- お知らせには答えない
    it "notifications get no answer" $
      handleLine "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
        `shouldBe` Nothing
    -- 道具を呼ぶ
    it "tools/call" $
      ask "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\
          \\"params\":{\"name\":\"element_power\",\
          \\"arguments\":{\"attacker\":\"fire\",\"defender\":\"grass\"}}}"
        `shouldBe` decode
          "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":\
          \{\"content\":[{\"type\":\"text\",\"text\":\"200\"}],\
          \\"isError\":false}}"
    -- 知らない method
    it "unknown method" $
      ask "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"dance\"}"
        `shouldBe` decode
          "{\"jsonrpc\":\"2.0\",\"id\":4,\
          \\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}"
