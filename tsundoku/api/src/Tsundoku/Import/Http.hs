-- ANCHOR: all
-- ANCHOR: header
-- | Asking a web API, with http-client. Every way it can go wrong comes
--   back as a value, not as an exception.
-- http-client で、Web の API に聞く。うまくいかない形は、どれも例外では
-- なく、値で返す
module Tsundoku.Import.Http (getUrl) where

import Control.Exception (try)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client
  (HttpException (..), Manager, httpLbs, parseRequest, requestHeaders,
   responseBody, responseStatus)
import Network.HTTP.Types (statusCode)

import Tsundoku.Import.Types
-- ANCHOR_END: header

-- ANCHOR: getUrl
-- 🌐 GET a URL, and take the body. No network, a wrong address, an
--    answer that is not 200 OK: a value that says which.
-- URL を GET して、中身を受け取る。ネットがない、宛先がまちがっている、
-- 200 OK 以外の答え：どれかを値で返す
getUrl :: Manager -> String -> IO (Either ImportProblem LBS.ByteString)
getUrl manager url = do
  result <- try $ do
    request <- parseRequest url
    httpLbs request { requestHeaders = [("User-Agent", "tsundoku/0.1")] }
            manager
  pure $ case result of
    Left err       -> Left (Offline (brief err))
    Right response -> case statusCode (responseStatus response) of
      200  -> Right (responseBody response)
      code -> Left (Unexpected ("HTTP " <> T.pack (show code)))

-- ✂️ What went wrong, in one line (the exception has the whole request
--    in it too, and that is long).
-- 何がまずかったかを1行で（例外には、頼んだ中身も全部入っていて、長い）
brief :: HttpException -> Text
brief (HttpExceptionRequest _ why) = T.pack (show why)
brief (InvalidUrlException url why) = T.pack (url <> "：" <> why)
-- ANCHOR_END: getUrl
-- ANCHOR_END: all
