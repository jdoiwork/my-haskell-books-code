-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE OverloadedRecordDot #-}
-- | Running the terminal screen: real keys become Keys, each Ask is sent
--   to the API with servant-client, and each answer goes back into the
--   model. The only part that touches the terminal and the network.
-- ターミナルの画面を動かす：本物のキーを Key にし、Ask の1つ1つを
-- servant-client で API に聞き、答えをモデルに戻す。端末とネットワークに
-- さわるのは、ここだけ
module Tsundoku.Tui.Run
  ( tuiApp
  , keyOf
  , perform
  ) where

-- IO inside brick, and text
-- brick の中の IO、文字列
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (get, modify, put)
import Data.Char (isDigit)
import Data.Text qualified as T

-- brick and vty, and servant's client
-- brick と vty、servant のクライアント
import Brick (App (..), BrickEvent (..), halt, neverShowCursor)
import Graphics.Vty qualified as Vty
import Servant.Client (ClientEnv, ClientError, ClientM)

import Tsundoku.Api.Client
import Tsundoku.Api.Json
import Tsundoku.Api.Routes
import Tsundoku.Tui.Model
import Tsundoku.Tui.View
-- ANCHOR_END: header

-- ANCHOR: app
-- 🖥️ The brick application: draw the model; on a key, step, then ask
--    what the step said to ask, one by one. At the start, load the shelf.
-- brick のアプリ：モデルを描く。キーが来たら step して、step が言ったことを
-- 1つずつ聞く。始めに本棚を読む
tuiApp :: ClientEnv -> App Model () ()
tuiApp env = App
  { appDraw         = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = onEvent
  , appStartEvent   = askAll [LoadWorks]
  , appAttrMap      = const theme
  }
  where
    onEvent event = case event of
      VtyEvent (Vty.EvKey key _) | Just k <- keyOf key -> do
        (model, asks) <- step k <$> get
        put model
        askAll asks
      _ -> pure ()
    askAll = mapM_ askOne
    askOne Quit = halt
    askOne ask  = do
      answer <- liftIO (perform env ask)
      modify (arrive answer)
-- ANCHOR_END: app

-- ANCHOR: keyOf
-- ⌨️ Real keys, as the Keys the model knows.
-- 本物のキーを、モデルが知っている Key に
keyOf :: Vty.Key -> Maybe Key
keyOf key = case key of
  Vty.KChar 'j'           -> Just Down
  Vty.KDown               -> Just Down
  Vty.KChar 'k'           -> Just Up
  Vty.KUp                 -> Just Up
  Vty.KEnter              -> Just Open
  Vty.KEsc                -> Just Back
  Vty.KChar 'h'           -> Just Back
  Vty.KChar '+'           -> Just More
  Vty.KChar '-'           -> Just Less
  Vty.KChar 'q'           -> Just QuitKey
  Vty.KChar c | isDigit c -> Just (Pick (read [c]))
  _                       -> Nothing
-- ANCHOR_END: keyOf

-- ANCHOR: perform
-- 📮 Ask the API, with the client made from the same records of routes.
--    A wrong route name or a wrong type does not compile.
-- API に聞く。同じ道のレコードから作ったクライアントで。道の名前や型を
-- まちがえると、コンパイルが通らない
perform :: ClientEnv -> Ask -> IO Answer
perform env ask = case ask of
  LoadWorks    -> run Works shelfClient.listWorks
  LoadCard w   -> run id (Card <$> (here w).card <*> (here w).form)
  Happen w e   -> run (const Done) ((here w).happen (Happened e))
  Rate   w p   -> run (const Done) ((here w).rate (Points p))
  Quit         -> pure Done
  where
    here = shelfClient.work
    run :: (a -> Answer) -> ClientM a -> IO Answer
    run done asking = either failed done <$> call env asking

-- 🚫 A refusal with a Problem, or no answer at all.
-- Problem のついた断りか、そもそも答えがない
failed :: ClientError -> Answer
failed e = maybe (Offline (T.pack (take 60 (show e)))) Refused (problemOf e)
-- ANCHOR_END: perform
-- ANCHOR_END: all
