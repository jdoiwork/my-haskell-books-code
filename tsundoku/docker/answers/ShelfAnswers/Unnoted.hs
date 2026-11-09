-- ANCHOR: all
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
-- | Training: a tool that says which entries have no note yet. It looks
--   only, so it takes a Glance; the read-only server can have it too.
-- トレーニング：まだ感想を書いていない項目を言う道具。見るだけなので、
-- もらうのは Glance。見るだけのサーバーにも入れられる
module ShelfAnswers.Unnoted (unnotedTool, unnoted) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Data.Text qualified as T

import Tsundoku.Api.Json (CardDetail (..), NoteItem (..), TocItem (..),
                          detailOf)
import Tsundoku.Mcp.Wire
import Tsundoku.Shelf (Glance (..))
import Tsundoku.Types

-- 📝 The headings of the entries with no note, from the top.
-- 感想のない項目の見出しを、上から順に
unnoted :: CardDetail -> [Text]
unnoted c = [ i.heading | i <- flat c.toc, Just i.id `notElem` written ]
  where
    written = map (.entry) c.notes
    flat    = concatMap (\i -> i : flat i.children)

-- 🧰 The tool: unnoted_entries {id}.
-- 道具：unnoted_entries {id}
unnotedTool :: Glance -> Tool
unnotedTool g = Tool "unnoted_entries" "まだ感想を書いていない項目の見出し"
  (object [ "type" .= ("object" :: Text)
          , "properties" .= object
              ["id" .= object ["type" .= ("integer" :: Text)]]
          , "required" .= (["id"] :: [Text]) ])
  $ \args -> case arg "id" args of
      Nothing -> pure (Refuse "id がいります")
      Just n  -> do
        found <- g.getCard (WorkId n)
        pure $ case found of
          Nothing   -> Refuse "本棚にない作品です"
          Just card ->
            Answer (T.unlines (unnoted (detailOf (WorkId n, card))))
-- ANCHOR_END: all
