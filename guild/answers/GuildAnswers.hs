-- ANCHOR: all
-- Answers for the training of the cabal chapter.
-- cabal の章のトレーニングの解答
module GuildAnswers
  ( spendGold
  ) where

import Guild.Gold (Gold, goldAmount, mkGold)

-- ANCHOR: spendGold
-- 💸 Pay the price from the wallet. Not enough gold: Nothing.
--    We are outside Guild.Gold, so we cannot use the constructor;
--    goldAmount and mkGold are all we need.
-- 財布から代金を払う。足りなければ Nothing
-- Guild.Gold の外なのでコンストラクターは使えないが、
-- goldAmount と mkGold だけで書ける
spendGold :: Gold -> Gold -> Maybe Gold
spendGold price wallet = mkGold (goldAmount wallet - goldAmount price)
-- ANCHOR_END: spendGold
-- ANCHOR_END: all
