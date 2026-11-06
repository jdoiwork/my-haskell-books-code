-- ANCHOR: all
-- ANCHOR: module
module Party where

-- Int8, Int64: integers of a fixed size (8 bits and 64 bits).
-- Int8・Int64：大きさの決まった整数（8 ビットと 64 ビット）
import Data.Int (Int8, Int64)

-- toIntegralSized: convert a number only when it fits.
-- toIntegralSized：入りきるときだけ、数を変換する
import Data.Bits (toIntegralSized)
-- ANCHOR_END: module

-- ANCHOR: levels
-- 🧙 The levels of the party members.
-- パーティーのメンバーのレベル
partyLevels :: [Int]
partyLevels = [12, 15, 9, 20]
-- ANCHOR_END: levels

-- ANCHOR: averageLevel
-- 📊 The average level of the party.
--    sum and length give an Int, but / needs a Fractional type,
--    so both become a Double first.
-- パーティーの平均レベル
-- sum と length は Int を返すが、/ には Fractional の型が要る。
-- だから、どちらも先に Double にする
averageLevel :: [Int] -> Double
averageLevel levels = total / count
  where
    -- 56 → 56.0
    total = fromIntegral (sum levels)
    -- 4 → 4.0
    count = fromIntegral (length levels)
-- ANCHOR_END: averageLevel

-- 📊 The same without names, as people often write it.
-- 同じものを、名前を付けずに（よく見る形）
-- ANCHOR: averageLevelShort
averageLevelShort :: [Int] -> Double
averageLevelShort levels =
  fromIntegral (sum levels) / fromIntegral (length levels)
-- ANCHOR_END: averageLevelShort

-- ANCHOR: expForLevel
-- 📈 The experience points needed for a level. They double at each
--    level, so they soon get bigger than any Int.
-- そのレベルに必要な経験値。レベルごとに倍になるので、
-- すぐに Int では入りきらなくなる
expForLevel :: Int -> Integer
expForLevel level = 100 * 2 ^ level

-- 📈 The same, in Int. It overflows after level 56.
-- 同じものを Int で。レベル 57 からあふれる
expForLevelInt :: Int -> Int
expForLevelInt level = 100 * 2 ^ level
-- ANCHOR_END: expForLevel

-- ANCHOR: criticalDamage
-- ⚔️ The damage of a critical hit: the attack times the rate,
--    with the fraction cut off (49.5 → 49).
-- 会心の一撃のダメージ：攻撃力に倍率をかけて、小数を切り捨てる（49.5 → 49）
criticalDamage :: Int -> Double -> Int
criticalDamage attack rate = floor raw
  where
    -- 33 * 1.5 = 49.5
    raw = fromIntegral attack * rate
-- ANCHOR_END: criticalDamage

-- ANCHOR: addSavedGold
-- 💰 The gold in the save file is an Int64 (64 bits on every computer),
--    and the gold found in a battle is an Int.
-- セーブデータのゴールドは Int64（どのパソコンでも 64 ビット）、
-- 戦いで拾ったゴールドは Int
addSavedGold :: Int64 -> Int -> Int64
addSavedGold saved found = saved + fromIntegral found
-- ANCHOR_END: addSavedGold

-- ANCHOR: saveLevelUnsafe
-- 💾 The old save file keeps a level in one byte (Int8: -128 to 127).
--    A level that does not fit wraps around without a word.
-- 昔のセーブデータは、レベルを1バイト（Int8：-128 〜 127）にしまう。
-- 入りきらないレベルは、何も言わずに一周してしまう
saveLevelUnsafe :: Int -> Int8
saveLevelUnsafe level = fromIntegral level
-- ANCHOR_END: saveLevelUnsafe

-- ANCHOR: saveLevel
-- 💾 Keep the level only when it fits in one byte.
-- 1バイトに入りきるときだけ、レベルをしまう
saveLevel :: Int -> Maybe Int8
saveLevel level = toIntegralSized level
-- ANCHOR_END: saveLevel
-- ANCHOR_END: all
