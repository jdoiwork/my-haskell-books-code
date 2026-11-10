-- ANCHOR: module
module BattleLog where

import Data.Function (on)
import Data.List     (groupBy, sortOn, unfoldr)
-- ANCHOR_END: module

-- ANCHOR: damage
-- ⚔️ Damage is attack minus defense, but at least 1.
-- ダメージは攻撃力 − 防御力。ただし最低 1
damage :: Int -> Int -> Int
damage attack defense = max 1 (attack - defense)
-- ANCHOR_END: damage

-- ANCHOR: party
-- 🛡️ The attack power of each party member, and the defense of each enemy.
-- 仲間それぞれの攻撃力と、敵それぞれの防御力
partyAttacks :: [Int]
partyAttacks = [50, 35, 60]

enemyDefenses :: [Int]
enemyDefenses = [20, 40, 10]
-- ANCHOR_END: party

-- ANCHOR: attackResults
-- 🤝 Each member attacks the enemy in front of them. (zipWith)
-- 仲間それぞれが、向かい合った敵を攻撃する（zipWith）
attackResults :: [Int] -> [Int] -> [Int]
attackResults attacks defenses = zipWith damage attacks defenses
-- ANCHOR_END: attackResults

-- ANCHOR: hpLog
-- 📜 The boss's HP at the start of each turn.
--    It keeps showing 0 after the boss is defeated.
-- 各ターンの始めのボスの HP。倒れたあとも 0 が続く
hpLog :: [Int]
hpLog = [100, 70, 40, 10, 0, 0]
-- ANCHOR_END: hpLog

-- ANCHOR: whileAlive
-- 💓 The part of the log while the boss is still alive. (takeWhile)
-- ボスが生きている間のログ（takeWhile）
whileAlive :: [Int] -> [Int]
whileAlive hps = takeWhile (> 0) hps

-- 🪦 The part of the log after the boss is defeated. (dropWhile)
-- ボスが倒れたあとのログ（dropWhile）
afterDefeat :: [Int] -> [Int]
afterDefeat hps = dropWhile (> 0) hps
-- ANCHOR_END: whileAlive

-- ANCHOR: bossHpHistory
-- 🌱 Grow the HP history from the starting HP. (unfoldr)
-- 最初の HP から、HP の移り変わりを育てる（unfoldr）
bossHpHistory :: Int -> Int -> [Int]
bossHpHistory hit startHp = unfoldr step startHp
  where
    step hp
      -- defeated: stop
      -- 倒れた：おしまい
      | hp <= 0   = Nothing
      -- record hp, next is hp - hit
      -- hp を記録し、次は hp - hit
      | otherwise = Just (hp, hp - hit)
-- ANCHOR_END: bossHpHistory

-- ANCHOR: digits
-- 🔢 The digits of a number, from the top. (unfoldr)
-- 数の各桁を、上の桁から並べる（unfoldr）
digits :: Int -> [Int]
digits n = reverse (unfoldr step n)
  where
    -- no more digits
    -- もう桁はない
    step 0 = Nothing
    -- last digit, and the rest
    -- 一の位と、残り
    step k = Just (k `mod` 10, k `div` 10)
-- ANCHOR_END: digits

-- ANCHOR: pairUp
-- 👫 Team up the party two by two for a combo attack:
--    multiply the pair's attack power.
--    A member left alone at the end is dropped. (unfoldr)
-- 仲間を2人ずつ組ませて協力攻撃する。威力は2人の攻撃力を掛け合わせたもの。
-- 最後に1人だけ余ったら、その人は数えない（unfoldr）
pairUp :: [Int] -> [Int]
pairUp attacks = unfoldr step attacks
  where
    -- a pair: combo attack, then the rest
    -- 2人組：協力攻撃して、残りへ
    step (x : y : rest) = Just (x * y, rest)
    -- 0 or 1 left: stop
    -- 残り1人以下：おしまい
    step _              = Nothing
-- ANCHOR_END: pairUp

-- ANCHOR: pairUpKeep
-- 👫 The same combo attack, but a member left alone attacks by themselves.
-- 同じく2人ずつ協力攻撃する。ただし、余った1人は1人で（ふつうに）攻撃する
pairUpKeep :: [Int] -> [Int]
pairUpKeep attacks = unfoldr step attacks
  where
    -- a pair: combo attack, then the rest
    -- 2人組：協力攻撃して、残りへ
    step (x : y : rest) = Just (x * y, rest)
    -- one left: use it as it is
    -- 1人だけ：そのまま使う
    step [x]            = Just (x, [])
    -- nobody left: stop
    -- 誰もいない：おしまい
    step []             = Nothing
-- ANCHOR_END: pairUpKeep

-- ANCHOR: mergePotions
-- 🧪 Merge the first two potions into one,
--    again and again, until one is left.
--    Record each new potion. The seed is the list of potions. (unfoldr)
-- 先頭の2つのポーションを1つに合成し、それを1つになるまで繰り返す。
-- できた新しいポーションを記録する。種はポーションのリスト（unfoldr）
mergePotions :: [Int] -> [Int]
mergePotions potions = unfoldr step potions
  where
    -- merge, and put it back
    -- 合成して、先頭に戻す
    step (x : y : rest) = Just (x + y, (x + y) : rest)
    -- 0 or 1 potion left
    -- 残り1つ以下ならおしまい
    step _              = Nothing
-- ANCHOR_END: mergePotions

-- ANCHOR: potionName
-- 🏷️ Name a potion by its strength. Returns the name and the strength.
-- ポーションの強さで名前を付ける。名前と強さの組を返す
potionName :: Int -> (String, Int)
potionName strength = (name, strength)
  where
    name
      -- 「弱いポーション」
      | strength < 30  = "Weak Potion"
      -- 「ポーション」
      | strength < 60  = "Potion"
      -- 「ハイポーション」
      | strength < 100 = "High Potion"
      -- 「メガポーション」
      | otherwise      = "Mega Potion"
-- ANCHOR_END: potionName

-- ANCHOR: fibonacci
-- 🐚 The Fibonacci numbers: each number is the sum of the two before it.
--    The seed is a list of the two latest numbers. (unfoldr)
-- フィボナッチ数：どの数も、その前の2つの数の和になっている。
-- 種は、直近の2つの数のリスト（unfoldr）
fibonacci :: [Int]
fibonacci = unfoldr step [0, 1]
  where
    -- emit a, next seed is [b, a + b]
    -- a を出し、次の種は [b, a + b]
    step (a : b : _) = Just (a, [b, a + b])
    step _           = Nothing
-- ANCHOR_END: fibonacci

-- ANCHOR: history
-- 📜 The boss's HP, 30 less each turn, from 100. (unfoldr)
-- ボスの HP を、100 から 1ターンに 30 ずつ減らして記録する（unfoldr）
history :: [Int]
history = unfoldr step 100
  where
    step hp
      | hp <= 0   = Nothing
      | otherwise = Just (hp, hp - 30)
-- ANCHOR_END: history

-- ANCHOR: battleLog
-- 📝 Which enemy was defeated on which turn: (turn, name).
-- どのターンにどの敵を倒したか：（ターン, 名前）
battleLog :: [(Int, String)]
battleLog =
  [ (1, "Goblin-A")
  , (1, "Goblin-B")
  , (2, "Goblin-C")
  , (3, "Goblin-D")
  , (3, "Goblin-E")
  ]
-- ANCHOR_END: battleLog

-- ANCHOR: byTurn
-- 🗂️ Group the log entries that have the same turn. (groupBy)
-- 同じターンのログをまとめる（groupBy）
byTurn :: [(Int, String)] -> [[(Int, String)]]
byTurn entries = groupBy sameTurn entries
  where
    sameTurn a b = fst a == fst b
-- ANCHOR_END: byTurn

-- ANCHOR: byTurnOn
-- 🗂️ The same as byTurn, written with `on`.
-- byTurn と同じ。`on` を使って書いたもの
byTurn' :: [(Int, String)] -> [[(Int, String)]]
byTurn' entries = groupBy ((==) `on` fst) entries
-- ANCHOR_END: byTurnOn

-- ANCHOR: byTurnSorted
-- 🗂️ Sort by turn first, so that the same turns are next to each other.
-- 先にターンで並べ替えて、同じターンを隣どうしにしてからまとめる
byTurnSorted :: [(Int, String)] -> [[(Int, String)]]
byTurnSorted entries = groupBy ((==) `on` fst) (sortOn fst entries)
-- ANCHOR_END: byTurnSorted

-- ANCHOR: cumulative
-- 📈 The running total of damage, turn by turn. (scanl)
-- ターンごとの、ダメージの累計（scanl）
cumulativeDamage :: [Int] -> [Int]
cumulativeDamage ds = scanl (+) 0 ds
-- ANCHOR_END: cumulative
