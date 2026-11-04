-- ANCHOR: module
module Swarm where
-- ANCHOR_END: module

-- ANCHOR: import
import Data.Function ((&))
-- ANCHOR_END: import

-- ANCHOR: enemies
-- 👿 The HP of each enemy goblin in the swarm.
-- 敵（ゴブリンの群れ）の、それぞれの HP
enemies :: [Int]
enemies = [30, 5, 80, 12, 45]
-- ANCHOR_END: enemies

-- ANCHOR: hit
-- 💥 Hit one goblin with attack power atk.
--    Its HP never goes below 0.
-- 攻撃力 atk で、ゴブリンを1体攻撃する
-- HP は 0 より小さくならない
hit :: Int -> Int -> Int
hit atk hp = max 0 (hp - atk)
-- ANCHOR_END: hit

-- ANCHOR: countGoblins
-- 📏 Count the goblins, one by one. (recursion)
-- ゴブリンを1体ずつ数える（再帰）
countGoblins :: [Int] -> Int
-- no goblins left: 0
-- もういない：0 体
countGoblins []       = 0
-- one goblin + the rest
-- 1体 + 残り
countGoblins (_ : xs) = 1 + countGoblins xs
-- ANCHOR_END: countGoblins

-- ANCHOR: totalHp
-- 🧮 Add up the HP of all the goblins. (recursion)
-- すべてのゴブリンの HP を合計する（再帰）
totalHp :: [Int] -> Int
-- no goblins: 0
-- いなければ 0
totalHp []       = 0
-- first HP + the rest
-- 先頭の HP + 残りの合計
totalHp (x : xs) = x + totalHp xs
-- ANCHOR_END: totalHp

-- ANCHOR: attackAll
-- 🌊 Hit every goblin in the enemies with the same atk. (map)
-- 群れのすべてのゴブリンを、同じ攻撃力 atk で攻撃する（map）
attackAll :: Int -> [Int] -> [Int]
attackAll atk hps = map (hit atk) hps
-- ANCHOR_END: attackAll

-- ANCHOR: survivors
-- 🧍 Keep only the goblins that are still alive. (filter)
-- まだ生きているゴブリンだけを残す（filter）
survivors :: [Int] -> [Int]
survivors hps = filter isAlive hps

-- ❤️ Is this goblin still alive?
-- このゴブリンはまだ生きている？
isAlive :: Int -> Bool
isAlive hp = hp > 0
-- ANCHOR_END: survivors

-- ANCHOR: totalHpFold
-- 🧮 The same as totalHp, written with foldr.
-- totalHp と同じ。foldr で書いたもの
totalHp' :: [Int] -> Int
totalHp' hps = foldr (+) 0 hps
-- ANCHOR_END: totalHpFold

-- ANCHOR: remainingHp
-- 🔗 Attack the enemies, then add up the HP of the survivors.
-- 群れを攻撃してから、生き残ったゴブリンの HP を合計する
remainingHp :: Int -> [Int] -> Int
remainingHp atk hps =
  let afterAttack = attackAll atk hps      -- attack everyone / 全員を攻撃
      alive       = survivors afterAttack  -- keep the living / 生き残りを残す
  in  sum alive                            -- add them up / 合計する
-- ANCHOR_END: remainingHp

-- ANCHOR: remainingHpWhere
-- 🔗 The same pipeline, naming the steps with where.
-- 同じパイプラインを、where で途中の結果に名前を付けたもの
remainingHpWhere :: Int -> [Int] -> Int
remainingHpWhere atk hps = sum alive                -- add them up / 合計する
  where
    afterAttack = attackAll atk hps     -- attack everyone / 全員を攻撃
    alive       = survivors afterAttack -- keep the living / 生き残りを残す
-- ANCHOR_END: remainingHpWhere

-- ANCHOR: remainingHpComposed
-- 🔗 The same pipeline, built with function composition (.).
-- 同じパイプラインを、関数合成（.）で組み立てたもの
remainingHp' :: Int -> [Int] -> Int
remainingHp' atk = sum . survivors . attackAll atk
-- ANCHOR_END: remainingHpComposed

-- ANCHOR: turnsToClear
-- 🌀 How many attacks does it take to defeat the whole enemies?
-- 群れを全滅させるには、何回攻撃すればいい？
turnsToClear :: Int -> [Int] -> Int
turnsToClear atk hps =
  -- every round, forever
  -- 毎ラウンドの様子（無限に続く）
  let rounds   = iterate (attackAll atk) hps
      -- while someone is alive
      -- 誰かが生きている間だけ
      fighting = takeWhile anyAlive rounds
  in  length fighting
  where
    anyAlive hs = any isAlive hs
-- ANCHOR_END: turnsToClear

-- ANCHOR: remainingHpPiped
-- 🔗 The same pipeline again, flowing from left to right with (&).
-- 同じパイプラインを、(&) で左から右へ流したもの
remainingHp'' :: Int -> [Int] -> Int
remainingHp'' atk hps =
  hps
    & attackAll atk
    & survivors
    & sum
-- ANCHOR_END: remainingHpPiped

-- ANCHOR: pipeOperator
-- 🚰 A pipe operator like the ones in F#, OCaml and Elixir.
-- F#、OCaml、Elixir にあるようなパイプ演算子
(|>) :: a -> (a -> b) -> b
(|>) = (&)

infixl 1 |>
-- ANCHOR_END: pipeOperator

-- ANCHOR: remainingHpPipe
-- 🔗 The same pipeline, written with our own (|>).
-- 同じパイプラインを、自作の (|>) で書いたもの
remainingHp''' :: Int -> [Int] -> Int
remainingHp''' atk hps =
  hps
    |> attackAll atk
    |> survivors
    |> sum
-- ANCHOR_END: remainingHpPipe
