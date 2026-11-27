-- ANCHOR: all
-- | The world of remainders: clocks, fast powers, hashes, and tiny RSA.
-- 剰余の世界：時計、速いべき乗、ハッシュ、小さな RSA
module Math.Modular
  ( hourAfter
  , powMod
  , extendedGcd
  , inverseMod
  , Mod
  , combinationsMod
  , stringHash
  , findPattern
  , rsaEncrypt
  , rsaDecrypt
  ) where

-- ANCHOR: imports
import Data.Char (ord)
import Data.List (isPrefixOf, tails)
-- ANCHOR_END: imports

-- ANCHOR: clock
-- 🕐 The hour n hours after h, on a 24-hour clock. Each is made small
--    first, so a huge n does not overflow.
-- h 時の n 時間後は、何時？（24 時間の時計）。先にそれぞれを小さく
-- してから足すので、とても大きな n でもあふれない
hourAfter :: Int -> Int -> Int
hourAfter h n = (h `mod` 24 + n `mod` 24) `mod` 24
-- ANCHOR_END: clock

-- ANCHOR: powMod
-- 💪 x ^ n mod m, squaring as we go (never a huge number in between).
--    n is 0 or more, and m is 1 or more.
-- x ^ n を m で割った余り。途中でも余りをとるので、大きな数にならない。
-- n は 0 以上、m は 1 以上
powMod :: Integer -> Integer -> Integer -> Integer
powMod x n m
  | m <= 0    = error "powMod: the modulus must be 1 or more"
  | n < 0     = error "powMod: the exponent must be 0 or more"
  | otherwise = go (1 `mod` m) (x `mod` m) n
  where
    go acc b e
      | e == 0    = acc
      | even e    = go acc                 (b * b `mod` m) (e `div` 2)
      | otherwise = go (acc * b `mod` m) b                (e - 1)
-- ANCHOR_END: powMod

-- ANCHOR: inverse
-- 🔁 Euclid, extended: gcd a b (0 or more), with x and y such that
--    a*x + b*y == gcd.
-- 拡張ユークリッドの互除法：a*x + b*y == gcd（0 以上）となる x と y も求める
extendedGcd :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGcd a 0 = (abs a, signum a, 0)
extendedGcd a b =
  let (g, x, y) = extendedGcd b (a `mod` b)
  in  (g, y, x - (a `div` b) * y)

-- ➗ The number that undoes "times a" mod m (m is 1 or more), if there
--    is one.
-- m を法として「a 倍」を元に戻す数（あれば）。m は 1 以上
inverseMod :: Integer -> Integer -> Maybe Integer
inverseMod a m
  | m <= 0    = Nothing
  | otherwise =
      case extendedGcd a m of
        (1, x, _) -> Just (x `mod` m)
        _         -> Nothing
-- ANCHOR_END: inverse

-- ANCHOR: mod
-- 🔢 Numbers mod 998244353, the favorite prime of competitive programming.
--    With a Num instance, +, -, *, sum and product all work mod p.
--    The constructor is hidden: every Mod is made by fromInteger, so it
--    is always 0 .. p - 1, and a product of two fits in an Int.
-- 998244353 を法とする数。競プロでおなじみの素数。
-- Num のインスタンスがあるので、+・-・*・sum・product がそのまま使える。
-- 構成子は外に出さない。どの Mod も fromInteger で作るので、いつも
-- 0 〜 p - 1 で、2つの積も Int に収まる
newtype Mod = Mod Int
  deriving (Eq, Show)

modulus :: Int
modulus = 998244353

instance Num Mod where
  Mod a + Mod b = Mod ((a + b) `mod` modulus)
  Mod a - Mod b = Mod ((a - b) `mod` modulus)
  Mod a * Mod b = Mod ((a * b) `mod` modulus)
  fromInteger n = Mod (fromInteger (n `mod` toInteger modulus))
  abs           = id
  signum _      = Mod 1

-- 🎲 n choose k mod p, for big n: n! / (k! (n - k)!) with inverses.
--    0 when k < 0 or k > n. n must be less than p (so k! is not a
--    multiple of p, and has an inverse).
-- 大きな n の組み合わせの数を、p で割った余りで。割り算は逆元で。
-- k < 0 か k > n なら 0。n は p より小さいこと（k! が p の倍数に
-- ならず、逆元があるように）
combinationsMod :: Int -> Int -> Mod
combinationsMod n k
  | k < 0 || k > n = 0
  | otherwise =
      let top    = product (map fromIntegral [n - k + 1 .. n]) :: Mod
          bottom = product (map fromIntegral [1 .. k])         :: Mod
          Mod b  = bottom
          p      = toInteger modulus
          inv    = powMod (toInteger b) (p - 2) p   -- Fermat / フェルマー
      in  top * fromInteger inv
-- ANCHOR_END: mod

-- ANCHOR: hash
-- #️⃣ A polynomial hash of a string: the characters as digits in base 31,
--    mod a big prime.
-- 文字列の多項式ハッシュ：文字を 31 進数の桁とみて、大きな素数の余りをとる
stringHash :: String -> Int
stringHash = foldl step 0
  where
    step acc c = (acc * 31 + ord c) `mod` 1000000007

-- 🔎 Where does a pattern appear? Compare hashes first (Rabin-Karp);
--    only when they match, compare the letters.
-- パターンはどこに出てくる？ まずハッシュを比べ（ラビン－カープ）、
-- 一致したときだけ文字を比べる
findPattern :: String -> String -> [Int]
findPattern pattern text =
  [ i
  | (i, rest) <- zip [0 ..] (tails text)
  , let window = take len rest
  , length window == len
  , stringHash window == target
  , pattern `isPrefixOf` rest
  ]
  where
    len    = length pattern
    target = stringHash pattern
-- ANCHOR_END: hash

-- ANCHOR: rsa
-- 🔐 A tiny RSA with p = 61, q = 53: n = 3233, e = 17, d = 2753.
--    Anyone may encrypt with (n, e); only the holder of d can decrypt.
--    (Far too small to be safe. Real keys are 2048 bits or more.)
-- p = 61、q = 53 の小さな RSA。(n, e) で誰でも暗号にでき、
-- d を持つ人だけが元に戻せる
-- （小さすぎて安全ではない。本物は 2048 ビット以上）
rsaEncrypt :: Integer -> Integer
rsaEncrypt message = powMod message 17 3233

rsaDecrypt :: Integer -> Integer
rsaDecrypt secret = powMod secret 2753 3233
-- ANCHOR_END: rsa
-- ANCHOR_END: all
