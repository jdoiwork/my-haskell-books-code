-- ANCHOR: all
-- | A tiny NumPy: n-dimensional arrays of Double.
--   Use it like NumPy:  import qualified MiniNumpy as Np
-- 小さな NumPy：Double の多次元配列。
-- NumPy と同じように使う：import qualified MiniNumpy as Np
module MiniNumpy
  ( NDArray
  , shape
  , ndim
  , size
  , nrows
  , toList
  , item
    -- * Making arrays / 配列を作る
  , array
  , array2
  , scalar
  , zeros
  , ones
  , full
  , arange
  , linspace
    -- * Changing the shape / 形を変える
  , reshape
  , transpose
    -- * Looking inside / 中をのぞく
  , at
  , row
    -- * Computing / 計算する
  , map
  , dot
  , sum
  , mean
  , sumAxis
  , maxAll
  , argmax
  , argmaxAxis1
  , broadcastShape
  ) where

import Prelude hiding (map, sum)
import qualified Prelude
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as VU

-- ANCHOR: type
-- 📦 An array: a shape, and all the numbers in one row (row-major).
--    [[1, 2, 3], [4, 5, 6]] is shape [2, 3] and values [1, 2, 3, 4, 5, 6].
-- 配列：形と、すべての数を1列に並べたもの（行を順に並べる）
data NDArray = NDArray
  { arrShape  :: [Int]
  , arrValues :: VU.Vector Double
  }
  deriving (Eq)

-- 📐 The shape, like a.shape in NumPy.
-- 形。NumPy の a.shape
shape :: NDArray -> [Int]
shape = arrShape

-- How many dimensions (a.ndim) and how many numbers (a.size).
-- 次元の数（a.ndim）と、数の個数（a.size）
ndim :: NDArray -> Int
ndim = length . arrShape

size :: NDArray -> Int
size = VU.length . arrValues

-- How many rows (the first dimension; a 0-D array has 1).
-- 行の数（最初の次元。0次元の配列は 1）
nrows :: NDArray -> Int
nrows a = case arrShape a of
  (r : _) -> r
  []      -> 1

toList :: NDArray -> [Double]
toList = VU.toList . arrValues

-- ANCHOR: item
-- The one number inside (like a.item() in NumPy).
-- 中の1つの数（NumPy の a.item()）
item :: NDArray -> Double
item a
  | size a == 1 = VU.head (arrValues a)
  | otherwise   = error "item: the array has more than one number"
-- ANCHOR_END: item
-- ANCHOR_END: type

-- ANCHOR: make
-- 🏭 Np.array [1, 2, 3]: a 1-D array.
-- 1次元の配列
array :: [Double] -> NDArray
array xs = NDArray [length xs] (VU.fromList xs)

-- 🏭 Np.array2 [[1, 2], [3, 4]]: a 2-D array (every row the same length).
-- 2次元の配列（どの行も同じ長さ）
array2 :: [[Double]] -> NDArray
array2 rows = case rows of
  [] -> NDArray [0, 0] VU.empty
  (r : _)
    | all (\x -> length x == length r) rows ->
        NDArray [length rows, length r] (VU.fromList (concat rows))
    | otherwise -> error "array2: the rows have different lengths"

-- 🔢 A single number as a 0-D array (shape []).
-- 1つの数を、0次元の配列にする（形は []）
scalar :: Double -> NDArray
scalar x = NDArray [] (VU.singleton x)

-- 🏭 Np.zeros [2, 3], Np.ones [2, 3], Np.full [2, 3] 7
zeros, ones :: [Int] -> NDArray
zeros s = full s 0
ones s  = full s 1

full :: [Int] -> Double -> NDArray
full s x = NDArray s (VU.replicate (product s) x)

-- 🏭 Np.arange 5  ->  [0, 1, 2, 3, 4]
arange :: Int -> NDArray
arange n = NDArray [n] (VU.generate n fromIntegral)

-- 🏭 Np.linspace 0 1 5  ->  [0, 0.25, 0.5, 0.75, 1]
linspace :: Double -> Double -> Int -> NDArray
linspace a b n =
  let step = (b - a) / fromIntegral (n - 1)
  in NDArray [n] (VU.generate n (\i -> a + fromIntegral i * step))
-- ANCHOR_END: make

-- ANCHOR: reshape
-- 🔁 The same numbers, seen in another shape. The sizes must match,
--    or it stops (like NumPy's ValueError).
-- 同じ数を、別の形で見る。大きさが合わなければ止まる
-- （NumPy の ValueError と同じ）
reshape :: [Int] -> NDArray -> NDArray
reshape s a
  | product s == size a = NDArray s (arrValues a)
  | otherwise = error ("cannot reshape array of size " <> show (size a)
                       <> " into shape " <> show s)

-- 🔄 Rows become columns (2-D only): a.T
-- 行と列を入れかえる（2次元だけ）
transpose :: NDArray -> NDArray
transpose a = case arrShape a of
  [r, c] -> NDArray [c, r]
              (VU.generate (r * c) (\k -> let (j, i) = k `divMod` r
                                          in arrValues a VU.! (i * c + j)))
  _ -> error "transpose: only for 2-D arrays"
-- ANCHOR_END: reshape

-- ANCHOR: at
-- 🔍 a[i, j]: the number at an index.
-- 添え字の場所の数
at :: NDArray -> [Int] -> Double
at a idx
  | length idx /= ndim a = error "at: wrong number of indices"
  | otherwise = arrValues a VU.! offset (arrShape a) idx

-- The place in the row of values: [i, j] in shape [r, c] is i * c + j.
-- 1列の中の位置：形 [r, c] の [i, j] は i * c + j
offset :: [Int] -> [Int] -> Int
offset s idx = Prelude.sum (zipWith (*) idx (strides s))

-- How far to move in the row for one step in each dimension.
-- 各次元で1つ進むと、1列の中でいくつ進むか
strides :: [Int] -> [Int]
strides s = drop 1 (scanr (*) 1 s)

-- 🔍 a[i]: one row of a 2-D array.
-- 2次元の配列の1行
row :: Int -> NDArray -> NDArray
row i a = case arrShape a of
  [_, c] -> NDArray [c] (VU.slice (i * c) c (arrValues a))
  _      -> error "row: only for 2-D arrays"
-- ANCHOR_END: at

-- ANCHOR: broadcast
-- 📡 Broadcasting: the shape of a + b. Line the shapes up from the
--    right; each pair must be equal, or one of them must be 1.
--    [2, 3] and [3] -> [2, 3].   [2, 3] and [2] -> error.
-- ブロードキャスト：a + b の形。形を右からそろえて並べ、
-- それぞれの組は、同じか、どちらかが 1 でなければならない
broadcastShape :: [Int] -> [Int] -> Maybe [Int]
broadcastShape s1 s2 =
  let n   = max (length s1) (length s2)
      pad s = replicate (n - length s) 1 <> s
      pick a b
        | a == b    = Just a
        | a == 1    = Just b
        | b == 1    = Just a
        | otherwise = Nothing
  in sequence (zipWith pick (pad s1) (pad s2))

-- ➕ Combine two arrays number by number, broadcasting the shapes.
-- 2つの配列を、数ごとに組み合わせる（形はブロードキャストする）
zipBroadcast :: (Double -> Double -> Double)
             -> NDArray -> NDArray -> NDArray
zipBroadcast f a b
  -- Fast paths: the same shape, or a single number on one side.
  -- 速い道：同じ形どうし、または片方が1つの数
  | arrShape a == arrShape b =
      NDArray (arrShape a) (VU.zipWith f (arrValues a) (arrValues b))
  | null (arrShape b) = let y = VU.head (arrValues b)
                        in a { arrValues = VU.map (`f` y) (arrValues a) }
  | null (arrShape a) = let x = VU.head (arrValues a)
                        in b { arrValues = VU.map (x `f`) (arrValues b) }
zipBroadcast f a b = case broadcastShape (arrShape a) (arrShape b) of
  Nothing -> error ("operands could not be broadcast together with shapes "
                    <> show (arrShape a) <> " " <> show (arrShape b))
  Just s ->
    let n = length s
        -- Where in a (or b) does the result's index k come from?
        -- 結果の k 番目は、a（や b）のどこから来る？
        source arr =
          let as  = replicate (n - ndim arr) 1 <> arrShape arr
              st  = strides as
          in \idx -> Prelude.sum [ if d == 1 then 0 else i * w
                                 | (i, d, w) <- zip3 idx as st ]
        fromA = source a
        fromB = source b
        indexOf k = zipWith (\q m -> q `mod` m)
                      (Prelude.map (k `div`) (strides s)) s
        value k =
          let idx = indexOf k
          in f (arrValues a VU.! fromA idx) (arrValues b VU.! fromB idx)
    in NDArray s (VU.generate (product s) value)
-- ANCHOR_END: broadcast

-- ANCHOR: num
-- 🧮 a + b, a * b, a - 2, ... work like NumPy (with broadcasting).
--    A plain number like 2 becomes a 0-D array.
-- a + b、a * b、a - 2 などが、NumPy と同じように動く
-- （ブロードキャスト付き）。
-- 2 のような数は、0次元の配列になる
instance Num NDArray where
  (+)         = zipBroadcast (+)
  (-)         = zipBroadcast (-)
  (*)         = zipBroadcast (*)
  negate      = map negate
  abs         = map abs
  signum      = map signum
  fromInteger = scalar . fromInteger

instance Fractional NDArray where
  (/)          = zipBroadcast (/)
  fromRational = scalar . fromRational

-- 📐 Np.exp, Np.log, Np.sqrt, ... (the usual math, number by number).
-- いつもの数学の関数を、数ごとに
instance Floating NDArray where
  pi    = scalar pi
  exp   = map exp
  log   = map log
  sqrt  = map sqrt
  sin   = map sin
  cos   = map cos
  asin  = map asin
  acos  = map acos
  atan  = map atan
  sinh  = map sinh
  cosh  = map cosh
  asinh = map asinh
  acosh = map acosh
  atanh = map atanh

-- 🔧 Use a function on every number (like np.vectorize).
-- すべての数に関数を使う
map :: (Double -> Double) -> NDArray -> NDArray
map f a = a { arrValues = VU.map f (arrValues a) }
-- ANCHOR_END: num

-- ANCHOR: dot
-- ✖️ Np.dot: matrix times matrix, matrix times vector, or vector · vector.
-- 行列×行列、行列×ベクトル、ベクトル・ベクトル
dot :: NDArray -> NDArray -> NDArray
dot a b = case (arrShape a, arrShape b) of
  ([n], [m]) | n == m ->
    scalar (VU.sum (VU.zipWith (*) (arrValues a) (arrValues b)))
  ([r, k], [k']) | k == k' ->
    NDArray [r] (VU.generate r (\i -> rowDot i (\t -> arrValues b VU.! t)))
    where rowDot i get = Prelude.sum [ arrValues a VU.! (i * k + t) * get t
                                     | t <- [0 .. k - 1] ]
  ([r, k], [k', c]) | k == k' ->
    NDArray [r, c] (VU.generate (r * c) cell)
    where
      cell ij = let (i, j) = ij `divMod` c
                in Prelude.sum [ arrValues a VU.! (i * k + t)
                                 * arrValues b VU.! (t * c + j)
                               | t <- [0 .. k - 1] ]
  (s1, s2) -> error ("shapes " <> show s1 <> " and " <> show s2
                     <> " not aligned")
-- ANCHOR_END: dot

-- ANCHOR: reduce
-- ➕ The sum and the mean of all numbers (a 0-D array).
-- すべての数の合計と平均（0次元の配列）
sum, mean :: NDArray -> NDArray
sum a  = scalar (VU.sum (arrValues a))
mean a = scalar (VU.sum (arrValues a) / fromIntegral (size a))

-- ➕ Np.sumAxis 0 a: add up the rows (2-D), giving one number per column.
--    Np.sumAxis 1 a: add up each row, giving one number per row.
-- 軸 0 で足す：列ごとに1つ。軸 1 で足す：行ごとに1つ
sumAxis :: Int -> NDArray -> NDArray
sumAxis axis a = case (axis, arrShape a) of
  (0, [r, c]) -> NDArray [c] (VU.generate c (\j ->
                   Prelude.sum [ arrValues a VU.! (i * c + j)
                               | i <- [0 .. r - 1] ]))
  (1, [r, c]) -> NDArray [r] (VU.generate r (\i ->
                   VU.sum (VU.slice (i * c) c (arrValues a))))
  _ -> error "sumAxis: axis 0 or 1 of a 2-D array"

-- 🏔️ The biggest number, and where it is (counting in the row).
-- いちばん大きな数と、その位置（1列で数えて）
maxAll :: NDArray -> Double
maxAll = VU.maximum . arrValues

argmax :: NDArray -> Int
argmax = VU.maxIndex . arrValues

-- 🏔️ For each row of a 2-D array: where is the biggest number?
-- 2次元の配列の行ごとに：いちばん大きな数はどこ？
argmaxAxis1 :: NDArray -> [Int]
argmaxAxis1 a = case arrShape a of
  [r, c] -> [ VU.maxIndex (VU.slice (i * c) c (arrValues a))
            | i <- [0 .. r - 1] ]
  _      -> error "argmaxAxis1: only for 2-D arrays"
-- ANCHOR_END: reduce

-- ANCHOR: show
-- 🖨️ Show it like NumPy: array([[1., 2.], [3., 4.]])
-- NumPy と同じように見せる
instance Show NDArray where
  show a = case arrShape a of
    [] -> showNum (VU.head (arrValues a))
    s  -> "array(" <> go s (toList a) <> ")"
    where
      go [_] xs      = "[" <> intercalate ", " (Prelude.map showNum xs) <> "]"
      go (_ : ds) xs =
        let chunk = product ds
            parts = Prelude.map (go ds) (chunksOf chunk xs)
        in "[" <> intercalate ", " parts <> "]"
      go [] _ = ""
      chunksOf k xs = case splitAt k xs of
        (front, [])   -> [front]
        (front, rest) -> front : chunksOf k rest

-- 1.0 -> "1.", 0.5 -> "0.5" (like NumPy).
showNum :: Double -> String
showNum x
  | x == fromIntegral (round x :: Integer) && abs x < 1e15 =
      show (round x :: Integer) <> "."
  | otherwise = show x
-- ANCHOR_END: show
-- ANCHOR_END: all
