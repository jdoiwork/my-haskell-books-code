-- ANCHOR: all
-- | Matrices whose shape is written in their type: Matrix 35 32.
-- 形が型に書いてある行列：Matrix 35 32
module MiniTorch.Typed
  ( Matrix
  , Vector
  , toNDArray
  , vectorToNDArray
  , natInt
  , fromNDArray
  , vectorFromNDArray
  , matrix
  , unsafeMatrix
  , unsafeVector
  , withRows
  , matmul
  , add
  , addRow
  , mapMatrix
  , transposeM
  , vstack
  ) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits
  (KnownNat, Nat, SomeNat (..), natVal, someNatVal, type (+))

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)

-- ANCHOR: types
-- 📐 A matrix with r rows and c columns. The constructor is hidden:
--    only the functions below can make one.
-- r 行 c 列の行列。コンストラクターは隠してあり、下の関数だけが作れる
newtype Matrix (r :: Nat) (c :: Nat) = Matrix NDArray

-- 📏 A vector with n numbers (for biases).
-- n 個の数のベクトル（バイアス用）
newtype Vector (n :: Nat) = Vector NDArray

-- 📤 Take out the plain array.
-- ふつうの配列を取り出す
toNDArray :: Matrix r c -> NDArray
toNDArray (Matrix a) = a

vectorToNDArray :: Vector n -> NDArray
vectorToNDArray (Vector a) = a

-- 👀 Show the numbers inside, like the plain array.
-- 中の数を、ふつうの配列と同じように見せる
instance Show (Matrix r c) where
  show (Matrix a) = show a

instance Show (Vector n) where
  show (Vector a) = show a
-- ANCHOR_END: types

-- ANCHOR: natInt
-- 🔢 Read a number written in a type, as an Int: natInt (Proxy @35) == 35.
-- 型に書いた数を、Int として読む
natInt :: KnownNat n => Proxy n -> Int
natInt p = fromIntegral (natVal p)
-- ANCHOR_END: natInt

-- ANCHOR: gate
-- 🚪 The gate: a plain array becomes a Matrix r c only if its shape
--    really is [r, c].
-- 門番：ふつうの配列は、形が本当に [r, c] のときだけ Matrix r c になる
fromNDArray :: forall r c. (KnownNat r, KnownNat c)
            => NDArray -> Either String (Matrix r c)
fromNDArray a
  | Np.shape a == wanted = Right (Matrix a)
  | otherwise            =
      Left ("expected shape " <> show wanted
            <> ", got " <> show (Np.shape a))
  where
    wanted = [natInt (Proxy @r), natInt (Proxy @c)]

-- 🚪 The same gate for vectors.
-- ベクトルの門番
vectorFromNDArray :: forall n. KnownNat n
                  => NDArray -> Either String (Vector n)
vectorFromNDArray a
  | Np.shape a == wanted = Right (Vector a)
  | otherwise            =
      Left ("expected shape " <> show wanted
            <> ", got " <> show (Np.shape a))
  where
    wanted = [natInt (Proxy @n)]

-- ✍️ From nested lists, through the gate.
-- 入れ子のリストから、門番を通して
matrix :: (KnownNat r, KnownNat c) => [[Double]] -> Either String (Matrix r c)
matrix rows = fromNDArray (Np.array2 rows)
-- ANCHOR_END: gate

-- ANCHOR: unsafe
-- ⚠️ No check at all. Only for code that already knows the shape is right
--    (like the inside of this library). The name says "be careful".
-- 何も確かめない。形が正しいとわかっているコード（このライブラリの中など）
-- だけが使う。名前が「気をつけて」と言っている
unsafeMatrix :: NDArray -> Matrix r c
unsafeMatrix = Matrix

unsafeVector :: NDArray -> Vector n
unsafeVector = Vector
-- ANCHOR_END: unsafe

-- ANCHOR: withRows
-- 🎁 When the number of rows is known only at run time (it came from a
--    file), turn it into a type, and hand the matrix to a function that
--    works for any n.
-- 行の数が実行するまでわからない（ファイルから来た）とき、それを型にして、
-- どんな n でも動く関数に、行列を渡す
withRows :: forall c k. KnownNat c
         => NDArray
         -> (forall n. KnownNat n => Matrix n c -> k)
         -> Either String k
withRows a use =
  case someNatVal (fromIntegral (Np.nrows a)) of
    Nothing -> Left "a negative number of rows"
    Just (SomeNat (_ :: Proxy n)) -> do
      m <- fromNDArray @n @c a
      pure (use m)
-- ANCHOR_END: withRows

-- ANCHOR: ops
-- ✖️ (a × b) times (b × c) is (a × c). The b's must match.
-- (a × b) と (b × c) の積は (a × c)。b がそろっていないといけない
matmul :: Matrix a b -> Matrix b c -> Matrix a c
matmul (Matrix x) (Matrix y) = Matrix (Np.dot x y)

-- ➕ Only matrices of the same shape can be added.
-- 同じ形の行列だけが足せる
add :: Matrix r c -> Matrix r c -> Matrix r c
add (Matrix x) (Matrix y) = Matrix (x + y)

-- ➕ Add a bias of c numbers to every row of an (n × c) matrix.
-- (n × c) の行列の、どの行にも、c 個のバイアスを足す
addRow :: Matrix n c -> Vector c -> Matrix n c
addRow (Matrix x) (Vector b) = Matrix (x + b)

-- 🪄 Change every number; the shape stays.
-- すべての数を変える。形はそのまま
mapMatrix :: (Double -> Double) -> Matrix r c -> Matrix r c
mapMatrix f (Matrix x) = Matrix (Np.map f x)

-- 🔄 Rows become columns.
-- 行と列を入れかえる
transposeM :: Matrix r c -> Matrix c r
transposeM (Matrix x) = Matrix (Np.transpose x)
-- ANCHOR_END: ops

-- ANCHOR: vstack
-- 🥞 Stack two matrices: a rows on top of b rows makes (a + b) rows.
-- 2つの行列を積む：a 行の下に b 行を積むと、(a + b) 行
vstack :: forall a b c. KnownNat c
       => Matrix a c -> Matrix b c -> Matrix (a + b) c
vstack (Matrix x) (Matrix y) =
  let rows = Np.nrows x + Np.nrows y
      cols = natInt (Proxy @c)
  in  Matrix (Np.reshape [rows, cols]
                         (Np.array (Np.toList x <> Np.toList y)))
-- ANCHOR_END: vstack
-- ANCHOR_END: all
