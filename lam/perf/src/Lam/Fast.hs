{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Faster versions, in Haskell, of some functions of the standard
--   modules. The Lam definitions stay: the type checker reads their
--   types, and lam to-js and lam to-python turn them into JavaScript and
--   Python. Only the value a module shows is the Haskell one.
--   Each one calls the function it is given in the same order as the
--   Lam definition, with the same name (in f, called at ...): the same
--   answers, the same errors, the same lines of Debug.log.
-- 標準のモジュールの関数のいくつかを、Haskell で書いた速い版。Lam の定義は
-- 残す：型検査器はその型を読み、lam to-js と lam to-python はそれを
-- JavaScript と Python にする。モジュールが見せる値だけが、Haskell の版に
-- なる。どれも、受け取った関数を Lam の定義と同じ順に、同じ名前で呼ぶ
-- （in f, called at …）。同じ答え、同じエラー、同じ Debug.log の行
module Lam.Fast
  ( fastVersion
  , fastNames
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Lam.Eval
import Lam.Syntax (Name, Op (..))

-- ANCHOR: table
-- 🚀 The Haskell version of a name of a standard module, if there is one.
-- 標準のモジュールの名前の、Haskell の版（あれば）
fastVersion :: Name -> Name -> Maybe Value
fastVersion m x = Map.lookup (m, x) fastTable

-- The names that have a Haskell version: the module, and the name in it.
-- Haskell の版がある名前：モジュールと、その中の名前
fastNames :: [(Name, Name)]
fastNames = Map.keys fastTable

fastTable :: Map (Name, Name) Value
fastTable = Map.fromList
  [ (("List", "foldl"),   prim "foldl" 3 foldlFast)
  , (("List", "foldr"),   prim "foldr" 3 foldrFast)
  , (("List", "map"),     prim "map" 2 mapFast)
  , (("List", "sortBy"),  prim "sortBy" 2 sortByFast)
  , (("Prelude", "range"), prim "range" 2 rangeFast)
  , (("Prelude", "sort"),  prim "sort" 1 sortFast)
  , (("File", "eachLine"), prim "eachLine" 3 eachLineFast) ]
  where
    prim name arity run = VPrim name arity run []
-- ANCHOR_END: table

-- 📞 Call f with one argument, by the name the Lam definition calls it.
-- f を引数1つで呼ぶ。Lam の定義が呼ぶときの名前で
call :: Name -> Value -> Value -> Either EvalError Value
call name f a = inCall name (apply f a)

-- ANCHOR: foldl
-- 🔁 foldl f z xs: f z x for each x, from the left.
--    Lam:  foldl f z (x : rest) = foldl f (f z x) rest
-- 左から、x ごとに f z x
foldlFast :: [Value] -> Either EvalError Value
foldlFast [f, z, VList xs] = go z xs
  where
    go acc ys = case ys of
      [] -> Right acc
      y : rest -> do
        g    <- call "f" f acc
        acc' <- call "f" g y
        go acc' rest
foldlFast _ = Left (BuiltinFailed "foldl" "not a list")
-- ANCHOR_END: foldl

-- 🔁 foldr f z xs. As the Lam definition does, f x is made first, then
--    the rest is folded, then f x is given it.
--    Lam:  foldr f z (x : rest) = f x (foldr f z rest)
-- Lam の定義と同じく、まず f x を作り、残りをたたみ、それを f x に渡す
foldrFast :: [Value] -> Either EvalError Value
foldrFast [f, z, VList xs] = go xs
  where
    go ys = case ys of
      [] -> Right z
      y : rest -> do
        g <- call "f" f y
        r <- go rest
        call "f" g r
foldrFast _ = Left (BuiltinFailed "foldr" "not a list")

-- ANCHOR: map
-- 🗺️ map f xs. The Lam definition is a foldr, so f is called from the
--    LAST item to the first: the same here.
--    Lam:  map f xs = foldr (\x rest -> f x : rest) [] xs
-- Lam の定義は foldr なので、f は最後の要素から先頭へ向かって呼ばれる。
-- ここでも同じ
mapFast :: [Value] -> Either EvalError Value
mapFast [f, VList xs] = VList <$> go (reverse xs) []
  where
    go ys done = case ys of
      [] -> Right done
      y : rest -> do
        v <- call "f" f y
        go rest (v : done)
mapFast _ = Left (BuiltinFailed "map" "not a list")
-- ANCHOR_END: map

-- ANCHOR: sortBy
-- 🗂️ sortBy before xs: the merge sort of the Lam definition, step for
--    step. Cut in half (the first half is length // 2), sort the first
--    half, then the second, then merge: before y x is asked in the same
--    order.
-- Lam の定義のマージソートを、1歩ずつ同じに。半分に切り（前半は
-- length // 2 個）、前半、後半の順に並べ、併せる。before y x を聞く順も同じ
sortByFast :: [Value] -> Either EvalError Value
sortByFast [before, VList xs] = VList <$> mergeSort (call2 "before" before) xs
sortByFast _ = Left (BuiltinFailed "sortBy" "not a list")

-- 🗂️ sort xs: sortOn id, that is, sortBy (\a b -> a < b).
-- sortOn id。つまり sortBy (\a b -> a < b)
sortFast :: [Value] -> Either EvalError Value
sortFast [VList xs] = VList <$> mergeSort less xs
  where
    less a b = binOp Lt a b
sortFast _ = Left (BuiltinFailed "sort" "not a list")

-- Call a function of two arguments, and want a Bool.
-- 引数2つの関数を呼び、Bool をもらう
call2 :: Name -> Value -> Value -> Value -> Either EvalError Value
call2 name f a b = do
  g <- call name f a
  call name g b

mergeSort
  :: (Value -> Value -> Either EvalError Value) -> [Value]
  -> Either EvalError [Value]
mergeSort before = sortList
  where
    sortList ys
      | n < 2     = Right ys
      | otherwise = do
          front <- sortList (take half ys)
          back  <- sortList (drop half ys)
          merge front back []
      where
        n    = length ys
        half = n `div` 2
    -- Lam:  if before y x then y : merge xs yr else x : merge xr ys
    merge as bs done = case (as, bs) of
      ([], _) -> Right (reverse done <> bs)
      (_, []) -> Right (reverse done <> as)
      (a : ar, b : br) -> do
        yes <- before b a
        case yes of
          VBool True  -> merge as br (b : done)
          VBool False -> merge ar bs (a : done)
          other       -> Left (IfNeedsBool (renderValue other))
-- ANCHOR_END: sortBy

-- ANCHOR: range
-- 🧮 range a b: a, a + 1, ... while it is not bigger than b. Made with
--    > and + of Lam, so a Float works too (range 0.5 2 is [0.5, 1.5]).
--    Lam:  range a b = if a > b then [] else a : range (a + 1) b
-- a, a + 1, … を、b より大きくなるまで。Lam の > と + で作るので、
-- Float でも動く
rangeFast :: [Value] -> Either EvalError Value
rangeFast [a0, b] = VList . reverse <$> go a0 []
  where
    go a done = do
      over <- binOp Gt a b
      case over of
        VBool True  -> Right done
        VBool False -> do
          next <- binOp Add a (VInt 1)
          go next (a : done)
        other       -> Left (IfNeedsBool (renderValue other))
rangeFast _ = Left (BuiltinFailed "range" "two numbers")
-- ANCHOR_END: range

-- ANCHOR: eachLine
-- 📜 eachLine h acc step: the loop under File.foldLines. One ReadLine
--    for each line, as before (so --trace, the permissions and the fake
--    world see the same requests), but the do and its binds are made
--    here, not by Lam.
--    Lam:  eachLine h acc step = do
--            next <- readLine h
--            case next of
--              | Nothing -> pure acc
--              | Just line -> do
--                  acc2 <- step acc line
--                  eachLine h acc2 step
-- File.foldLines の下のループ。1行ごとに ReadLine を1つ、前と同じく頼む
-- （--trace・権限・偽物の世界には、同じ頼みごとが見える）。ただし、do と
-- その bind は、Lam ではなくここで作る
eachLineFast :: [Value] -> Either EvalError Value
eachLineFast [VCon "Handle" _ [VInt n], start, step] = Right (VIO (go start))
  where
    go acc = Bind (Ask (ReadLine (fromInteger n))) $ \answer ->
      case answer of
        VCon "Ok" _ [VCon "Nothing" _ []]   -> Right (Pure acc)
        VCon "Ok" _ [VCon "Just" _ [line]] -> do
          next <- call2 "step" step acc line
          case next of
            VIO act -> Right (Bind act (Right . go))
            _       -> Left (BuiltinFailed "IO.bindIO"
                               "got a value that is not IO")
        -- readLine: a broken line stops the program (error why).
        -- readLine：壊れた行は、プログラムを止める（error why）
        VCon "Err" _ [VString why]          -> Left (Raised why)
        other -> Left (BuiltinFailed "File.nextLine" (renderValue other))
eachLineFast _ = Left (BuiltinFailed "eachLine" "not a Handle")
-- ANCHOR_END: eachLine
-- ANCHOR_END: all
