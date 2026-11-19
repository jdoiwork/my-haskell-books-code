{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The evaluator: walk the tree and compute a value.
-- 評価器：木をたどって、値を計算する。
module Lam.Eval
  ( Value (..)
  , Key (..)
  , Env
  , EvalError (..)
  , eval
  , apply
  , binOp
  , floatOf
  , compareValues
  , match
  , renderValue
  , renderError
  ) where

-- The LAZY Map: a value is not computed when it is put in.
-- The knot of let needs this (see the chapter).
-- 遅延な Map：値は、入れたときには計算されない。
-- let の結び目には、これが要る（本文を参照）
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Pretty (opSymbol, renderString, showFloat)
import Lam.Syntax

-- ANCHOR: value
-- 💎 What a Lam program computes.
-- Lam のプログラムが計算するもの
data Value
  = VInt Integer
  -- ANCHOR: vfloat
  -- A Float. A Float may also be held as a VInt: the 3 of  3 / 2  is
  -- read as a number of any type, and the type checker said Float.
  -- Float。Float が VInt のこともある：3 / 2 の 3 は、どの型にもなれる
  -- 数として読み、型検査器が Float と決めたもの
  | VFloat Double
  -- ANCHOR_END: vfloat
  | VBool Bool
  | VString Text           -- "hello"
  | VTuple [Value]         -- (1, "one")
  | VList [Value]          -- [1, 2, 3]
  -- ANCHOR: vmap
  -- {"Python": 82, "TS": 75} and Set.fromList [1, 2]: the keys are
  -- kept in order, inside a Key.
  -- キーは順に並べて持つ。キーは Key に包む
  | VMap (Map Key Value)
  | VSet (Set Key)
  -- ANCHOR_END: vmap
  -- ANCHOR: vcon
  -- A value made with a constructor: Just 3, Circle 2, Nothing.
  -- The number is the constructor's place in the declaration
  -- (Nothing is 0, Just is 1), for comparing.
  -- 構成子で作った値。番号は、宣言の中での構成子の順番（比べるのに使う）
  | VCon Name Int [Value]
  -- ANCHOR_END: vcon
  -- ANCHOR: vrecord
  -- A record: each field's name and value.  { name = "Lam", age = 3 }
  -- レコード：フィールドの名前と値
  | VRecord (Map Name Value)
  -- ANCHOR_END: vrecord
  -- A function, together with the variables it could see when it
  -- was made (a closure).
  -- 関数と、それが作られたときに見えていた変数たち（クロージャ）
  | VClosure Name Expr Env
  -- A function written in Haskell: its name, how many arguments
  -- it needs, what it does, and the arguments given so far.
  -- Haskell で書いた関数：名前、要る引数の数、すること、ここまでの引数
  | VPrim Name Int ([Value] -> Either EvalError Value) [Value]

-- ANCHOR: key
-- 🔑 A key of a Map or a Set. Data.Map needs Ord, but a Value has no
--    Ord: a function cannot be compared. The type checker lets only a
--    Comparable type be a key, so compareKeys always gives Right.
-- Map や Set のキー。Data.Map には Ord が要るが、Value には Ord がない
-- （関数は比べられない）。キーにできるのは Comparable の型だけだと
-- 型検査器が確かめるので、compareKeys はいつも Right を返す
newtype Key = Key Value

instance Eq Key where
  a == b = compare a b == EQ

instance Ord Key where
  compare (Key a) (Key b) = case compareKeys a b of
    Right o -> o
    Left _  -> error "a key that cannot be compared"
-- ANCHOR_END: key

-- 📒 The environment: the value of each variable.
-- 環境：それぞれの変数の値
type Env = Map Name Value

-- 🚨 Things that can go wrong while computing.
-- 計算の途中で起きうるまちがい
data EvalError
  = UnboundVariable Name
  | NotAFunction Text
  | WrongOperand Op Text
  | IfNeedsBool Text
  | DivideByZero
  | CannotCompare Text           -- a function cannot be compared
  | BuiltinFailed Name Text      -- head []
  | NoMatch Text                 -- no branch of case fits the value
  -- ANCHOR: noField
  -- r.name, but r has no name (the type checker stops it before).
  -- r.name なのに、r に name がない（型検査器が先に止める）
  | NoField Name Text
  -- ANCHOR_END: noField
  deriving (Eq, Show)
-- ANCHOR_END: value

-- ANCHOR: eval
-- ⚙️ The value of an expression, in an environment.
-- 環境の中での、式の値
eval :: Env -> Expr -> Either EvalError Value
eval env expr = case expr of
  Lit (LInt n)    -> Right (VInt n)
  Lit (LFloat x)  -> Right (VFloat x)
  Lit (LBool b)   -> Right (VBool b)
  Lit (LString s) -> Right (VString s)

  Var x -> case Map.lookup x env of
    Just v  -> Right v
    Nothing -> Left (UnboundVariable x)

  -- A function remembers the environment of NOW.
  -- 関数は、「いま」の環境を覚えておく
  Lam x body -> Right (VClosure x body env)

  -- ANCHOR: app
  App f a -> do
    fv <- eval env f
    av <- eval env a
    apply fv av
  -- ANCHOR_END: app

  If c t e -> do
    cv <- eval env c
    case cv of
      VBool True  -> eval env t
      VBool False -> eval env e
      other       -> Left (IfNeedsBool (renderValue other))

  -- ANCHOR: collections
  -- Compute every part, from left to right.
  -- 部品を、左から順に全部計算する
  Tuple es -> VTuple <$> traverse (eval env) es
  List es  -> VList  <$> traverse (eval env) es
  -- ANCHOR_END: collections

  -- ANCHOR: shortcut
  -- && and || look at the right side only when they need it.
  -- && と || は、要るときだけ右側を見る
  BinOp And l r -> do
    lv <- eval env l
    case lv of
      VBool False -> Right (VBool False)   -- false && ...  is false
      VBool True  -> eval env r            -- true && r     is r
      other       -> Left (WrongOperand And (renderValue other))

  BinOp Or l r -> do
    lv <- eval env l
    case lv of
      VBool True  -> Right (VBool True)    -- true || ...   is true
      VBool False -> eval env r            -- false || r    is r
      other       -> Left (WrongOperand Or (renderValue other))
  -- ANCHOR_END: shortcut

  -- ANCHOR: orElse
  -- m ?: d looks at d only when m has no value: Nothing or Err.
  -- m ?: d は、m に値がないとき（Nothing か Err）だけ d を見る
  BinOp OrElse l r -> do
    lv <- eval env l
    case lv of
      VCon "Just" _ [v] -> Right v           -- Just v ?: d  is v
      VCon "Ok"   _ [v] -> Right v           -- Ok v ?: d    is v
      VCon _      _ _   -> eval env r        -- Nothing, Err e: d
      other             -> Left (WrongOperand OrElse (renderValue other))
  -- ANCHOR_END: orElse

  -- The other operators need both sides.
  -- ほかの演算子は、両側とも要る
  BinOp op l r -> do
    lv <- eval env l
    rv <- eval env r
    binOp op lv rv

  -- ANCHOR: let
  Let x e body -> do
    -- let is recursive: e can see x. env' holds v, and v is made
    -- from env'. Laziness ties this knot.
    -- let は再帰する：e から x が見える。env' は v を持ち、
    -- v は env' から作られる。遅延評価が、この結び目を結ぶ
    let env'   = Map.insert x v env
        result = eval env' e
        v      = either (const (VInt 0)) id result
    _ <- result
    eval env' body
  -- ANCHOR_END: let

  -- ANCHOR: case
  -- Try the branches from the top. The first pattern that fits the
  -- value wins, and its names are added to the environment.
  -- 枝を上から試す。値に合った最初のパターンの枝を使い、
  -- パターンの名前を環境に足す
  Case scrutinee branches -> do
    v <- eval env scrutinee
    let firstFit bs = case bs of
          [] -> Left (NoMatch (renderValue v))
          (p, body) : rest -> case match p v of
            Just names -> eval (Map.union (Map.fromList names) env) body
            Nothing    -> firstFit rest
    firstFit branches
  -- ANCHOR_END: case

  -- ANCHOR: evalRecord
  -- Compute the fields from left to right, and keep them by name.
  -- フィールドを左から計算し、名前で引けるようにしておく
  Record fields -> do
    vs <- traverse (eval env . snd) fields
    Right (VRecord (Map.fromList (zip (map fst fields) vs)))

  Field e x -> do
    v <- eval env e
    field x v

  -- A new record: the old one, with some fields changed. The old one
  -- stays as it was.
  -- 新しいレコード：古いもののフィールドをいくつか変えたもの。
  -- 古いものは、そのまま
  Update e fields -> do
    v  <- eval env e
    vs <- traverse (eval env . snd) fields
    case v of
      VRecord old -> Right (VRecord (Map.union
                       (Map.fromList (zip (map fst fields) vs)) old))
      other -> Left (NoField (T.intercalate ", " (map fst fields))
                             (renderValue other))
  -- ANCHOR_END: evalRecord
-- ANCHOR_END: eval

-- ANCHOR: match
-- 🧩 Does the value fit the pattern? If so, the names and their values.
--    Just x  and  Just 3  give  [(x, 3)].
--    Just x  and  Nothing  give Nothing.
-- 値はパターンに合うか。合うなら、名前とその値
match :: Pattern -> Value -> Maybe [(Name, Value)]
match p v = case (p, v) of
  (PVar x, _)                 -> Just [(x, v)]
  (PWild, _)                  -> Just []
  (PLit (LInt n), VInt m)     -> if n == m then Just [] else Nothing
  (PLit (LBool a), VBool b)   -> if a == b then Just [] else Nothing
  (PLit (LString a), VString b) -> if a == b then Just [] else Nothing
  -- ANCHOR: matchCon
  -- S.Circle r  fits  Circle 2: the value knows only the name inside
  -- its module.
  -- S.Circle r は Circle 2 に合う。値が知っているのは、モジュールの中の
  -- 名前だけ
  (PCon c ps, VCon c' _ vs)
    | baseName c == c'        -> matchAll ps vs
  -- ANCHOR_END: matchCon
  (PTuple ps, VTuple vs)      -> matchAll ps vs
  (PList ps, VList vs)        -> matchAll ps vs
  (PCons h t, VList (x : xs)) -> (<>) <$> match h x <*> match t (VList xs)
  -- ANCHOR: matchRecord
  -- { name, age }: each name gets the value of its field.
  -- それぞれの名前に、そのフィールドの値を付ける
  (PRecord xs, VRecord m)     -> traverse (\x -> (,) x <$> Map.lookup x m) xs
  -- ANCHOR_END: matchRecord
  _                           -> Nothing

-- 🧩 Every pattern fits its value, side by side (and as many of each).
--    Walk the two lists together, and stop when one ends first: the
--    pattern [] must not count the whole list (foldl would be slow).
-- パターンが、並んだ値にそれぞれ合う（数も同じ）。2つのリストを一緒に
-- たどり、片方が先に尽きたら合わない。[] のパターンで、リストの全部を
-- 数えないため（数えると foldl が遅くなる）
matchAll :: [Pattern] -> [Value] -> Maybe [(Name, Value)]
matchAll ps vs = case (ps, vs) of
  ([], [])           -> Just []
  (p : ps', v : vs') -> (<>) <$> match p v <*> matchAll ps' vs'
  _                  -> Nothing
-- ANCHOR_END: match

-- ANCHOR: fieldOf
-- 🔎 The value of one field.
-- フィールド1つの値
field :: Name -> Value -> Either EvalError Value
field x v = case v of
  VRecord m | Just fv <- Map.lookup x m -> Right fv
  _ -> Left (NoField x (renderValue v))
-- ANCHOR_END: fieldOf

-- ANCHOR: apply
-- 📞 Call a function: the body runs in the function's OWN environment,
--    plus the argument.
-- 関数を呼ぶ：本体は、関数「自身」の環境に、引数を足して動く
apply :: Value -> Value -> Either EvalError Value
apply (VClosure x body closed) arg = eval (Map.insert x arg closed) body
-- A builtin waits until it has all its arguments, then runs.
-- 組み込みの関数は、引数がそろうまで待ってから動く
apply (VPrim n arity run args) arg
  | length args' == arity = run args'
  | otherwise             = Right (VPrim n arity run args')
  where
    args' = args <> [arg]
apply other _ = Left (NotAFunction (renderValue other))
-- ANCHOR_END: apply

-- ANCHOR: binOp
-- ➕ The operators that need both sides.
-- 両側とも要る演算子
binOp :: Op -> Value -> Value -> Either EvalError Value
binOp op lv rv = case (op, lv, rv) of
  (Add, VInt a, VInt b) -> Right (VInt (a + b))
  (Sub, VInt a, VInt b) -> Right (VInt (a - b))
  (Mul, VInt a, VInt b) -> Right (VInt (a * b))
  -- ANCHOR: binOpFloat
  -- A Float on either side: both are Floats (the type checker said
  -- so), so a VInt there is turned into a Float first.
  -- どちらかが Float なら、両方とも Float（型検査器が決めた）。
  -- そこにある VInt は、先に Float にする
  (Add, _, _) | Just (a, b) <- floats -> Right (VFloat (a + b))
  (Sub, _, _) | Just (a, b) <- floats -> Right (VFloat (a - b))
  (Mul, _, _) | Just (a, b) <- floats -> Right (VFloat (a * b))
  -- / is always for Floats: 7 / 2 is 3.5, and 1 / 0 is Infinity.
  -- / はいつも Float の割り算。7 / 2 は 3.5、1 / 0 は Infinity
  (Div, _, _) | Just (a, b) <- numbers -> Right (VFloat (a / b))
  (DivMaybe, _, _) | Just (a, b) <- numbers ->
    Right (if b == 0 then nothing else just (VFloat (a / b)))
  -- // is for Ints, rounded down as Haskell's div and Python's //.
  -- // は Int の割り算。Haskell の div、Python の // と同じく下へ丸める
  (IntDiv, VInt a, VInt b)
    | b == 0            -> Left DivideByZero
    | otherwise         -> Right (VInt (a `div` b))
  (IntDivMaybe, VInt a, VInt b) ->
    Right (if b == 0 then nothing else just (VInt (a `div` b)))
  -- == < ... on two numbers, one of them a Float: as IEEE 754 says
  -- (NaN == NaN is false, and NaN < 1 is false, too).
  -- 片方が Float の2つの数を比べる：IEEE 754 のとおり
  (_, _, _) | Just test <- floatComparison op
            , Just (a, b) <- floats -> Right (VBool (test a b))
  -- ANCHOR_END: binOpFloat
  -- ANCHOR: concat
  (Concat, VString a, VString b) -> Right (VString (a <> b))
  (Concat, VList a,   VList b)   -> Right (VList (a <> b))
  -- ANCHOR_END: concat
  (Cons, x, VList xs)            -> Right (VList (x : xs))
  _ | Just test <- comparison op -> do
        o <- compareValues lv rv
        Right (VBool (test o))
    | otherwise -> Left (WrongOperand op (renderValue rv))
  where
    -- Both numbers, at least one a Float.
    -- 両方とも数で、少なくとも1つが Float
    floats = case (lv, rv) of
      (VInt _, VInt _) -> Nothing
      _                -> numbers
    -- Both numbers, as Doubles.
    -- 両方とも数。Double にして
    numbers = (,) <$> floatOf lv <*> floatOf rv
    -- Maybe, made here (Nothing is 0, Just is 1, as in the prelude).
    -- ここで作る Maybe（番号はプレリュードと同じ）
    nothing = VCon "Nothing" 0 []
    just v  = VCon "Just" 1 [v]

-- ANCHOR: floatOf
-- 🔢 A number as a Double: a Float, or an Int that is a Float here.
-- 数を Double にする。Float か、ここでは Float の Int
floatOf :: Value -> Maybe Double
floatOf v = case v of
  VFloat x -> Just x
  VInt n   -> Just (fromInteger n)
  _        -> Nothing
-- ANCHOR_END: floatOf

-- 🔍 The comparing operators on Doubles, as IEEE 754 has them.
-- Double を比べる演算子。IEEE 754 のとおり
floatComparison :: Op -> Maybe (Double -> Double -> Bool)
floatComparison op = case op of
  Eq -> Just (==)
  Ne -> Just (/=)
  Lt -> Just (<)
  Le -> Just (<=)
  Gt -> Just (>)
  Ge -> Just (>=)
  _  -> Nothing

-- 🔍 What each comparing operator wants to see.
-- 比べる演算子が、それぞれ見たい答え
comparison :: Op -> Maybe (Ordering -> Bool)
comparison op = case op of
  Eq -> Just (== EQ)
  Ne -> Just (/= EQ)
  Lt -> Just (== LT)
  Le -> Just (/= GT)
  Gt -> Just (== GT)
  Ge -> Just (/= LT)
  _  -> Nothing
-- ANCHOR_END: binOp

-- ANCHOR: compare
-- ⚖️ Compare two values of the same type. Lists and tuples are
--    compared from the left, like words in a dictionary.
--    Functions cannot be compared.
-- 同じ型の2つの値を比べる。リストとタプルは、辞書の言葉のように
-- 左から比べる。関数は比べられない
compareValues :: Value -> Value -> Either EvalError Ordering
compareValues = compareWith compare

-- ANCHOR: compareKeys
-- 🔑 Compare two keys of a Map or a Set. The same as compareValues,
--    but NaN is equal to NaN and larger than every other number, so
--    that a NaN key can be found again (NaN == NaN is false).
-- Map や Set のキーを比べる。compareValues と同じだが、NaN は NaN と
-- 等しく、ほかのどの数よりも大きい。NaN のキーをまた引けるように
-- （NaN == NaN は false なので）
compareKeys :: Value -> Value -> Either EvalError Ordering
compareKeys = compareWith keyOrder
  where
    keyOrder x y
      | isNaN x && isNaN y = EQ
      | isNaN x            = GT
      | isNaN y            = LT
      | otherwise          = compare x y
-- ANCHOR_END: compareKeys

-- ⚖️ Compare, with num as the way to compare two Floats.
-- 比べる。num は、2つの Float の比べ方
compareWith
  :: (Double -> Double -> Ordering)
  -> Value -> Value -> Either EvalError Ordering
compareWith num a b = case (a, b) of
  (VInt x,    VInt y)    -> Right (compare x y)
  -- Two numbers, one of them a Float (in a list, a tuple, a record).
  -- 片方が Float の2つの数（リストやタプル、レコードの中）
  _ | Just x <- floatOf a, Just y <- floatOf b -> Right (num x y)
  (VBool x,   VBool y)   -> Right (compare x y)
  (VString x, VString y) -> Right (compare x y)
  (VTuple xs, VTuple ys) -> compareLists num xs ys
  (VList xs,  VList ys)  -> compareLists num xs ys
  -- ANCHOR: compareMap
  -- Two maps: compare their (key, value) pairs, in the order of keys.
  -- Two sets: their values, in order.
  -- 2つの Map は、（キー, 値）の組を、キーの順に比べる。
  -- 2つの Set は、値を順に比べる
  (VMap xs,   VMap ys)   -> compareLists num (pairs xs) (pairs ys)
  (VSet xs,   VSet ys)   -> compareLists num (keys xs) (keys ys)
  -- ANCHOR_END: compareMap
  -- ANCHOR: compareCon
  -- The constructor written first is the smaller one. With the same
  -- constructor, compare the arguments from the left.
  -- 先に書いた構成子のほうが小さい。同じ構成子なら、引数を左から比べる
  (VCon _ i xs, VCon _ j ys)
    | i /= j    -> Right (compare i j)
    | otherwise -> compareLists num xs ys
  -- ANCHOR_END: compareCon
  -- ANCHOR: compareRecord
  -- Two records of the same type: the fields in the order of names.
  -- 同じ型の2つのレコード：フィールドを名前の順に比べる
  (VRecord xs, VRecord ys) -> compareLists num (Map.elems xs) (Map.elems ys)
  -- ANCHOR_END: compareRecord
  _                      -> Left (CannotCompare (renderValue a))

-- ANCHOR: pairs
-- 🔑 The pairs of a map, as Lam tuples, and the values of a set.
-- Map の組を Lam のタプルに。Set の値を並べる
pairs :: Map Key Value -> [Value]
pairs m = [ VTuple [k, v] | (Key k, v) <- Map.toList m ]

keys :: Set Key -> [Value]
keys s = [ k | Key k <- Set.toList s ]
-- ANCHOR_END: pairs

compareLists
  :: (Double -> Double -> Ordering)
  -> [Value] -> [Value] -> Either EvalError Ordering
compareLists num xs ys = case (xs, ys) of
  ([],      [])      -> Right EQ
  ([],      _)       -> Right LT
  (_,       [])      -> Right GT
  (x : xs', y : ys') -> do
    o <- compareWith num x y
    if o == EQ then compareLists num xs' ys' else Right o
-- ANCHOR_END: compare

-- ANCHOR: render
-- 🖨️ A value, as the REPL shows it.
-- REPL が見せる値
renderValue :: Value -> Text
renderValue v = case v of
  VInt n         -> T.pack (show n)
  -- 1.5, 0.30000000000000004, 1e+21, as JavaScript and Elm show them
  -- JavaScript や Elm と同じ形
  VFloat x       -> showFloat x
  VBool True     -> "true"
  VBool False    -> "false"
  VString s      -> renderString s
  VTuple vs      -> "(" <> T.intercalate ", " (map renderValue vs) <> ")"
  VList vs       -> "[" <> T.intercalate ", " (map renderValue vs) <> "]"
  -- ANCHOR: renderMap
  -- {"Python": 82, "TS": 75}, the same as it is written. A key that
  -- is not a string or a number is put in [ ]:  {[(1, 2)]: "a"}
  -- 書くときと同じ形。文字列でも数でもないキーは [ ] で包む
  VMap m         -> "{" <> T.intercalate ", "
                      [ renderKey k <> ": " <> renderValue x
                      | (Key k, x) <- Map.toList m ] <> "}"
  -- Set.fromList [1, 2], as Haskell and Elm show a set.
  -- Haskell や Elm と同じ形
  VSet s         -> "Set.fromList " <> renderValue (VList (keys s))
  -- ANCHOR_END: renderMap
  -- ANCHOR: renderCon
  -- Just (Just 3): an argument with arguments of its own needs ( ).
  -- 引数を持つ引数は、括弧で包む
  VCon c _ []    -> c
  VCon c _ vs    -> T.unwords (c : map argument vs)
  -- ANCHOR_END: renderCon
  -- ANCHOR: renderRecord
  -- { age = 3, name = "Lam" }, the same as it is written (by name).
  -- 書くときと同じ形（名前の順）
  VRecord m      -> "{ " <> T.intercalate ", "
                      [ k <> " = " <> renderValue x | (k, x) <- Map.toList m ]
                      <> " }"
  -- ANCHOR_END: renderRecord
  VClosure x _ _ -> "<function of " <> x <> ">"
  VPrim n _ _ _  -> "<builtin " <> n <> ">"
  where
    argument a = case a of
      VCon _ _ (_ : _) -> "(" <> renderValue a <> ")"
      VSet _           -> "(" <> renderValue a <> ")"
      -- Just (-3), as Haskell shows it.
      -- Haskell と同じく、負の数も括弧で包む
      VInt n | n < 0   -> "(" <> renderValue a <> ")"
      VFloat x | x < 0 -> "(" <> renderValue a <> ")"
      _                -> renderValue a

-- ANCHOR: renderKey
-- 🔑 A key of a map: a string or a number as it is, others in [ ].
-- Map のキー。文字列と数はそのまま、ほかは [ ] で包む
renderKey :: Value -> Text
renderKey k = case k of
  VString s -> renderString s
  VInt n    -> T.pack (show n)
  VFloat x  -> showFloat x
  _         -> "[" <> renderValue k <> "]"
-- ANCHOR_END: renderKey

-- 📣 An error, as people read it.
-- 人が読むエラー
renderError :: EvalError -> Text
renderError err = case err of
  -- 「知らない変数です」
  UnboundVariable x  -> "unknown variable: " <> x
  -- 「関数ではないので、呼べません」
  NotAFunction v     -> "not a function, cannot call: " <> v
  -- 「この演算子には使えません」
  WrongOperand op v  -> opSymbol op <> " cannot use " <> v
  -- 「if の条件は真偽値でなければなりません」
  IfNeedsBool v      -> "if needs true or false, but got " <> v
  -- 「0 で割りました」
  DivideByZero       -> "divided by zero"
  -- 「関数は比べられません」
  CannotCompare v    -> "cannot compare a function: " <> v
  -- 「（組み込みの関数）：……」
  BuiltinFailed n m  -> n <> ": " <> m
  -- 「case のどの枝にも合いません」
  NoMatch v          -> "no branch of case fits " <> v
  -- 「{ age = 3 } には、フィールド name がありません」
  NoField x v        -> v <> " has no field " <> x
-- ANCHOR_END: render
-- ANCHOR_END: all
