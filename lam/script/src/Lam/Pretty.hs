{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
-- ANCHOR: all
-- | Turn an AST back into Lam source code, with as few
--   parentheses as possible.
-- AST を、Lam のソースコードに戻す。括弧はできるだけ少なく。
module Lam.Pretty
  ( render
  , renderString
  , renderPattern
  , opSymbol
  , showFloat
  , floatLiteral
  , renderTypeExpr
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Numeric (floatToDigits)

import Lam.Syntax

-- ANCHOR: levels
-- 🪜 How tightly each kind of expression holds together.
--    A bigger number holds tighter: 2 * 3 + 4 is (2 * 3) + 4.
-- 式の種類ごとの、結びつきの強さ。大きいほど強く結びつく
level :: Expr -> Int
level expr = case expr of
  -- A mark of a place is not shown: the expression inside decides.
  -- 場所の印は見せない。中の式で決まる
  At _ e        -> level e
  Lam {}        -> 0   -- \x -> ...     （右の端まで続く）
  Let {}        -> 0   -- let ... in ...
  If {}         -> 0   -- if ... then ... else ...
  Case {}       -> 0   -- case ... of | ...
  BinOp op _ _  -> opLevel op
  App {}        -> 8   -- f x
  Lit _         -> 9   -- 42
  Var _         -> 9   -- x
  Tuple _       -> 9   -- (1, 2)    has its own parentheses / 自前の括弧がある
  List _        -> 9   -- [1, 2]
  Record _      -> 9   -- { name = "Lam" }
  Field _ _     -> 9   -- r.name
  Update _ _    -> 9   -- { r | age = 4 }

-- The same order as Haskell: || < && < == < : ++ < + - < * /
--    ?: goes between == and ++, as in Kotlin.
-- Haskell と同じ順番。?: は、Kotlin と同じく == と ++ のあいだ
opLevel :: Op -> Int
opLevel op = case op of
  Or          -> 1
  And         -> 2
  Eq          -> 3
  Ne          -> 3
  Lt          -> 3
  Le          -> 3
  Gt          -> 3
  Ge          -> 3
  OrElse      -> 4
  Concat      -> 5
  Cons        -> 5
  Add         -> 6
  Sub         -> 6
  Mul         -> 7
  Div         -> 7
  IntDiv      -> 7
  DivMaybe    -> 7
  IntDivMaybe -> 7

opSymbol :: Op -> Text
opSymbol op = case op of
  Add    -> "+"
  Sub    -> "-"
  Mul    -> "*"
  Div    -> "/"
  IntDiv -> "//"
  DivMaybe    -> "/?"
  IntDivMaybe -> "//?"
  OrElse -> "?:"
  Eq     -> "=="
  Ne     -> "/="
  Lt     -> "<"
  Le     -> "<="
  Gt     -> ">"
  Ge     -> ">="
  Concat -> "++"
  Cons   -> ":"
  And    -> "&&"
  Or     -> "||"
-- ANCHOR_END: levels

-- ANCHOR: render
-- 🖨️ The source code of an expression.
-- 式のソースコード
render :: Expr -> Text
render = at 0

-- Render e where at least level p is needed: if e holds looser
-- than that, wrap it in parentheses.
-- 少なくとも強さ p が要る場所に e を書く。e がそれより弱ければ、括弧で包む
at :: Int -> Expr -> Text
at p e
  | level e < p = "(" <> plain e <> ")"
  | otherwise   = plain e

plain :: Expr -> Text
plain expr = case expr of
  At _ e            -> plain e
  Lit (LInt n)      -> T.pack (show n)
  Lit (LFloat x)    -> floatLiteral x
  Lit (LBool True)  -> "true"
  Lit (LBool False) -> "false"
  Lit (LString s)   -> renderString s
  Var x             -> x
  -- ANCHOR: renderAccessor
  -- .name is sugar for  \$r -> $r.name: show it as it was written.
  -- .name は \$r -> $r.name の糖衣構文。書いたとおりに見せる
  Lam "$r" (Field (Var "$r") x) -> "." <> x
  -- ANCHOR_END: renderAccessor
  Lam x body        -> "\\" <> x <> " -> " <> at 0 body
  App f a           -> at 8 f <> " " <> at 9 a
  Let x e body      -> "let " <> x <> " = " <> at 0 e <> " in " <> at 0 body
  If c a b          -> "if " <> at 0 c <> " then " <> at 0 a
                         <> " else " <> at 0 b
  Tuple es          -> "(" <> T.intercalate ", " (map (at 0) es) <> ")"
  List es           -> "[" <> T.intercalate ", " (map (at 0) es) <> "]"
  BinOp op l r      ->
    let n = opLevel op
        -- How each operator leans. 1 - 2 - 3 is (1 - 2) - 3,
        -- 1 : 2 : xs is 1 : (2 : xs), and a == b == c is not allowed.
        -- 演算子の寄り方。+ - * / は左、: ++ && || は右に寄る。
        -- == などは続けられないので、両側とも強さが要る
        (lp, rp)
          | n == 3        = (n + 1, n + 1)
          | leansRight op = (n + 1, n)
          | otherwise     = (n, n + 1)
        leansRight o = o `elem` [OrElse, Concat, Cons, And, Or]
    in at lp l <> " " <> opSymbol op <> " " <> at rp r
  -- ANCHOR: renderCase
  -- A branch body that reaches to the right end (\, let, if, case)
  -- would swallow the next | , so it gets ( ), except in the last.
  -- 右の端まで続く枝（\、let、if、case）は、次の | を飲みこんでしまう
  -- ので、最後の枝のほかは括弧で包む
  Case e alts       ->
    let n = length alts
        branch i (p, body) =
          " | " <> renderPattern p <> " -> "
            <> at (if i < n then 1 else 0) body
    in "case " <> at 0 e <> " of" <> T.concat (zipWith branch [1 ..] alts)
  -- ANCHOR_END: renderCase
  -- ANCHOR: renderRecordExpr
  Record fs         -> "{ " <> fieldsOf fs <> " }"
  -- r.name, and (f x).name: only a name can go before the dot as it is.
  -- 点の前にそのまま置けるのは、名前だけ
  Field e x         -> beforeDot e <> "." <> x
  Update e fs       -> "{ " <> plain e <> " | " <> fieldsOf fs <> " }"
  -- ANCHOR_END: renderRecordExpr
  where
    fieldsOf fs = T.intercalate ", " [ x <> " = " <> at 0 e | (x, e) <- fs ]
    beforeDot e = case e of
      Var _     -> plain e
      Field _ _ -> plain e
      _         -> "(" <> plain e <> ")"
-- ANCHOR_END: render

-- ANCHOR: renderPattern
-- 🧩 A pattern as Lam source code:  Just (x, _) : rest
-- パターンを、Lam のソースコードにする
renderPattern :: Pattern -> Text
renderPattern = go 0
  where
    -- 0: anywhere, 1: left of :, 2: argument of a constructor
    -- 0：どこでも、1：: の左、2：構成子の引数
    go :: Int -> Pattern -> Text
    go p pat = case pat of
      PVar x       -> x
      PWild        -> "_"
      PLit l       -> plain (Lit l)
      PCon c []    -> c
      PCon c ps    -> wrap (p >= 2) (T.unwords (c : map (go 2) ps))
      PTuple ps    -> "(" <> T.intercalate ", " (map (go 0) ps) <> ")"
      PList ps     -> "[" <> T.intercalate ", " (map (go 0) ps) <> "]"
      PCons h t    -> wrap (p >= 1) (go 1 h <> " : " <> go 0 t)
      PRecord xs   -> "{ " <> T.intercalate ", " xs <> " }"
    wrap yes t = if yes then "(" <> t <> ")" else t
-- ANCHOR_END: renderPattern

-- ANCHOR: string
-- 🧵 A string as Lam source code: "say \"hi\"\n".
--    Only " \ newline and tab need a backslash.
-- 文字列を、Lam のソースコードにする。
-- バックスラッシュが要るのは、" \ 改行 タブだけ
renderString :: Text -> Text
renderString s = "\"" <> T.concatMap escape s <> "\""
  where
    escape c = case c of
      '"'  -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      _    -> T.singleton c
-- ANCHOR_END: string

-- ANCHOR: showFloat
-- 🔢 A Float, as JavaScript (and Elm) shows it: the fewest digits that
--    give the same Float back, and no ".0" for a whole number.
--      1.0 is "1", 0.1 + 0.2 is "0.30000000000000004",
--      1e21 is "1e+21", 0.0000001 is "1e-7"
-- Float を、JavaScript（と Elm）と同じ形で見せる。同じ Float に戻る
-- いちばん少ない桁で、整数なら ".0" を付けない
showFloat :: Double -> Text
showFloat x
  | isNaN x      = "NaN"
  | isInfinite x = if x > 0 then "Infinity" else "-Infinity"
  | x == 0       = "0"                    -- -0 is shown as 0, too
  | x < 0        = "-" <> showFloat (negate x)
  | otherwise    =
      -- floatToDigits 10 x: the digits, and where the point goes.
      --   12.5 is ([1,2,5], 2): 0.125 times 10 to the 2
      -- 数字の並びと、小数点の場所
      let (ds, n) = floatToDigits 10 x
          digits  = T.pack (concatMap show ds)
          k       = length ds
          -- for 1.25e-7 and 1e+21: the digits after the first, and
          -- the power of 10 after the e
          -- 1.25e-7 や 1e+21 のため：2けた目からの数字と、e のあとの
          -- 10 の何乗か
          rest    = if k == 1 then "" else "." <> T.drop 1 digits
          e       = n - 1
          sign    = if e < 0 then "-" else "+"
      in if
           -- 1200: the digits, then zeros
           -- 数字のあとに 0 を並べる
           | k <= n && n <= 21 -> digits <> T.replicate (n - k) "0"
           -- 12.5: the point inside the digits
           -- 数字の途中に小数点
           | 0 < n && n <= 21  -> T.take n digits <> "." <> T.drop n digits
           -- 0.0125: "0." and zeros before the digits
           -- 「0.」と 0 のあとに数字
           | -6 < n && n <= 0  -> "0." <> T.replicate (negate n) "0" <> digits
           -- 1.25e-7, 1e+21: one digit before the point
           -- 小数点の前に1けた
           | otherwise         ->
               T.take 1 digits <> rest <> "e" <> sign <> T.pack (show (abs e))
-- ANCHOR_END: showFloat

-- ANCHOR: floatLiteral
-- 🔢 A Float literal in source code: the same, but always with a point
--    or an e, so that it is read back as a Float (1 would be a number
--    of any type).
-- ソースコードの中の Float のリテラル。読み戻して Float になるように、
-- 点か e をいつも入れる
floatLiteral :: Double -> Text
floatLiteral x
  | T.any (`elem` (".e" :: String)) shown = shown
  | otherwise                             = shown <> ".0"
  where
    shown = showFloat x
-- ANCHOR_END: floatLiteral
-- ANCHOR: renderTypeExpr
-- 🖨️ A type as written (with its own names), in a place of this
--    strength: 0 alone, 1 left of ->, 2 an argument (Maybe a needs ( )).
-- 書いたとおりの型（書いた名前のまま）。強さ 0 は単独、1 は -> の左、
-- 2 は引数（Maybe a には括弧が要る）
renderTypeExpr :: Int -> TypeExpr -> Text
renderTypeExpr strength te = case te of
  TyVar a     -> a
  TyApp f ts  -> wrap 2 (T.unwords (f : map (renderTypeExpr 2) ts))
  TyCon c []  -> c
  TyCon c ts  -> wrap 2 (T.unwords (c : map (renderTypeExpr 2) ts))
  TyFun a b   -> wrap 1 (renderTypeExpr 1 a <> " -> " <> renderTypeExpr 0 b)
  TyList t    -> "[" <> renderTypeExpr 0 t <> "]"
  TyTuple ts  -> "(" <> T.intercalate ", " (map (renderTypeExpr 0) ts) <> ")"
  TyRecord fs r -> "{ " <> maybe "" (<> " | ") r
    <> T.intercalate ", " [ f <> " : " <> renderTypeExpr 0 t | (f, t) <- fs ]
    <> " }"
  where
    wrap needed t = if strength >= needed then "(" <> t <> ")" else t
-- ANCHOR_END: renderTypeExpr
-- ANCHOR_END: all
