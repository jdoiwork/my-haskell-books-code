{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The evaluator: walk the tree and compute a value.
-- 評価器：木をたどって、値を計算する。
module Lam.Eval
  ( Value (..)
  , Env
  , EvalError (..)
  , Action (..)
  , Request (..)
  , LamParser
  , Farthest
  , eval
  , apply
  , compareValues
  , located
  , binOp
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
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State.Strict (StateT)
import Data.Void (Void)
import Text.Megaparsec (ParseError, ParsecT)

import Lam.Pcre (Regex (..))
import Lam.Pretty (opSymbol, renderString)
import Lam.Syntax

-- ANCHOR: value
-- 💎 What a Lam program computes.
-- Lam のプログラムが計算するもの
data Value
  = VInt Integer
  | VBool Bool
  | VString Text           -- "hello"
  | VTuple [Value]         -- (1, "one"), and () (unit)
  | VList [Value]          -- [1, 2, 3]
  -- ANCHOR: vmap
  -- {"Python": 82, "TS": 75}: the keys are strings, kept in order.
  -- キーは文字列。順に並べて持つ
  | VMap (Map Text Value)
  -- ANCHOR_END: vmap
  -- ANCHOR: vcon
  -- A value made with a constructor: Just 3, Circle 2, Nothing.
  -- The number is the constructor's place in the declaration
  -- (Nothing is 0, Just is 1), for comparing.
  -- 構成子で作った値。番号は、宣言の中での構成子の順番（比べるのに使う）
  | VCon Name Int [Value]
  -- ANCHOR_END: vcon
  -- A function, together with the variables it could see when it
  -- was made (a closure).
  -- 関数と、それが作られたときに見えていた変数たち（クロージャ）
  | VClosure Name Expr Env
  -- A function written in Haskell: its name, how many arguments
  -- it needs, what it does, and the arguments given so far.
  -- Haskell で書いた関数：名前、要る引数の数、すること、ここまでの引数
  | VPrim Name Int ([Value] -> Either EvalError Value) [Value]
  -- ANCHOR: vio
  -- An IO action: a request to the outside world, not yet done.
  -- IO の値：外の世界への頼みごと。まだ、やっていない
  | VIO Action
  -- ANCHOR_END: vio
  -- ANCHOR: vregex
  -- A compiled regular expression:  re"\d+".
  -- コンパイルした正規表現
  | VRegex Regex
  -- ANCHOR_END: vregex
  -- ANCHOR: vparser
  -- A parser, made of megaparsec: it reads a string and gives a value.
  -- パーサー。中身は megaparsec：文字列を読んで、値をくれる
  | VParser LamParser
  -- ANCHOR_END: vparser

-- ANCHOR: lamParser
-- 🧩 The parser inside a Parser value. It reads Text, and calls Lam
--    functions as it goes (Parser.map, Parser.andThen). Under it are
--    two more things:
--    - Farthest: the error that got the farthest so far. A choice
--      that failed is forgotten when the next one is tried, but its
--      error is kept here, for a better message at the end.
--    - Either EvalError: a Lam function can fail (1 / 0), and a Left
--      stops the whole run. It is an error of the program, not a
--      string that does not fit.
-- Parser の値の中のパーサー。Text を読みながら、Lam の関数を呼ぶ
-- （Parser.map、Parser.andThen）。下に、もう2つ重ねる：
-- ・Farthest：ここまでで、いちばん遠くまで読めたエラー。失敗した選択肢は、
--   次を試すときに忘れるが、エラーはここに残し、最後のエラーの文に使う
-- ・Either EvalError：Lam の関数は失敗しうる（1 / 0）。Left は、読むのを
--   丸ごと止める。文字列が合わないのではなく、プログラムのまちがいだから
type LamParser = ParsecT Void Text (StateT Farthest (Either EvalError)) Value

type Farthest = Maybe (ParseError Text Void)
-- ANCHOR_END: lamParser

-- ANCHOR: action
-- 📮 What an IO action asks for. Only the runner does it (Lam.Io).
-- IO の値が頼むこと。やるのは、実行役だけ（Lam.Io）
data Action
  = Pure Value              -- pure 3: ask for nothing, the answer is 3
  | Ask Request             -- putStrLn "hi": one request
  -- getLine, and then what to do with the line it gives.
  -- getLine と、その行を受け取ってから、することの続き
  | Bind Action (Value -> Either EvalError Action)
  -- ANCHOR: exitAction
  -- exit 2: not a request to the world, but a sign to the runner:
  -- stop here, and do not do the rest.
  -- exit 2：世界への頼みごとではなく、実行役への合図。ここで止まり、
  -- 続きはやらない
  | Exit Int
  -- ANCHOR_END: exitAction

-- 📨 One request to the outside world.
-- 外の世界への、1つの頼みごと
data Request
  = PutStr Text             -- write to the screen
  | GetLine                 -- read a line (Nothing at the end)
  | ReadFile Text           -- read a file
  | WriteFile Text Text     -- write a file
  -- ANCHOR: scriptRequests
  | GetArgs                 -- the arguments after the file name
  | LookupEnv Text          -- one environment variable
  | GetEnvironment          -- all the environment variables
  | GetContents             -- read all of the standard input
  | PutErr Text             -- write to the standard error
  -- ANCHOR_END: scriptRequests
  deriving (Eq, Show)
-- ANCHOR_END: action

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
  -- ANCHOR: locatedError
  -- The same error, and where it happened (the innermost place).
  -- 同じエラーと、それが起きた場所（いちばん内側の場所）
  | Located Loc EvalError
  -- ANCHOR_END: locatedError
  deriving (Eq, Show)
-- ANCHOR_END: value

-- ANCHOR: eval
-- ⚙️ The value of an expression, in an environment.
-- 環境の中での、式の値
eval :: Env -> Expr -> Either EvalError Value
eval env expr = case expr of
  Lit (LInt n)    -> Right (VInt n)
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

  -- ANCHOR: evalAt
  -- A mark of a place: the value of the expression inside. If it
  -- fails, the error remembers this place.
  -- 場所の印：中の式の値。失敗したら、エラーにこの場所を覚えさせる
  At loc e -> located loc (eval env e)
  -- ANCHOR_END: evalAt

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
-- ANCHOR_END: eval

-- ANCHOR: located
-- 📍 Put the place on an error. An error that already has a place
--    keeps it: the innermost place is the most exact.
-- エラーに場所を付ける。もう場所のあるエラーは、そのまま。
-- いちばん内側の場所が、いちばんくわしいから
located :: Loc -> Either EvalError a -> Either EvalError a
located loc result = case result of
  Left err@(Located _ _) -> Left err
  Left err               -> Left (Located loc err)
  Right v                -> Right v
-- ANCHOR_END: located

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
  (PCon c ps, VCon c' _ vs)
    | c == c'                 -> matchAll ps vs
  (PTuple ps, VTuple vs)      -> matchAll ps vs
  (PList ps, VList vs)        -> matchAll ps vs
  (PCons h t, VList (x : xs)) -> (<>) <$> match h x <*> match t (VList xs)
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
  (Div, VInt a, VInt b)
    | b == 0            -> Left DivideByZero
    | otherwise         -> Right (VInt (a `div` b))
  -- ANCHOR: concat
  (Concat, VString a, VString b) -> Right (VString (a <> b))
  (Concat, VList a,   VList b)   -> Right (VList (a <> b))
  -- ANCHOR_END: concat
  (Cons, x, VList xs)            -> Right (VList (x : xs))
  _ | Just test <- comparison op -> do
        o <- compareValues lv rv
        Right (VBool (test o))
    | otherwise -> Left (WrongOperand op (renderValue rv))

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
compareValues a b = case (a, b) of
  (VInt x,    VInt y)    -> Right (compare x y)
  (VBool x,   VBool y)   -> Right (compare x y)
  (VString x, VString y) -> Right (compare x y)
  (VTuple xs, VTuple ys) -> compareLists xs ys
  (VList xs,  VList ys)  -> compareLists xs ys
  -- ANCHOR: compareMap
  -- Two maps: compare their (key, value) pairs, in the order of keys.
  -- 2つの Map は、（キー, 値）の組を、キーの順に比べる
  (VMap xs,   VMap ys)   -> compareLists (pairs xs) (pairs ys)
  -- ANCHOR_END: compareMap
  -- ANCHOR: compareCon
  -- The constructor written first is the smaller one. With the same
  -- constructor, compare the arguments from the left.
  -- 先に書いた構成子のほうが小さい。同じ構成子なら、引数を左から比べる
  (VCon _ i xs, VCon _ j ys)
    | i /= j    -> Right (compare i j)
    | otherwise -> compareLists xs ys
  -- ANCHOR_END: compareCon
  _                      -> Left (CannotCompare (renderValue a))

-- ANCHOR: pairs
-- 🔑 The pairs of a map, as Lam tuples.
-- Map の組を、Lam のタプルにする
pairs :: Map Text Value -> [Value]
pairs m = [ VTuple [VString k, v] | (k, v) <- Map.toList m ]
-- ANCHOR_END: pairs

compareLists :: [Value] -> [Value] -> Either EvalError Ordering
compareLists xs ys = case (xs, ys) of
  ([],      [])      -> Right EQ
  ([],      _)       -> Right LT
  (_,       [])      -> Right GT
  (x : xs', y : ys') -> do
    o <- compareValues x y
    if o == EQ then compareLists xs' ys' else Right o
-- ANCHOR_END: compare

-- ANCHOR: render
-- 🖨️ A value, as the REPL shows it.
-- REPL が見せる値
renderValue :: Value -> Text
renderValue v = case v of
  VInt n         -> T.pack (show n)
  VBool True     -> "true"
  VBool False    -> "false"
  VString s      -> renderString s
  VTuple vs      -> "(" <> T.intercalate ", " (map renderValue vs) <> ")"
  VList vs       -> "[" <> T.intercalate ", " (map renderValue vs) <> "]"
  -- ANCHOR: renderMap
  -- {"Python": 82, "TS": 75}, the same as it is written.
  -- 書くときと同じ形
  VMap m         -> "{" <> T.intercalate ", "
                      [ renderString k <> ": " <> renderValue x
                      | (k, x) <- Map.toList m ] <> "}"
  -- ANCHOR_END: renderMap
  -- ANCHOR: renderCon
  -- Just (Just 3): an argument with arguments of its own needs ( ).
  -- 引数を持つ引数は、括弧で包む
  VCon c _ []    -> c
  VCon c _ vs    -> T.unwords (c : map argument vs)
  -- ANCHOR_END: renderCon
  VClosure x _ _ -> "<function of " <> x <> ">"
  VPrim n _ _ _  -> "<builtin " <> n <> ">"
  -- ANCHOR: renderIO
  VIO _          -> "<IO action>"
  -- ANCHOR_END: renderIO
  -- ANCHOR: renderRegex
  -- re"\d+", the same as it is written.
  -- 書くときと同じ形
  VRegex p       -> "re\"" <> regexSource p <> "\""
  -- ANCHOR_END: renderRegex
  VParser _      -> "<parser>"
  -- ANCHOR: argument
  where
    argument a = case a of
      VCon _ _ (_ : _) -> "(" <> renderValue a <> ")"
      -- JInt (-5), as Haskell shows it.
      -- Haskell と同じく、負の数も括弧で包む
      VInt n | n < 0   -> "(" <> renderValue a <> ")"
      _                -> renderValue a
  -- ANCHOR_END: argument

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
  -- 「場所：……」
  Located loc e      -> renderLoc loc <> ": " <> renderError e
-- ANCHOR_END: render
-- ANCHOR_END: all
