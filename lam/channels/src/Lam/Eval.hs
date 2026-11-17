{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The evaluator: walk the tree and compute a value.
-- 評価器：木をたどって、値を計算する。
module Lam.Eval
  ( Value (..)
  , Env
  , EvalError (..)
  , Action (..)
  , Early (..)
  , Stop (..)
  , Task (..)
  , Chan (..)
  , RealBox (..)
  , Pid (..)
  , Matcher
  , taskId
  , renderChan
  , Request (..)
  , OpenMode (..)
  , Cell (..)
  , LamParser
  , Farthest
  , eval
  , apply
  , compareValues
  , floatOf
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
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TVar)
import Data.Sequence (Seq)
import Control.Monad.State.Strict (StateT)
import Data.Void (Void)
import Text.Megaparsec (ParseError, ParsecT)

import Lam.Pcre (Regex (..))
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
  -- ANCHOR: vio
  -- An IO action: a request to the outside world, not yet done.
  -- IO の値：外の世界への頼みごと。まだ、やっていない
  | VIO Action
  -- ANCHOR: vtask
  -- A job started with spawn, that may still be running.
  -- spawn で始めた仕事。まだ動いているかもしれない
  | VTask Task
  -- ANCHOR_END: vtask
  -- ANCHOR: vchan
  -- A channel (Chan.new 0), and the address of a mailbox (a Pid).
  -- チャネル（Chan.new 0）と、受け箱の宛先（Pid）
  | VChan Chan
  | VPid Pid
  -- ANCHOR_END: vchan
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
  -- ANCHOR: togetherActions
  -- IO.all, IO.allN 4, IO.allOk: do the actions at once (at most n;
  -- 0 is no limit), and wait for them. A sign to the runner, like Exit.
  -- IO.race: do them at once; the first to end wins.
  -- IO.all・IO.allN 4・IO.allOk：頼みごとをまとめて同時にやり（多くて
  -- n 個。0 は上限なし）、待つ。Exit と同じく、実行役への合図。
  -- IO.race：まとめて同時にやり、最初に終わったものが勝ち
  | Gather Int Early [Action]
  | Race [Action]
  -- spawn io: start io, and go on at once; the answer is the Task.
  -- await t: wait for the task, and take its answer.
  -- spawn io：io を始めて、すぐに先へ進む。答えは Task。
  -- await t：仕事を待ち、その答えを受け取る
  | Spawn Action
  | Await Task
  -- ANCHOR_END: togetherActions
  -- ANCHOR: chanActions
  -- Chan.new n, Chan.send c v, Chan.recv c, Chan.close c. They may
  -- wait for another job, so they are signs to the runner, too.
  -- Chan.new n・Chan.send c v・Chan.recv c・Chan.close c。ほかの仕事を
  -- 待つことがあるので、これも実行役への合図
  | NewChan Int
  | Send Chan Value
  | Receive Chan
  | Close Chan
  -- ANCHOR_END: chanActions
  -- ANCHOR: mailActions
  -- receive me | P -> a | Q -> b after 500 -> c : take the first
  -- message that fits (the others stay), or after 500 ms, do c.
  -- receive me | P -> a | Q -> b after 500 -> c：合う最初のメッセージを
  -- 取る（ほかは残る）。500 ミリ秒たっても来なければ c
  | Select Chan Matcher (Maybe (Integer, Action))
  -- Process.spawnOn box f : a job of its own, with box as its mailbox.
  -- Process.monitor pid me f : when the job of pid ends, put f down
  -- (Normal or Crashed why) in me.
  -- Process.spawnOn box f：box を受け箱にした、自分の仕事を始める。
  -- Process.monitor pid me f：pid の仕事が終わったら、f down（Normal か
  -- Crashed why）を me に入れる
  | Start Chan Action
  | Monitor Pid (Value -> Either EvalError Value) Chan
  -- ANCHOR_END: mailActions

-- ANCHOR: matcher
-- 🧲 The branches of receive, as one function: Just the action of the
--    first branch that fits the message, or Nothing (leave it there).
-- receive の枝を1つの関数にしたもの：メッセージに合う最初の枝の頼みごとを
-- Just で。合わなければ Nothing（受け箱に残す）
type Matcher = Value -> Either EvalError (Maybe Action)
-- ANCHOR_END: matcher

-- ANCHOR: task
-- 🧵 A job started with spawn. In the real world, a thread of async;
--    in a fake world, a job that has already run: when it ended (on
--    the fake clock), and its answer.
-- spawn で始めた仕事。本物の世界では async のスレッド。偽物の世界では、
-- もう動き終わった仕事：（偽の時計で）いつ終わったかと、その答え
data Task
  = RealTask Int (Async (Either Stop Value))
  | FakeTask Int
-- ANCHOR_END: task

-- 🔢 The number of a task, for people: job #3.
-- 仕事の番号（人が読むため）：job #3
taskId :: Task -> Int
taskId t = case t of
  RealTask n _ -> n
  FakeTask n   -> n

-- ANCHOR: chan
-- 📬 A channel. Its number is for people (<chan #3>). Its capacity:
--    0 hands each value over (the sender waits for a receiver), n
--    keeps up to n, and -1 keeps any number (a mailbox).
--    The values are in a RealBox in the real world; a fake world keeps
--    them in itself, by the number.
-- チャネル。番号は人が読むため（<chan #3>）。容量：0 は1つずつ手渡す
-- （送り手は受け手を待つ）、n は n 個までためる、-1 はいくつでも（受け箱）。
-- 値は、本物の世界では RealBox に。偽物の世界は、番号で自分の中に持つ
data Chan = Chan
  { chanId       :: Int
  , chanCapacity :: Int
  , chanBox      :: Maybe RealBox
  }

-- 📦 The values of a real channel, in STM variables (TVar): the
--    values waiting, closed or not, and how many were sent and taken
--    (a sender of Chan.new 0 waits until its value is taken).
-- 本物のチャネルの中身。STM の変数（TVar）に：待っている値、閉じたか、
-- 送った数と取った数（Chan.new 0 の送り手は、自分の値が取られるまで待つ）
data RealBox = RealBox
  { boxItems  :: TVar (Seq Value)
  , boxClosed :: TVar Bool
  , boxSent   :: TVar Integer
  , boxTaken  :: TVar Integer
  }

-- 📮 The address of a mailbox (Pid msg), and the job that reads it,
--    if it was started with Process.spawn (Process.new has no job).
-- 受け箱の宛先（Pid msg）と、それを読む仕事（Process.spawn で始めたとき。
-- Process.new には仕事がない）
data Pid = Pid
  { pidChan :: Chan
  , pidJob  :: Maybe Task
  }
-- ANCHOR_END: chan

-- ANCHOR: renderChan
-- 🏷️ A channel or a mailbox, for people.
-- 人が読む、チャネルか受け箱
renderChan :: Chan -> Text
renderChan c
  | chanCapacity c < 0 = "<pid #" <> T.pack (show (chanId c)) <> ">"
  | otherwise          = "<chan #" <> T.pack (show (chanId c)) <> ">"
-- ANCHOR_END: renderChan

-- ANCHOR: stop
-- 🛑 Why the runner stopped before the end: an error, or exit.
-- 実行役が、最後まで行かずに止まったわけ：エラーか、exit か
data Stop
  = Failed EvalError
  | Exited Int
  deriving (Eq, Show)
-- ANCHOR_END: stop

-- ANCHOR: early
-- 🛎️ When to stop the others: only when one stops the program (an
--    error, exit), or also at the first Err (IO.allOk).
-- ほかを取り消すとき：プログラムが止まるとき（エラー、exit）だけか、
-- 最初の Err でも（IO.allOk）か
data Early = WaitAll | StopAtErr
  deriving (Eq, Show)
-- ANCHOR_END: early

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
  -- ANCHOR: httpRequest
  -- HTTP: the method, the URL, the headers, and the body.
  -- HTTP：メソッド、URL、ヘッダー、本文
  | HttpSend Text Text [(Text, Text)] Text
  -- ANCHOR_END: httpRequest
  -- ANCHOR: sqlRequests
  -- SQLite: open a file (":memory:" for one in memory), then ask the
  -- database with its number. The ? in the SQL are the cells.
  -- SQLite：ファイルを開き（":memory:" ならメモリーの中に）、あとは
  -- データベースの番号で頼む。SQL の ? には、マスの値が入る
  | SqlOpen Text OpenMode
  | SqlQuery Int Text [Cell]
  | SqlExec Int Text [Cell]
  | SqlClose Int
  -- ANCHOR_END: sqlRequests
  -- ANCHOR: fileRequests
  -- Files and folders: look, then change.
  -- ファイルとフォルダー：見る、それから変える
  | ListDir Text            -- the names in a folder
  | Glob Text               -- the paths that match "logs/*.log"
  | Exists Text             -- a file or a folder is there
  | IsDir Text              -- a folder is there
  | FileInfo Text           -- the size and the time it was changed
  | MakeDir Text            -- make a folder (and the ones above it)
  | Remove Text             -- remove a file, or an empty folder
  | Rename Text Text        -- move from, to
  | CopyFile Text Text      -- copy from, to
  | AppendFile Text Text    -- add to the end of a file
  -- ANCHOR_END: fileRequests
  -- ANCHOR: processRequest
  -- A command: its name, its arguments, and the text for its input.
  -- No shell: "a b; rm x" is one argument, as it is.
  -- コマンド：名前、引数、標準入力に渡す文字。シェルは通さない：
  -- "a b; rm x" は、そのまま1つの引数
  | RunProcess Text [Text] Text
  -- ANCHOR_END: processRequest
  -- ANCHOR: clockRequests
  -- The clock, the time zone of this machine, and a die.
  -- 時計、このマシンのタイムゾーン、さいころ
  | Now
  | Here
  | RandomInt Integer Integer  -- from, to (both in)
  -- ANCHOR_END: clockRequests
  -- ANCHOR: sleepRequest
  -- Wait for some milliseconds. Uptime: the milliseconds since the
  -- program started (only for --trace; Lam has no name for it).
  -- 何ミリ秒か待つ。Uptime：プログラムが始まってからのミリ秒（--trace の
  -- ためだけ。Lam からは呼べない）
  | Sleep Integer
  | Uptime
  -- ANCHOR_END: sleepRequest
  deriving (Eq, Show)

-- ANCHOR: cell
-- 🔓 Open a database to read and write, or only to read.
-- データベースを、読み書きのために開くか、読むだけに開くか
data OpenMode = ReadWrite | ReadOnly
  deriving (Eq, Show)

-- 🧱 One cell of SQLite: what a ? in the SQL can be. Json has more
--    (arrays, objects), so the builtin turns a Json into a Cell first.
-- SQLite の1マス：SQL の ? に入れられるもの。Json のほうが多い（配列、
-- オブジェクト）ので、組み込みの関数が、先に Json をマスに直す
data Cell
  = CNull
  | CInt Integer
  | CDec Integer Int        -- c / 10^d, like JDec
  | CText Text
  deriving (Eq, Show)
-- ANCHOR_END: cell
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
  -- ANCHOR: noField
  -- r.name, but r has no name (the type checker stops it before).
  -- r.name なのに、r に name がない（型検査器が先に止める）
  | NoField Name Text
  -- ANCHOR_END: noField
  -- ANCHOR: locatedError
  -- The same error, and where it happened (the innermost place).
  -- 同じエラーと、それが起きた場所（いちばん内側の場所）
  | Located Loc EvalError
  -- ANCHOR_END: locatedError
  -- ANCHOR: deadlockError
  -- Every job waits, and none can go on: who waits, and for what.
  -- どの仕事も待っていて、どれも進めない：だれが、何を待っているか
  | Deadlock [(Text, Text)]
  -- ANCHOR_END: deadlockError
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

-- ANCHOR: fieldOf
-- 🔎 The value of one field.
-- フィールド1つの値
field :: Name -> Value -> Either EvalError Value
field x v = case v of
  VRecord m | Just fv <- Map.lookup x m -> Right fv
  _ -> Left (NoField x (renderValue v))
-- ANCHOR_END: fieldOf

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
  -- ANCHOR: matchCon
  -- Json.JInt and JInt are the same constructor: look at the name
  -- without the module (the type checker made sure of the type).
  -- Json.JInt と JInt は同じ構成子。モジュールを取った名前を見る
  -- （型は、型検査器が確かめてある）
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
compareValues a b = case (a, b) of
  (VInt x,    VInt y)    -> Right (compare x y)
  -- Two numbers, one of them a Float (in a list, a tuple, sort).
  -- 片方が Float の2つの数（リストやタプルの中、sort）
  _ | Just x <- floatOf a, Just y <- floatOf b -> Right (compare x y)
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
  -- ANCHOR: compareRecord
  -- Two records of the same type: the fields in the order of names.
  -- 同じ型の2つのレコード：フィールドを名前の順に比べる
  (VRecord xs, VRecord ys) -> compareLists (Map.elems xs) (Map.elems ys)
  -- ANCHOR_END: compareRecord
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
  -- 1.5, 0.30000000000000004, 1e+21, as JavaScript and Elm show them
  -- JavaScript や Elm と同じ形
  VFloat x       -> showFloat x
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
  -- ANCHOR: renderRecord
  -- { age = 3, name = "Lam" }, the same as it is written (by name).
  -- 書くときと同じ形（名前の順）
  VRecord m      -> "{ " <> T.intercalate ", "
                      [ k <> " = " <> renderValue x | (k, x) <- Map.toList m ]
                      <> " }"
  -- ANCHOR_END: renderRecord
  VClosure x _ _ -> "<function of " <> x <> ">"
  VPrim n _ _ _  -> "<builtin " <> n <> ">"
  -- ANCHOR: renderIO
  VIO _          -> "<IO action>"
  -- ANCHOR_END: renderIO
  VTask t        -> "<task #" <> T.pack (show (taskId t)) <> ">"
  VChan c        -> renderChan c
  VPid p         -> renderChan (pidChan p)
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
      VFloat x | x < 0 -> "(" <> renderValue a <> ")"
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
  -- 「フィールドがありません」
  NoField x v        -> v <> " has no field " <> x
  -- 「場所：……」
  Located loc e      -> renderLoc loc <> ": " <> renderError e
  -- ANCHOR: renderDeadlock
  -- 「デッドロック：どの仕事も待っていて、どれも進めません」
  Deadlock waits     -> T.intercalate "\n" $
    "deadlock: every job is waiting, and none can go on"
    : [ "  " <> T.justifyLeft width ' ' who <> "  " <> what
      | (who, what) <- waits ]
    where width = maximum (0 : map (T.length . fst) waits)
  -- ANCHOR_END: renderDeadlock
-- ANCHOR_END: render
-- ANCHOR_END: all
