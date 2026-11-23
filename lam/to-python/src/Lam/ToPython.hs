{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- ANCHOR: all
-- | Lam to Python: the small tree (Lam.Core) as one file that python3
--   runs. The runtime (runtime/lam.py) goes in front of it.
-- Lam から Python へ：小さな木（Lam.Core）を、python3 が動かすファイル
-- 1つにする。前にランタイム（runtime/lam.py）を入れる。
module Lam.ToPython
  ( toPython
  , pyBackend
  , pyTarget
  , pyPrims
  , pyNatives
  , pyRuntimeSource
  , programPy
  , exprPy
  , pyName
  ) where

import Control.Monad (forM)
import Control.Monad.State.Strict (State, evalState, get, put)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, isAsciiLower, isDigit, ord, toUpper)
import Data.FileEmbed (embedFile)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Lam.Backend
import Lam.Core
import Lam.Program (Session)
import Lam.Syntax
import Lam.ToJs (jsNatives, jsPrims)

-- ANCHOR: runtime
-- 📜 The runtime, put into lam itself when lam is built (file-embed).
-- ランタイム。lam を作るときに、lam の中に入れる（file-embed）
pyRuntimeBytes :: ByteString
pyRuntimeBytes = $(embedFile "runtime/lam.py")

pyRuntimeSource :: Text
pyRuntimeSource = TE.decodeUtf8 pyRuntimeBytes
-- ANCHOR_END: runtime

-- ANCHOR: target
-- 🎯 What the runtime of Python has: the same builtins as the one of
--    JavaScript, and one more definition written as a loop (groupBy).
--    The test "the runtime has every name" reads runtime/lam.py.
-- Python のランタイムが持っているもの：JavaScript のものと同じ組み込みと、
-- ループで書いた定義をもう1つ（groupBy）。テストが runtime/lam.py を読む
pyTarget :: Target
pyTarget = Target (Set.fromList pyPrims) (Set.fromList pyNatives)

pyPrims :: [Name]
pyPrims = jsPrims

pyNatives :: [Name]
pyNatives = jsNatives <> ["Prelude.groupBy"]
-- ANCHOR_END: target

-- ANCHOR: backend
-- 🎒 Python, as a Backend.
-- Backend としての Python
pyBackend :: Backend
pyBackend = Backend
  { backendName    = "Python"
  , backendCommand = "to-python"
  , backendSuffix  = ".py"
  , backendTarget  = pyTarget
  , backendWrite   = \file lowered -> T.unlines
      [ "# Made by lam to-python from " <> T.pack file
          <> ". Run it with python3."
      , "# lam to-python が " <> T.pack file
          <> " から作った。python3 で動かす。"
      , pyRuntimeSource
      , programPy lowered ]
  }

-- 🚀 The program in a session as one Python file.
-- セッションのプログラムを、1つの Python のファイルに
toPython :: FilePath -> Session -> Either Text Text
toPython = translate pyBackend
-- ANCHOR_END: backend

-- ANCHOR: programPy
-- 📃 The program: one function, program(), that makes the builtins and
--    the constructors it uses, then each definition, and gives main to
--    the runtime. Its names are the locals of program(), so they never
--    meet the names of the runtime.
-- プログラム：関数 program() 1つ。使う組み込みと構成子を作り、定義を
-- 1つずつ作って、main をランタイムに渡す。名前は program() のローカル
-- なので、ランタイムの名前とぶつからない
programPy :: Lowered -> Text
programPy lowered =
  renderStrict (layoutPretty (LayoutOptions (AvailablePerLine 78 1)) doc)
  where
    globals = loweredGlobals lowered
    cons = nub (concatMap (consIn . globalBody) globals
                <> consIn (loweredMain lowered))
    table = topNames globals (loweredPrims lowered) cons
    scope = scopeOf table
    made = concat
      [ [ pretty (tablePrim table Map.! p) <+> "= rt.prim("
            <> pyString p <> ")" | p <- loweredPrims lowered ]
      , [ constructor (tableCon table Map.! (c, n)) c i n
        | (c, i, n) <- cons ] ]
    body = evalState (do
      defs <- forM globals (global scope)
      (pre, m) <- expr scope (loweredMain lowered)
      pure (made : defs <> [pre <> ["return" <+> m]])) 0
    doc = lines'
      [ "rt = make_runtime()", "", ""
      , "def program():" <> nest 4 (hardline <> blankBetween body)
      , "", ""
      , "rt.start(program)" ]

-- 🧱 A constructor: a function that makes the value, one argument at a
--    time (or the value itself, with no arguments).
-- 構成子：値を作る関数。引数は1つずつ（引数がなければ、値そのもの）
constructor :: Text -> Name -> Int -> Int -> Doc ()
constructor name c i n =
  pretty name <+> "=" <+> hsep [ "lambda" <+> pretty a <> ":" | a <- args ]
    <> (if n > 0 then " " else "")
    <> "Con(" <> pyString c <> ", " <> pretty i <> ", " <> tuple <> ")"
  where
    args = [ "a" <> T.pack (show k) | k <- [1 .. n] ]
    tuple = case args of
      [a] -> "(" <> pretty a <> ",)"
      _   -> parens (hsep (punctuate "," (map pretty args)))

-- 📏 Each definition: a def for a function, name = value for the others.
-- 定義1つ：関数なら def、ほかは 名前 = 値
global :: Scope -> Global -> State Int [Doc ()]
global scope g = do
  let name = tableGlobal (scopeTable scope) Map.! (globalHome g, globalName g)
      self = CGlobal (globalHome g) (globalName g)
  case globalBody g of
    CLam _ _ -> (: []) <$> defOf scope name (Just self) [] (globalBody g)
    body -> do
      (pre, v) <- expr scope body
      pure (pre <> [pretty name <+> "=" <+> v])
-- ANCHOR_END: programPy

-- ANCHOR: names
-- 🏷️ The names that the converter makes all begin with a capital
--    letter: List_foldl, Shapes_area, Prim_show, Con_Circle_1, Fn_1.
--    The name of a local of Lam begins with a small letter or _, so
--    none of them is a local. Two names that would be the same get a
--    number (Prim_show and Prim_show_2).
-- 変換器が作る名前は、どれも大文字で始まる。Lam のローカルの名前は、
-- 小文字か _ で始まるので、どれもローカルとぶつからない。同じになって
-- しまう2つの名前には、番号を付ける
data Table = Table
  { tableGlobal :: Map (Name, Name) Text
  , tablePrim   :: Map Name Text
  , tableCon    :: Map (Name, Int) Text
  }

topNames :: [Global] -> [Name] -> [(Name, Int, Int)] -> Table
topNames globals prims cons =
  Table (Map.fromList (zip globalKeys gs))
        (Map.fromList (zip prims ps))
        (Map.fromList (zip conKeys cs))
  where
    globalKeys = [ (globalHome g, globalName g) | g <- globals ]
    conKeys = [ (c, n) | (c, _, n) <- cons ]
    wanted = [ homeName h <> "_" <> pyPart n | (h, n) <- globalKeys ]
          <> [ "Prim_" <> T.intercalate "_" (map pyPart (T.splitOn "." p))
             | p <- prims ]
          <> [ "Con_" <> pyPart c <> "_" <> T.pack (show n)
             | (c, n) <- conKeys ]
    given = numbered wanted
    (gs, rest) = splitAt (length globalKeys) given
    (ps, cs) = splitAt (length prims) rest
    -- examples/shapes.lam is Shapes; a home that does not begin with a
    -- letter gets an M in front.
    -- examples/shapes.lam は Shapes。文字で始まらないものは、前に M
    homeName h =
      let base = pyPart (T.replace ".lam" "" (last (T.splitOn "/" h)))
      in case T.uncons base of
           Just (c, more) | isAsciiLower c -> T.cons (toUpper c) more
                          | isDigit c || c == '_' -> "M" <> base
           _ -> base

-- 🔢 The first name of each, with a number if it is taken.
-- それぞれの名前。もう使われていれば番号を付ける
numbered :: [Text] -> [Text]
numbered = go Set.empty
  where
    go _ [] = []
    go taken (n : ns) =
      let free = firstFree (n : [ n <> "_" <> T.pack (show k)
                                | k <- [2 :: Int ..] ])
          firstFree ms = case ms of
            m : more | m `Set.member` taken -> firstFree more
                     | otherwise -> m
            [] -> n
      in free : go (Set.insert free taken) ns

-- 🔤 A part of a name that Python can read: letters, digits and _ as
--    they are, $ and ' as _, the others as _ and their number.
-- Python が読める名前の部品。文字・数字・_ はそのまま、$ と ' は _ に、
-- ほかは _ と番号に
pyPart :: Name -> Text
pyPart = T.concatMap one
  where
    one c
      | isAlphaNum c || c == '_' = T.singleton c
      | c == '$' || c == '\'' = "_"
      | otherwise = "_" <> T.pack (showHex (ord c) "") <> "_"

-- 🔤 The name of a local: a word Python keeps for itself gets a _ at
--    the end (lambda is lambda_), and so does rt, the runtime's name.
-- ローカルの名前。Python が自分のために取ってある語には、後ろに _ を
-- 付ける。ランタイムの名前 rt も同じ
pyName :: Name -> Text
pyName x
  | base `Set.member` reserved = base <> "_"
  | otherwise = base
  where
    base = pyPart x

reserved :: Set Text
reserved = Set.fromList
  [ "False", "None", "True", "and", "as", "assert", "async", "await"
  , "break", "class", "continue", "def", "del", "elif", "else", "except"
  , "finally", "for", "from", "global", "if", "import", "in", "is"
  , "lambda", "nonlocal", "not", "or", "pass", "raise", "return", "try"
  , "while", "with", "yield", "rt" ]
-- ANCHOR_END: names

-- ANCHOR: scope
-- 🗺️ While writing: the Python name of each local, the names at the
--    top, and the locals that a loop gives new values (loopVars).
-- 書いているあいだ：ローカルごとの Python の名前、上の段の名前、ループが
-- 新しい値を入れるローカル（loopVars）
data Scope = Scope
  { scopeLocals   :: Map Name Text
  , scopeTable    :: Table
  , scopeInLoop   :: Bool
  , scopeLoopVars :: Set Text
  }

scopeOf :: Table -> Scope
scopeOf t = Scope Map.empty t False Set.empty

-- 🆕 Bind a local: its Python name, and the scope with it. A name that
--    is taken already gets a number (x_1), so a local is never given
--    a value twice in one function (but by a loop).
-- ローカルを1つ束ねる。使われている名前には番号を付ける（x_1）。だから、
-- 1つの関数の中で、ローカルに2度値を入れることはない（ループを除く）
bind :: Scope -> Name -> State Int (Text, Scope)
bind scope x = do
  let base = pyName x
      used = Set.fromList (Map.elems (scopeLocals scope))
  name <- if base `Set.member` used then fresh base used else pure base
  pure ( name
       , scope { scopeLocals = Map.insert x name (scopeLocals scope)
               , scopeLoopVars = if scopeInLoop scope
                   then Set.insert name (scopeLoopVars scope)
                   else scopeLoopVars scope } )

bindAll :: Scope -> [Name] -> State Int ([Text], Scope)
bindAll scope xs = case xs of
  [] -> pure ([], scope)
  x : rest -> do
    (n, s) <- bind scope x
    (ns, s') <- bindAll s rest
    pure (n : ns, s')

fresh :: Text -> Set Text -> State Int Text
fresh base used = do
  k <- get
  put (k + 1)
  let name = base <> "_" <> T.pack (show (k + 1))
  if name `Set.member` used then fresh base used else pure name

-- 🆕 A name for a function the converter makes up: Fn_1, Let_2.
-- 変換器が作る関数の名前
helper :: Text -> State Int Text
helper base = do
  k <- get
  put (k + 1)
  pure (base <> "_" <> T.pack (show (k + 1)))
-- ANCHOR_END: scope

-- ANCHOR: capture
-- 🪤 A function made inside a loop holds on to the variables of the
--    loop, not to their values: when the loop goes around, it would see
--    the new ones (the Python FAQ, "Why do lambdas defined in a loop
--    with different values all return the same result?"). So it takes
--    them as default arguments, n=n, which keep the value of now.
-- ループの中で作った関数は、ループの変数の「値」ではなく「変数」を
-- つかまえる。ループが回ると、新しい値が見えてしまう。だから、既定の
-- 引数 n=n で受け取る。既定の引数は、いまの値を持つ
captured :: Scope -> Maybe Name -> Core -> [Text]
captured scope self core
  | not (scopeInLoop scope) = []
  | otherwise =
      [ py | x <- Set.toList (freeLocals core), Just x /= self
           , Just py <- [Map.lookup x (scopeLocals scope)]
           , py `Set.member` scopeLoopVars scope ]

-- 🚪 The scope inside a new function: no loop of its own (yet).
-- 新しい関数の中のスコープ：（まだ）自分のループはない
inside :: Scope -> Scope
inside scope = scope { scopeInLoop = False, scopeLoopVars = Set.empty }
-- ANCHOR_END: capture

-- ANCHOR: loop
-- 🔁 A loop: the function that calls itself last, and the Python names
--    of its parameters.
-- ループ：最後に自分を呼ぶ関数と、その引数の Python の名前
data Loop = Loop
  { loopSelf :: Core
  , loopVars :: [Text]
  }
-- ANCHOR_END: loop

-- ANCHOR: def
-- 🧩 A function as a def. A function of more than one argument is
--    curried by @rt.curry(n), and called f(a)(b), as in Lam. If it calls
--    itself last, its body goes in a while loop.
-- 関数を def に。引数が2つ以上なら @rt.curry(n) でカリー化し、Lam と
-- 同じく f(a)(b) と呼ぶ。最後に自分を呼ぶなら、本体を while のループに
defOf :: Scope -> Text -> Maybe Core -> [Text] -> Core -> State Int (Doc ())
defOf scope name self keep core = do
  let (xs, body) = params core
  (names, s) <- bindAll (inside scope) xs
  let loop = case self of
        Just me | tailCalls me (length xs) body -> Just (Loop me names)
        _ -> Nothing
      s' = case loop of
        Just _  -> s { scopeInLoop = True
                     , scopeLoopVars = Set.fromList names }
        Nothing -> s
  stmts <- statements s' loop body
  let header = "def" <+> pretty name
        <> tupled (map pretty names <> [ pretty k <> "=" <> pretty k
                                       | k <- keep ]) <> ":"
      decorator = [ "@rt.curry(" <> pretty (length xs) <> ")"
                  | length xs > 1 ]
      inner = case loop of
        Just _  -> [block "while True:" stmts]
        Nothing -> stmts
  pure (lines' (decorator <> [block header inner]))
-- ANCHOR_END: def

-- ANCHOR: statements
-- 📜 A tree as statements that end in return: let becomes = (or def),
--    if stays if, case becomes match, and a tail call of a loop becomes
--    new values for the parameters, and continue.
-- 木を、return で終わる文にする。let は = か def に、if は if に、case は
-- match に。ループの末尾呼び出しは、引数に新しい値を入れて continue に
statements :: Scope -> Maybe Loop -> Core -> State Int [Doc ()]
statements scope loop core = case core of
  -- A record that needs itself at once (a dictionary whose default
  -- methods get the dictionary): make the dict first, then fill it.
  -- すぐに自分が要るレコード（既定のメソッドが辞書を受け取る辞書）：
  -- 先に dict を作り、それから中身を入れる
  CLet x (CRecord fs) b | usesAtOnce x (CRecord fs) -> do
    (name, scope') <- bind scope x
    (pre, fields) <- fieldsOf scope' fs
    rest <- statements scope' loop b
    pure ( pre
        <> [ pretty name <+> "= {}"
           , pretty name <> ".update" <> arguments [dict fields] ]
        <> rest )
  CLet x e@(CLam _ _) b -> do
    (name, scope') <- bind scope x
    d <- defOf scope' name (Just (CLocal x)) (captured scope' (Just x) e) e
    rest <- statements scope' (unhide x loop) b
    pure (d : rest)
  CLet x e b -> do
    (name, scope') <- bind scope x
    (pre, v) <- expr scope' e
    rest <- statements scope' (unhide x loop) b
    pure (pre <> [pretty name <+> "=" <+> v] <> rest)
  CIf c a b -> do
    (pre, test) <- expr scope c
    yes <- statements scope loop a
    no <- statements scope loop b
    pure (pre <> [block ("if" <+> test <> ":") yes] <> no)
  CCase x alts -> do
    (pre, subject) <- expr scope x
    branches <- caseBranches scope loop alts
    pure (pre <> [block ("match" <+> subject <> ":") branches])
  COp And l r | maybe False (`loopCalls` r) loop -> do
    (pre, test) <- expr scope l
    rest <- statements scope loop r
    pure (pre <> [block ("if not" <+> test <> ":") ["return False"]] <> rest)
  COp Or l r | maybe False (`loopCalls` r) loop -> do
    (pre, test) <- expr scope l
    rest <- statements scope loop r
    pure (pre <> [block ("if" <+> test <> ":") ["return True"]] <> rest)
  _ | Just lp <- loop
    , isSelfCall (loopSelf lp) (length (loopVars lp)) core -> do
        let (_, args) = unApply core
        found <- forM args (expr scope)
        let changed = [ (v, a) | (v, arg, (_, a)) <- zip3 (loopVars lp)
                                                        args found
                               , not (stays v arg) ]
        pure (concatMap fst found <> assign changed <> ["continue"])
    | otherwise -> do
        (pre, v) <- expr scope core
        pure (pre <> ["return" <+> v])
  where
    loopCalls lp = tailCalls (loopSelf lp) (length (loopVars lp))
    -- A parameter given itself again needs no new value.
    -- 自分をそのまま渡す引数には、新しい値が要らない
    stays v arg = case arg of
      CLocal y -> Map.lookup y (scopeLocals scope) == Just v
      _        -> False
    assign changed = case changed of
      [] -> []
      _  -> [ hsep (punctuate "," (map (pretty . fst) changed)) <+> "="
                <+> hsep (punctuate "," (map snd changed)) ]

-- 🙈 A let that binds the name of the loop hides the loop.
-- ループの名前を束ねる let は、ループを隠す
unhide :: Name -> Maybe Loop -> Maybe Loop
unhide x loop
  | fmap loopSelf loop == Just (CLocal x) = Nothing
  | otherwise = loop
-- ANCHOR_END: statements

-- ANCHOR: caseBranches
-- 🌿 The branches of case, as the cases of match. A branch that always
--    fits (a variable, or _) ends them; else the last case says that
--    nothing fits (as lam does).
-- case の枝を、match の case に。いつも合う枝（変数か _）で終わり。
-- そうでなければ、最後の case が「どれも合わない」と言う（lam と同じ）
caseBranches :: Scope -> Maybe Loop -> [(Pattern, Core)] -> State Int [Doc ()]
caseBranches scope loop alts = case alts of
  [] -> do
    other <- helper "Other"
    pure [block ("case" <+> pretty other <> ":")
                ["return rt.no_match(" <> pretty other <> ")"]]
  (p, body) : rest -> do
    let names = patternNames p
    (_, scope') <- bindAll scope names
    let loop' = if any (\n -> fmap loopSelf loop == Just (CLocal n)) names
                  then Nothing else loop
    stmts <- statements scope' loop' body
    let this = block ("case" <+> patternOf scope' p <> ":") stmts
    case p of
      PVar _ -> pure [this]
      PWild  -> pure [this]
      _      -> (this :) <$> caseBranches scope loop rest

-- 🧩 A pattern of Lam, as a pattern of Python's match:
--      Box w h      Con("Box", [w, h])
--      x : rest     Cons(x, rest)
--      [a, b]       Cons(a, Cons(b, None))
--      (a, _)       (a, _)
--      { name }     {"name": name}
-- Lam のパターンを、Python の match のパターンに
patternOf :: Scope -> Pattern -> Doc ()
patternOf scope p = case p of
  PVar x -> pretty (scopeLocals scope Map.! x)
  PWild -> "_"
  PLit l -> literalPattern l
  PCon c [] -> "Con(" <> pyString c <> ")"
  PCon c ps -> "Con(" <> pyString c <> ", "
                 <> list (map (patternOf scope) ps) <> ")"
  PTuple [] -> "()"
  PTuple [q] -> "(" <> patternOf scope q <> ",)"
  PTuple ps -> tupled (map (patternOf scope) ps)
  PList ps -> foldr (\q t -> "Cons(" <> patternOf scope q <> ", " <> t <> ")")
                "None" ps
  PCons h t ->
    "Cons(" <> patternOf scope h <> ", " <> patternOf scope t <> ")"
  PRecord xs -> braces (hsep (punctuate ","
    [ pyString x <> ":" <+> pretty (scopeLocals scope Map.! x) | x <- xs ]))

-- 📛 The names a pattern binds, from the left.
-- パターンが束ねる名前。左から
patternNames :: Pattern -> [Name]
patternNames p = case p of
  PVar x     -> [x]
  PCon _ ps  -> concatMap patternNames ps
  PTuple ps  -> concatMap patternNames ps
  PList ps   -> concatMap patternNames ps
  PCons h t  -> patternNames h <> patternNames t
  PRecord xs -> xs
  _          -> []

-- A literal in a pattern: -7, not (-7).
-- パターンの中の値：(-7) ではなく -7
literalPattern :: Literal -> Doc ()
literalPattern l = case l of
  LInt n   -> pretty (show n)
  LFloat x -> pretty (show x)
  _        -> literal l
-- ANCHOR_END: caseBranches

-- ANCHOR: expr
-- ✏️ A tree as one Python expression, and the statements that must come
--    before it: a lambda can hold only an expression, so a function
--    whose body is let or case becomes a def first (Fn_1), and a let or
--    case in the middle of an expression a def that is called at once
--    (Let_2()). A def does nothing until it is called, so it can go
--    before the statement.
-- 木を、Python の式1つと、その前に要る文に。lambda は式しか持てないので、
-- 本体が let や case の関数は、先に def にする（Fn_1）。式の途中の let と
-- case は、すぐ呼ぶ def に（Let_2()）。def は呼ぶまで何もしないので、
-- 文の前に置ける
expr :: Scope -> Core -> State Int ([Doc ()], Doc ())
expr scope core = case core of
  CLit l -> pure ([], literal l)
  CLocal x ->
    pure ([], pretty (Map.findWithDefault (pyName x) x (scopeLocals scope)))
  CGlobal h n -> pure ([], pretty (tableGlobal table Map.! (h, n)))
  CPrim p -> pure ([], pretty (tablePrim table Map.! p))
  CCon c _ n -> pure ([], pretty (tableCon table Map.! (c, n)))
  CLam _ _ -> do
    let (xs, b) = params core
        keep = captured scope Nothing core
    (names, scope') <- bindAll (inside scope) xs
    (pre, body) <- expr scope' b
    if needsDef b || not (null pre)
      then do
        name <- helper "Fn"
        d <- defOf scope name Nothing keep core
        pure ([d], pretty name)
      else pure ([], lambda names keep body)
  CApp _ _ -> do
    let (f, args) = unApply core
    (pf, fun) <- expr scope f
    found <- forM args (expr scope)
    pure (pf <> concatMap fst found,
          callee f fun <> hcat (map (argument . snd) found))
  CIf c a b -> do
    (pc, c') <- expr scope c
    (pa, a') <- expr scope a
    (pb, b') <- expr scope b
    pure (pc <> pa <> pb,
          group (parens (a' <> nest 4 (line <> "if" <+> c'
                                       <> line <> "else" <+> b'))))
  COp o l r -> do
    (pl, l') <- expr scope l
    (pr, r') <- expr scope r
    pure (pl <> pr, operator o l' r')
  CTuple [] -> pure ([], "()")
  CTuple [e] -> do
    (pre, x) <- expr scope e
    pure (pre, "(" <> x <> ",)")
  CTuple es -> do
    found <- forM es (expr scope)
    pure (concatMap fst found, tupled (map snd found))
  CList [] -> pure ([], "None")
  CList es -> do
    found <- forM es (expr scope)
    pure (concatMap fst found, "rt.list" <> arguments (map snd found))
  CRecord fs -> do
    (pre, xs) <- fieldsOf scope fs
    pure (pre, dict xs)
  CField x n -> do
    (pre, v) <- expr scope x
    pure (pre, v <> brackets (pyString n))
  CUpdate x fs -> do
    (pre, v) <- expr scope x
    (pf, xs) <- fieldsOf scope fs
    pure (pre <> pf, dict (("**" <> v) : xs))
  _ -> do
    -- let or case: a def that is called at once
    -- let か case：すぐ呼ぶ def に
    name <- helper (case core of CCase {} -> "Case"; _ -> "Let")
    let keep = captured scope Nothing core
    stmts <- statements (inside scope) Nothing core
    let header = "def" <+> pretty name
          <> tupled [ pretty k <> "=" <> pretty k | k <- keep ] <> ":"
    pure ([block header stmts], pretty name <> "()")
  where
    table = scopeTable scope
    callee f d = case f of
      CLam _ _ -> parens d
      _        -> d

-- 🏹 Must this body be statements? let and case must.
-- 本体は文でなければならないか。let と case はそう
needsDef :: Core -> Bool
needsDef b = case b of
  CLet {}  -> True
  CCase {} -> True
  _        -> False

-- 🏹 lambda x: lambda y: body. The variables of a loop it holds on to
--    come after the first parameter: lambda x, n=n: ...
-- 引数のあとに、つかまえるループの変数
lambda :: [Text] -> [Text] -> Doc () -> Doc ()
lambda names keep body = case names of
  [] -> body
  first : rest ->
    "lambda" <+> hsep (punctuate "," (pretty first
                                      : [ pretty k <> "=" <> pretty k
                                        | k <- keep ])) <> ":"
      <+> lambda rest [] body

fieldsOf :: Scope -> [(Name, Core)] -> State Int ([Doc ()], [Doc ()])
fieldsOf scope fs = do
  found <- forM fs $ \(n, x) -> do
    (pre, v) <- expr scope x
    pure (pre, pyString n <> ":" <+> v)
  pure (concatMap fst found, map snd found)
-- ANCHOR_END: expr

-- ANCHOR: operator
-- ➕ The operators. + - * // and and, or are Python's own: Python's int
--    is as long as it must be, and its // rounds down, as Lam's. The
--    others go to the runtime: / (1 / 0 is Infinity in Lam), and the
--    comparisons (they compare the insides, as lam does).
-- 演算子。+ - * // と and・or は Python のまま：Python の int は要るだけ
-- 長く、// は Lam と同じく下へ丸める。ほかはランタイムへ：/（Lam の
-- 1 / 0 は Infinity）と、比べる演算子（lam と同じく中身を比べる）
operator :: Op -> Doc () -> Doc () -> Doc ()
operator o l r = case o of
  Add    -> infixOp "+"
  Sub    -> infixOp "-"
  Mul    -> infixOp "*"
  IntDiv -> infixOp "//"
  And    -> infixOp "and"
  Or     -> infixOp "or"
  Cons   -> "Cons" <> arguments [l, r]
  -- m ?: d looks at d only when m has no value.
  -- m ?: d は、m に値がないときだけ d を見る
  OrElse -> "rt.or_else" <> arguments [l, "lambda:" <+> r]
  _      -> "rt." <> pretty (opName o) <> arguments [l, r]
  where
    infixOp s = group (parens (l <> nest 4 (line <> s <+> r)))
    opName op = case op of
      Div -> "div"; DivMaybe -> "div_maybe"; IntDivMaybe -> "int_div_maybe"
      Eq -> "eq"; Ne -> "ne"; Lt -> "lt"; Le -> "le"; Gt -> "gt"; Ge -> "ge"
      Concat -> "append"
      _ -> "unknown" :: Text

-- 🔢 A literal: an Int is an int, a Float a float, a string as JSON
--    writes it (it is a Python string, too).
-- そのまま書く値：Int は int、Float は float、文字列は JSON の書き方で
-- （Python の文字列でもある）
literal :: Literal -> Doc ()
literal l = case l of
  LInt n | n < 0     -> parens (pretty (show n))
         | otherwise -> pretty (show n)
  LFloat x | x < 0     -> parens (pretty (show x))
           | otherwise -> pretty (show x)
  LBool True  -> "True"
  LBool False -> "False"
  LString s   -> pyString s

pyString :: Text -> Doc ()
pyString s = pretty (TE.decodeUtf8 (BL.toStrict (Aeson.encode s)))
-- ANCHOR_END: operator

-- 📐 How to lay out: statements one a line (hardline, so that a group
--    never puts two on one line), a block indented by 4 under its
--    header, and an argument on a line of its own when the line is too
--    long (Python joins the lines inside ( )).
-- 並べ方：文は1行に1つ（hardline。group が2つを1行にしないように）、
-- ブロックは見出しの下に4つ下げる。長すぎる行では、引数を自分の行に
-- （Python は ( ) の中の行をつなぐ）
lines' :: [Doc ()] -> Doc ()
lines' = concatWith (\a b -> a <> hardline <> b)

blankBetween :: [[Doc ()]] -> Doc ()
blankBetween groups =
  concatWith (\a b -> a <> hardline <> hardline <> b)
    [ lines' g | g <- groups, not (null g) ]

block :: Doc () -> [Doc ()] -> Doc ()
block header stmts = header <> nest 4 (hardline <> lines' stmts)

argument :: Doc () -> Doc ()
argument a = group (parens (nest 4 (line' <> a) <> line'))

arguments :: [Doc ()] -> Doc ()
arguments xs = group (parens (nest 4 (line' <> vsep (punctuate "," xs))
                              <> line'))

dict :: [Doc ()] -> Doc ()
dict xs = group (braces (nest 4 (line' <> vsep (punctuate "," xs)) <> line'))

-- 🧪 One expression with no definitions, for the tests and the book:
--    the statements before it, then the expression.
-- 定義のない式1つ。テストと本のため：前に要る文、それから式
exprPy :: Core -> Text
exprPy core = renderStrict (layoutPretty defaultLayoutOptions
  (lines' (pre <> [e])))
  where
    (pre, e) = evalState (expr (scopeOf (Table Map.empty Map.empty Map.empty))
                               core) 0
-- ANCHOR_END: all
