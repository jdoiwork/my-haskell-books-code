{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- ANCHOR: all
-- | Lam to JavaScript: the small tree (Lam.Core) as an ES module that
--   node runs. The runtime (runtime/lam.mjs) goes in front of it.
-- Lam から JavaScript へ：小さな木（Lam.Core）を、node が動かす ES
-- モジュールにする。前にランタイム（runtime/lam.mjs）を入れる。
module Lam.ToJs
  ( toJs
  , jsTarget
  , jsPrims
  , jsNatives
  , runtimeSource
  , programJs
  , exprJs
  , jsName
  ) where

import Control.Monad (forM)
import Control.Monad.State.Strict (State, evalState, get, put)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, isAscii, ord)
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

import Lam.Cli (prepareCli)
import Lam.Core
import Lam.Program (Session (..), checkMain)
import Lam.Syntax

-- ANCHOR: runtime
-- 📜 The runtime, put into lam itself when lam is built (file-embed):
--    the JavaScript a program needs, but not written by the program.
-- ランタイム。lam を作るときに、lam の中に入れる（file-embed）。
-- プログラムが要るが、プログラムが書くのではない JavaScript
runtimeBytes :: ByteString
runtimeBytes = $(embedFile "runtime/lam.mjs")

runtimeSource :: Text
runtimeSource = TE.decodeUtf8 runtimeBytes
-- ANCHOR_END: runtime

-- ANCHOR: target
-- 🎯 What the runtime has. The test "the runtime has every name" reads
--    runtime/lam.mjs, and checks that it has these, and no others.
-- ランタイムが持っているもの。テストが runtime/lam.mjs を読み、これが
-- そろっていて、ほかにないことを確かめる
jsTarget :: Target
jsTarget = Target (Set.fromList jsPrims) (Set.fromList jsNatives)

jsPrims :: [Name]
jsPrims =
  -- Strings, tuples, lists
  -- 文字列・タプル・リスト
  [ "$key.name", "toUpper", "toLower", "chars", "join", "showInt"
  , "fst", "snd", "length", "head", "tail", "lines", "words"
  , "String.isLetter", "String.toInt", "String.toFloat", "String.trim"
  , "String.trimStart", "String.trimEnd", "String.split"
  , "String.startsWith", "String.endsWith", "String.contains"
  , "String.replace", "String.repeat", "String.reverse", "String.take"
  , "String.drop", "String.toUpper", "String.toLower", "String.join"
  , "String.lines", "String.words"
  -- Map and Set
  -- Map と Set
  , "Map.empty", "Map.insert", "Map.lookup", "Map.delete", "Map.member"
  , "Map.size", "Map.toList", "Map.fromList", "Map.keys", "Map.values"
  , "Map.union", "Map.map"
  , "Set.empty", "Set.fromList", "Set.toList", "Set.insert"
  , "Set.delete", "Set.member", "Set.size", "Set.union"
  , "Set.intersection", "Set.difference"
  -- Numbers
  -- 数
  , "toFloat", "round", "floor", "ceiling", "truncate"
  , "Float.roundTiesEven", "Float.roundTiesUp", "negate", "sqrt"
  , "Float.exp", "Float.log", "Float.pow", "Float.pi", "Float.isNaN"
  , "Float.isInfinite", "showFloat", "Float.fixed"
  -- JSON, and showing values
  -- JSON と、値を見せること
  , "Json.parse", "Json.show", "Json.toFloat", "Json.fromFloat"
  , "Json.pretty", "show", "showNumber", "padLeft", "padRight", "center"
  , "groupDigits", "zeroPad"
  -- IO
  , "IO.returnIO", "IO.bindIO", "IO.putStr", "IO.putStrLn", "IO.getLine"
  , "IO.readFile", "IO.writeFile", "IO.finally", "IO.try", "IO.getArgs"
  , "IO.lookupEnv", "IO.getEnvironment", "IO.getContents", "IO.ePutStr"
  , "IO.ePutStrLn", "IO.exit", "error", "todo"
  -- Paths, files, the clock, dice
  -- 道・ファイル・時計・さいころ
  , "Path.join", "Path.basename", "Path.dirname", "Path.extension"
  , "Path.stem", "Path.withExtension"
  , "File.list", "File.exists", "File.isDir", "File.makeDir"
  , "File.remove", "File.rename", "File.copy", "File.append"
  , "Time.now", "Time.toIso", "Time.fromIso", "Random.int"
  -- At once
  -- 同時に
  , "sleep", "IO.all", "IO.allN", "IO.both", "IO.allOk", "IO.race"
  , "spawn", "await"
  , "Debug.log"
  ]

-- 🏎️ The definitions of the standard modules that the runtime writes
--    itself, as loops: the Lam ones call themselves once for each
--    item, and JavaScript has a small stack.
-- 標準のモジュールの定義のうち、ランタイムがループで書くもの。Lam の
-- ものは、要素1つごとに自分を1回呼び、JavaScript のスタックは小さい
jsNatives :: [Name]
jsNatives =
  [ "List.foldr", "List.take", "List.merge", "Prelude.range"
  , "Prelude.replicate", "Prelude.zipWith", "Prelude.takeWhile" ]
-- ANCHOR_END: target

-- ANCHOR: toJs
-- 🚀 The program in a session as one JavaScript file: the runtime,
--    then the program. Left: why it cannot be done (no main, or names
--    the runtime does not have).
-- セッションのプログラムを、1つの JavaScript のファイルに：ランタイム、
-- それからプログラム。Left は、できない理由（main がない、ランタイムに
-- ない名前がある）
toJs :: FilePath -> Session -> Either Text Text
toJs file session
  | not (Map.member "main" (sessionTypes session)) =
      -- 「to-js には main が要ります」
      Left (T.pack file <> ": lam to-js needs a main")
  | otherwise = do
      ready <- prepareCli session
      start <- checkMain ready
      lowered <- either noJs Right (lower jsTarget (sessionCode ready) start)
      Right (T.unlines
        [ "// Made by lam to-js from " <> T.pack file <> ". Run it with node."
        , "// lam to-js が " <> T.pack file <> " から作った。node で動かす。"
        , runtimeSource
        , programJs lowered ])
  where
    -- 「この名前は、まだ JavaScript にありません」
    noJs names = Left (T.pack file <> ": these have no JavaScript yet: "
                       <> T.intercalate ", " (nub (map written names)))
    -- A hidden name, as the program wrote it.
    -- 隠した名前を、プログラムが書いた形で
    written x = case x of
      "$regex"    -> "re\"...\""
      "$group"    -> "re\"...\""
      "$main.cli" -> "main with options (cli)"
      _           -> x
-- ANCHOR_END: toJs

-- ANCHOR: programJs
-- 📃 The program: the builtins it calls and the constructors it makes,
--    then each definition, then main, given to the runtime.
-- プログラム：呼ぶ組み込みと作る構成子、それから定義を1つずつ、最後に
-- main をランタイムに渡す
programJs :: Lowered -> Text
programJs lowered =
  renderStrict (layoutPretty (LayoutOptions (AvailablePerLine 78 1)) doc)
  where
    names = globalNames (loweredGlobals lowered)
    cons  = nub (concatMap (consIn . globalBody) (loweredGlobals lowered)
                 <> consIn (loweredMain lowered))
    doc = vsep
      [ "const $rt = makeRuntime();"
      , "$rt.start(() => {"
      , indent 2 (vsep (concat
          [ [ "const" <+> pretty (primName p) <+> "=" <+> "$rt.prim("
                <> jsString p <> ");" | p <- loweredPrims lowered ]
          , [ constructor c i n | (c, i, n) <- cons ]
          , evalState (forM (loweredGlobals lowered) (global names)) 0
          , [ "return" <+> evalState (expr (scopeOf names) Nothing
                                        (loweredMain lowered)) 0 <> ";" ]
          ]))
      , "});" ]

-- 🧱 A constructor: a function that makes the value, one argument at a
--    time (or the value itself, with no arguments).
-- 構成子：値を作る関数。引数は1つずつ（引数がなければ、値そのもの）
constructor :: Name -> Int -> Int -> Doc ()
constructor c i n =
  "const" <+> pretty (conName c n) <+> "=" <+> arrows
    <> "$rt.con(" <> jsString c <> ", " <> pretty i <> ", ["
    <> hsep (punctuate "," [ pretty a | a <- args ]) <> "]);"
  where
    args = [ "a" <> T.pack (show k) | k <- [1 .. n] ]
    arrows = hsep [ pretty a <+> "=>" | a <- args ]
               <> (if n > 0 then " " else "")

-- 📏 Each definition: const name = its value.
-- 定義1つ：const 名前 = 値
global :: Map (Name, Name) Text -> Global -> State Int (Doc ())
global names g = do
  let self = CGlobal (globalHome g) (globalName g)
  value <- function (scopeOf names) (Just self) (globalBody g)
  pure ("const" <+> pretty (names Map.! (globalHome g, globalName g))
          <+> "=" <+> value <> ";")
-- ANCHOR_END: programJs

-- ANCHOR: names
-- 🏷️ The JavaScript names of the definitions: List.foldl is
--    List$foldl, and Receipt.lam's total is Receipt$total. Every name
--    at the top has a $, so none is a name JavaScript has already (Map,
--    Set, Error), and none is a local of Lam (they have no $ inside).
-- 定義の JavaScript の名前。上の段の名前はどれも $ を持つので、
-- JavaScript がもう持っている名前（Map・Set・Error）とも、Lam のローカル
-- （中に $ がない）とも重ならない
globalNames :: [Global] -> Map (Name, Name) Text
globalNames gs = snd (foldl one (Set.empty, Map.empty) gs)
  where
    one (taken, table) g =
      let base = jsName (homeName (globalHome g)) <> "$"
                   <> jsName (globalName g)
          name = firstFree (base : [ base <> "$" <> T.pack (show k)
                                   | k <- [2 :: Int ..] ])
          firstFree ns = case ns of
            n : rest | n `Set.member` taken -> firstFree rest
                     | otherwise -> n
            [] -> base
      in ( Set.insert name taken
         , Map.insert (globalHome g, globalName g) name table )
    -- examples/to-js/Price.lam is Price.
    homeName h = T.replace ".lam" "" (last (T.splitOn "/" h))

primName :: Name -> Text
primName p = "prim$" <> T.intercalate "$" (map jsName (T.splitOn "." p))

conName :: Name -> Int -> Text
conName c n = "Con$" <> jsName c <> "$" <> T.pack (show n)

-- 🔤 A name JavaScript can read: letters, digits, _ and $ as they are,
--    the others as $ and their number; and a word JavaScript keeps for
--    itself gets a _ at the end (new is new_).
-- JavaScript が読める名前。英数字・_・$ はそのまま、ほかは $ と番号に。
-- JavaScript が自分のために取ってある語には、後ろに _ を付ける
jsName :: Name -> Text
jsName x
  | x `Set.member` reserved = x <> "_"
  | otherwise = T.concatMap one x
  where
    one c
      | isAscii c && (isAlphaNum c || c == '_' || c == '$') = T.singleton c
      | otherwise = "$" <> T.pack (showHex (ord c) "") <> "_"

reserved :: Set Text
reserved = Set.fromList
  [ "await", "break", "case", "catch", "class", "const", "continue"
  , "debugger", "default", "delete", "do", "else", "enum", "export"
  , "extends", "false", "finally", "for", "function", "if", "import"
  , "in", "instanceof", "let", "new", "null", "return", "static"
  , "super", "switch", "this", "throw", "true", "try", "typeof", "var"
  , "void", "while", "with", "yield", "implements", "interface"
  , "package", "private", "protected", "public", "arguments", "eval"
  , "undefined", "NaN", "Infinity" ]
-- ANCHOR_END: names

-- ANCHOR: scope
-- 🗺️ While writing: the JavaScript name of each local, and of each
--    definition. A local that comes again inside (let x ... let x)
--    gets a new name (x$2): JavaScript does not let a block declare a
--    const twice.
-- 書いているあいだ：ローカルごと・定義ごとの JavaScript の名前。内側で
-- もう一度出てくるローカル（let x ... let x）には新しい名前（x$2）。
-- JavaScript は、1つのブロックで同じ const を2度宣言させない
data Scope = Scope
  { scopeLocals  :: Map Name Text
  , scopeGlobals :: Map (Name, Name) Text
  }

scopeOf :: Map (Name, Name) Text -> Scope
scopeOf = Scope Map.empty

-- 🆕 Bind a local: its JavaScript name, and the scope with it.
-- ローカルを1つ束ねる：JavaScript の名前と、それを入れたスコープ
bind :: Scope -> Name -> State Int (Text, Scope)
bind scope x = do
  let base = jsName x
      used = Set.fromList (Map.elems (scopeLocals scope))
  name <- if base `Set.member` used then fresh base else pure base
  pure (name, scope { scopeLocals = Map.insert x name (scopeLocals scope) })

fresh :: Text -> State Int Text
fresh base = do
  k <- get
  put (k + 1)
  pure (base <> "$" <> T.pack (show (k + 1)))
-- ANCHOR_END: scope

-- ANCHOR: loop
-- 🔁 A function that calls itself last (a tail call) becomes a loop:
--    the call puts the new arguments in, and goes around again. The
--    one that calls itself, and the outer names of its arguments.
-- 最後に自分を呼ぶ関数（末尾呼び出し）は、ループにする。呼び出しは新しい
-- 引数を入れて、もう一周する。自分を呼ぶ名前と、引数の外側の名前
data Loop = Loop
  { loopSelf  :: Core
  , loopVars  :: [Text]
  }

-- 🔎 Does the body call self with all n arguments, as the last thing?
-- 本体は、最後のこととして、n 個の引数全部で self を呼ぶか
tailCalls :: Core -> Int -> Core -> Bool
tailCalls self n core = case core of
  CLet x _ b    -> not (hides x) && tailCalls self n b
  CIf _ a b     -> tailCalls self n a || tailCalls self n b
  CCase _ alts  -> or [ tailCalls self n b | (p, b) <- alts
                      , not (any hides (Set.toList (patternVars p))) ]
  COp And _ r   -> tailCalls self n r
  COp Or _ r    -> tailCalls self n r
  _             -> isSelfCall self n core
  where
    hides x = self == CLocal x

isSelfCall :: Core -> Int -> Core -> Bool
isSelfCall self n core = n > 0 && spine core == Just (self, n)
  where
    spine c = case c of
      CApp f _ -> fmap (fmap (+ 1)) (spine f)
      _ | c == self -> Just (c, 0)
        | otherwise -> Nothing

-- 📞 The function and the arguments of a call: f a b is (f, [a, b]).
-- 呼び出しの関数と引数
unApply :: Core -> (Core, [Core])
unApply core = case core of
  CApp f a -> let (g, as) = unApply f in (g, as <> [a])
  _        -> (core, [])
-- ANCHOR_END: loop

-- ANCHOR: function
-- 🧩 A value to name: a function becomes arrows, x => y => ..., and a
--    loop if it calls itself last. self is the name it is called by.
-- 名前を付ける値：関数は矢印 x => y => ... に。最後に自分を呼ぶなら
-- ループに。self は、それを呼ぶ名前
function :: Scope -> Maybe Core -> Core -> State Int (Doc ())
function scope self core = case (self, params core) of
  (Just me, (xs@(_ : _), body)) | tailCalls me (length xs) body -> do
    outer <- mapM (const (fresh "next")) xs
    (inner, scope') <- bindAll scope xs
    stmts <- statements scope' (Just (Loop me outer)) body
    pure (hsep [ pretty o <+> "=>" | o <- outer ] <+> "{"
      <> nest 2 (line <> "for (;;) {"
        <> nest 2 (line <> "const" <+> hsep (punctuate ","
             [ pretty i <+> "=" <+> pretty o | (i, o) <- zip inner outer ])
             <> ";" <> line <> vsep stmts)
        <> line <> "}")
      <> line <> "}")
  _ -> expr scope Nothing core

bindAll :: Scope -> [Name] -> State Int ([Text], Scope)
bindAll scope xs = case xs of
  [] -> pure ([], scope)
  x : rest -> do
    (n, s) <- bind scope x
    (ns, s') <- bindAll s rest
    pure (n : ns, s')
-- ANCHOR_END: function

-- ANCHOR: statements
-- 📜 A tree as statements that end in return: let becomes const, if
--    and case become if, and a tail call of a loop becomes new
--    arguments and continue.
-- 木を、return で終わる文にする。let は const に、if と case は if に。
-- ループの末尾呼び出しは、新しい引数と continue に
statements :: Scope -> Maybe Loop -> Core -> State Int [Doc ()]
statements scope loop core = case core of
  -- A record that needs itself at once (a dictionary whose default
  -- methods get the dictionary): make the object first, then fill it.
  -- すぐに自分が要るレコード（既定のメソッドが辞書を受け取る辞書）：
  -- 先にオブジェクトを作り、それから中身を入れる
  CLet x (CRecord fs) b | usesAtOnce x (CRecord fs) -> do
    (name, scope') <- bind scope x
    fields <- forM fs $ \(n, v) -> do
      d <- expr scope' Nothing v
      pure (pretty (jsName n) <> ":" <+> d)
    rest <- statements scope' loop b
    pure ( ("const" <+> pretty name <+> "= {};")
         : ("Object.assign" <> arguments [pretty name, object fields]
              <> ";")
         : rest )
  CLet x e b -> do
    (name, scope') <- bind scope x
    let loop' = if fmap loopSelf loop == Just (CLocal x) then Nothing
                else loop
    value <- function scope' (Just (CLocal x)) e
    rest <- statements scope' loop' b
    pure (("const" <+> pretty name <+> "=" <+> value <> ";") : rest)
  CIf c a b -> do
    test <- expr scope loop c
    yes <- statements scope loop a
    no <- statements scope loop b
    pure (("if" <+> parens test <+> block yes) : no)
  CCase x alts -> do
    (subject, setUp) <- subjectOf scope x
    branches <- caseBranches scope loop subject alts
    pure (setUp <> branches)
  COp And l r | maybe False (\lp -> loopCalls lp r) loop -> do
    test <- expr scope loop l
    rest <- statements scope loop r
    pure (("if" <+> parens ("!" <> parens test) <+> "return false;") : rest)
  COp Or l r | maybe False (\lp -> loopCalls lp r) loop -> do
    test <- expr scope loop l
    rest <- statements scope loop r
    pure (("if" <+> parens test <+> "return true;") : rest)
  _ | Just lp <- loop
    , isSelfCall (loopSelf lp) (length (loopVars lp)) core -> do
        let (_, args) = unApply core
        values <- mapM (expr scope Nothing) args
        pure [ assign (loopVars lp) values, "continue;" ]
    | otherwise -> do
        value <- expr scope Nothing core
        pure ["return" <+> value <> ";"]
  where
    loopCalls lp = tailCalls (loopSelf lp) (length (loopVars lp))
    assign [v] [x] = pretty v <+> "=" <+> x <> ";"
    assign vs xs = brackets (hsep (punctuate "," (map pretty vs)))
                     <+> "=" <+> brackets (hsep (punctuate "," xs)) <> ";"

-- 🔎 Does the tree use the local x at once (not only inside a function)?
-- 木は、ローカル x を、すぐに使うか（関数の中だけでなく）
usesAtOnce :: Name -> Core -> Bool
usesAtOnce x core = case core of
  CLocal y      -> x == y
  CLam _ _      -> False
  CApp f a      -> usesAtOnce x f || usesAtOnce x a
  CLet y e b    -> x /= y && (usesAtOnce x e || usesAtOnce x b)
  CIf c a b     -> any (usesAtOnce x) [c, a, b]
  COp _ l r     -> usesAtOnce x l || usesAtOnce x r
  CTuple es     -> any (usesAtOnce x) es
  CList es      -> any (usesAtOnce x) es
  CCase e alts  -> usesAtOnce x e || any (usesAtOnce x . snd) alts
  CRecord fs    -> any (usesAtOnce x . snd) fs
  CField e _    -> usesAtOnce x e
  CUpdate e fs  -> usesAtOnce x e || any (usesAtOnce x . snd) fs
  _             -> False

block :: [Doc ()] -> Doc ()
block stmts = "{" <> nest 2 (line <> vsep stmts) <> line <> "}"

-- 🎯 What case looks at: a local as it is, anything else in a const
--    first (so that it is computed once).
-- case が見るもの：ローカルはそのまま、ほかは先に const に入れる
--    （1度だけ計算するため）
subjectOf :: Scope -> Core -> State Int (Doc (), [Doc ()])
subjectOf scope x = case x of
  CLocal _ -> do
    v <- expr scope Nothing x
    pure (v, [])
  _ -> do
    name <- fresh "subject"
    v <- expr scope Nothing x
    pure (pretty name, ["const" <+> pretty name <+> "=" <+> v <> ";"])
-- ANCHOR_END: statements

-- ANCHOR: caseBranches
-- 🌿 The branches of case: if the pattern fits, bind its names and do
--    the branch. A branch that always fits ends the list. If none fits,
--    the runtime says so (as lam does).
-- case の枝：パターンが合えば、名前を束ねて枝をやる。いつも合う枝で、
-- 一覧は終わり。どれも合わなければ、ランタイムがそう言う（lam と同じ）
caseBranches
  :: Scope -> Maybe Loop -> Doc () -> [(Pattern, Core)] -> State Int [Doc ()]
caseBranches scope loop subject alts = case alts of
  [] -> pure ["return $rt.noMatch(" <> subject <> ");"]
  (p, body) : rest -> do
    let (tests, binds) = patternOf subject p
    (names, scope') <- bindAll scope (map fst binds)
    let hidden = any (\n -> fmap loopSelf loop == Just (CLocal n))
                     (map fst binds)
        loop' = if hidden then Nothing else loop
    stmts <- statements scope' loop' body
    let bound = [ "const" <+> pretty n <+> "=" <+> v <> ";"
                | (n, (_, v)) <- zip names binds ]
        inside = bound <> stmts
    case tests of
      [] -> pure inside
      _  -> do
        others <- caseBranches scope loop subject rest
        let test = hsep (punctuate " &&" tests)
        pure (("if" <+> parens test <+> block inside) : others)

-- 🧩 A pattern, on the value at a path: the tests it needs, and the
--    names it binds (with where they are).
-- パターンを、ある道の値に当てる：要るテストと、束ねる名前（と、その場所）
patternOf :: Doc () -> Pattern -> ([Doc ()], [(Name, Doc ())])
patternOf at p = case p of
  PVar x -> ([], [(x, at)])
  PWild  -> ([], [])
  PLit l -> (["$rt.eq(" <> at <> ", " <> literal l <> ")"], [])
  PCon c ps ->
    let (ts, bs) = parts [ (at <> ".args[" <> pretty i <> "]", q)
                         | (i, q) <- zip [0 :: Int ..] ps ]
    in ((at <> ".tag ===" <+> jsString c) : ts, bs)
  PTuple ps -> parts [ (at <> "[" <> pretty i <> "]", q)
                     | (i, q) <- zip [0 :: Int ..] ps ]
  PList ps ->
    let cells = take (length ps) (iterate (<> ".t") at)
        (ts, bs) = parts [ (c <> ".h", q) | (c, q) <- zip cells ps ]
        end = last (at : map (<> ".t") cells)
    in ([ c <+> "!== null" | c <- cells ] <> [end <+> "=== null"] <> ts, bs)
  PCons h t ->
    let (ts, bs) = parts [(at <> ".h", h), (at <> ".t", t)]
    in ((at <+> "!== null") : ts, bs)
  PRecord xs -> ([], [ (x, at <> "." <> pretty (jsName x)) | x <- xs ])
  where
    parts ps = let found = [ patternOf a q | (a, q) <- ps ]
               in (concatMap fst found, concatMap snd found)
-- ANCHOR_END: caseBranches

-- ANCHOR: expr
-- ✏️ A tree as one JavaScript expression. let and case, which need
--    statements, go in a function that is called at once.
-- 木を、JavaScript の式1つに。文が要る let と case は、すぐ呼ぶ関数の中へ
expr :: Scope -> Maybe Loop -> Core -> State Int (Doc ())
expr scope _ core = case core of
  CLit l -> pure (literal l)
  CLocal x ->
    pure (pretty (Map.findWithDefault (jsName x) x (scopeLocals scope)))
  CGlobal h n -> pure (pretty (scopeGlobals scope Map.! (h, n)))
  CPrim p -> pure (pretty (primName p))
  CCon c _ n -> pure (pretty (conName c n))
  CLam _ _ -> do
    let (xs, b) = params core
    (names, scope') <- bindAll scope xs
    body <- arrowBody scope' b
    pure (arrow (hsep (punctuate " =>" (map pretty names))) b body)
  CApp _ _ -> do
    let (f, args) = unApply core
    fun <- expr scope Nothing f
    as <- mapM (expr scope Nothing) args
    pure (callee f fun <> hcat (map argument as))
  CIf c a b -> do
    let part x = wrapArrow x <$> expr scope Nothing x
    c' <- part c
    a' <- part a
    b' <- part b
    pure (group (parens (c' <> nest 2 (line <> "?" <+> a'
                                          <> line <> ":" <+> b'))))
  COp o l r -> do
    l' <- expr scope Nothing l
    r' <- expr scope Nothing r
    pure (operator o l' r')
  CTuple es -> do
    xs <- mapM (expr scope Nothing) es
    pure (brackets (hsep (punctuate "," xs)))
  CList [] -> pure "null"
  CList es -> do
    xs <- mapM (expr scope Nothing) es
    pure ("$rt.list" <> arguments xs)
  CRecord fs -> do
    xs <- fieldsOf fs
    pure (object xs)
  CField x n -> do
    v <- expr scope Nothing x
    pure (callee x v <> "." <> pretty (jsName n))
  CUpdate x fs -> do
    v <- expr scope Nothing x
    xs <- fieldsOf fs
    pure (object (("..." <> v) : xs))
  _ -> do
    -- let or case: a function called at once
    -- let か case：すぐ呼ぶ関数に
    stmts <- statements scope Nothing core
    pure ("(() =>" <+> block stmts <> ")()")
  where
    fieldsOf fs = forM fs $ \(n, x) -> do
      v <- expr scope Nothing x
      pure (pretty (jsName n) <> ":" <+> v)
    -- A call needs ( ) around an arrow: (x => x)(1).
    -- 矢印を呼ぶには ( ) が要る
    callee = wrapArrow

-- 📐 How to break a long line: an argument, or the body of an arrow,
--    goes on a line of its own when the line is too long.
-- 長い行の折り方：長すぎるときは、引数や矢印の本体を、自分の行に置く
argument :: Doc () -> Doc ()
argument a = group (parens (nest 2 (line' <> a) <> line'))

arguments :: [Doc ()] -> Doc ()
arguments xs = group (parens (nest 2 (line' <> vsep (punctuate "," xs))
                              <> line'))

object :: [Doc ()] -> Doc ()
object xs = group (braces (nest 2 (line <> vsep (punctuate "," xs))
                           <> line))

arrow :: Doc () -> Core -> Doc () -> Doc ()
arrow name body d = case body of
  CLet {}  -> name <+> "=>" <+> d
  CCase {} -> name <+> "=>" <+> d
  _        -> group (name <+> "=>" <> nest 2 (line <> d))

-- 🏹 An arrow inside another expression needs ( ) around it.
-- ほかの式の中の矢印には ( ) が要る
wrapArrow :: Core -> Doc () -> Doc ()
wrapArrow c d = case c of
  CLam _ _ -> parens d
  _        -> d

-- 🏹 The body of an arrow: an expression, or a block for let and case.
-- 矢印の本体：式か、let と case ならブロック
arrowBody :: Scope -> Core -> State Int (Doc ())
arrowBody scope body = case body of
  CLet {}  -> block <$> statements scope Nothing body
  CCase {} -> block <$> statements scope Nothing body
  CRecord _ -> parens <$> expr scope Nothing body
  CUpdate _ _ -> parens <$> expr scope Nothing body
  _        -> expr scope Nothing body
-- ANCHOR_END: expr

-- ANCHOR: operator
-- ➕ The operators. && and || are JavaScript's own (they stop early,
--    as Lam's do). The others go to the runtime, which works as lam
--    does: + on two Ints is a BigInt, and on a Float a number; // is
--    rounded down; == compares the insides.
-- 演算子。&& と || は JavaScript のまま（Lam と同じく打ち切る）。ほかは
-- ランタイムへ。lam と同じに動く：Int どうしの + は BigInt、Float なら
-- number。// は下へ丸め、== は中身をくらべる
operator :: Op -> Doc () -> Doc () -> Doc ()
operator o l r = case o of
  And    -> group (parens (l <> nest 2 (line <> "&&" <+> r)))
  Or     -> group (parens (l <> nest 2 (line <> "||" <+> r)))
  -- m ?: d looks at d only when m has no value.
  -- m ?: d は、m に値がないときだけ d を見る
  OrElse -> "$rt.orElse" <> arguments [l, "() =>" <+> r]
  _      -> "$rt." <> pretty (opName o) <> arguments [l, r]
  where
    opName op = case op of
      Add -> "add"; Sub -> "sub"; Mul -> "mul"; Div -> "div"
      IntDiv -> "intDiv"; DivMaybe -> "divMaybe"
      IntDivMaybe -> "intDivMaybe"
      Eq -> "eq"; Ne -> "ne"; Lt -> "lt"; Le -> "le"; Gt -> "gt"; Ge -> "ge"
      Concat -> "append"; Cons -> "cons"
      _ -> "unknown" :: Text

-- 🔢 A literal: an Int is a BigInt (7n), a Float a number, a string
--    as JSON writes it (it is a JavaScript string, too).
-- そのまま書く値：Int は BigInt（7n）、Float は number、文字列は JSON の
-- 書き方で（JavaScript の文字列でもある）
literal :: Literal -> Doc ()
literal l = case l of
  LInt n | n < 0     -> parens (pretty (show n) <> "n")
         | otherwise -> pretty (show n) <> "n"
  LFloat x | x < 0     -> parens (pretty (show x))
           | otherwise -> pretty (show x)
  LBool True  -> "true"
  LBool False -> "false"
  LString s   -> jsString s

jsString :: Text -> Doc ()
jsString s = pretty (TE.decodeUtf8 (BL.toStrict (Aeson.encode s)))
-- ANCHOR_END: operator

-- ANCHOR: consIn
-- 🧱 The constructors a tree makes (as functions, or values).
-- 木が作る構成子（関数か、値として）
consIn :: Core -> [(Name, Int, Int)]
consIn core = case core of
  CCon c i n    -> [(c, i, n)]
  CLam _ b      -> consIn b
  CApp f a      -> consIn f <> consIn a
  CLet _ e b    -> consIn e <> consIn b
  CIf c a b     -> consIn c <> consIn a <> consIn b
  COp _ l r     -> consIn l <> consIn r
  CTuple es     -> concatMap consIn es
  CList es      -> concatMap consIn es
  CCase e alts  -> consIn e <> concatMap (consIn . snd) alts
  CRecord fs    -> concatMap (consIn . snd) fs
  CField e _    -> consIn e
  CUpdate e fs  -> consIn e <> concatMap (consIn . snd) fs
  _             -> []
-- ANCHOR_END: consIn

-- 🧪 One expression with no definitions, for the tests and the book.
-- 定義のない式1つ。テストと本のため
exprJs :: Core -> Text
exprJs core = renderStrict (layoutPretty defaultLayoutOptions
  (evalState (expr (scopeOf Map.empty) Nothing core) 0))
-- ANCHOR_END: all
