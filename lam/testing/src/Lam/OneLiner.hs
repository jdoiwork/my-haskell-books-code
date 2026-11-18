-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | One-liners, as in Perl: lam -e 'program', and -n, -p, -a, -F, -i,
--   --begin, --end. The flags only build a Lam expression around the
--   program; the type checker and the evaluator do the rest.
-- Perl のようなワンライナー：lam -e 'プログラム' と、-n・-p・-a・-F・-i・
-- --begin・--end。フラグは、プログラムのまわりに Lam の式を組み立てる
-- だけ。あとは型検査器と評価器の仕事
module Lam.OneLiner
  ( Mode (..)
  , Split (..)
  , Input (..)
  , OneLiner (..)
  , noOneLiner
  , Command (..)
  , readCommand
  , wrapLine
  , wrapWith
  , recordName
  , selectPath
  , Job (..)
  , prepare
  , runOneLiner
  , showValueOnly
  ) where

import Control.Monad (foldM, forM_, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson.Text (encodeToLazyText)
import Data.Char (isAlpha, isDigit)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Lam.Eval
import Lam.Guard (Options, readOptions)
import Lam.Io (Concurrent, Handler, ioOf, runAction)
import Lam.Json (fromAeson, parseJson, toAeson)
import Lam.Module (Files, runProgramWith)
import Lam.Parser (parseLam)
import Lam.Pcre (compileRegex)
import Lam.Program
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: types
-- 🚩 What to do with the program: run it once (-e), once for each
--    line (-n), or once for each line and print what it gives (-p).
--    -p is stronger than -n, as in Perl.
-- プログラムをどう動かすか：1回（-e）、1行ごと（-n）、1行ごとに動かして
-- 答えを出す（-p）。Perl と同じく、-p は -n より強い
data Mode = Once | EachLine | Printing
  deriving (Eq, Ord, Show)

-- ✂️ How -a cuts a line into columns: at spaces (as awk does), or at a
--    regular expression (-F, or -F: or -F'\t').
-- -a が行を列に切るやり方：空白で（awk と同じ）、または正規表現で（-F）
data Split = NoSplit | ByWords | ByRegex Text
  deriving (Eq, Show)

-- ANCHOR: input
-- 📥 What one piece of the input is: a line of text, or JSON. --json
--    reads the whole input as one JSON, --jsonl one JSON on each line
--    (JSON Lines).
-- 入力の1つ分：テキストの1行か、JSON か。--json は入力全体を1つの
-- JSON として、--jsonl は1行に1つの JSON（JSON Lines）として読む
data Input = TextLines | WholeJson | JsonLines
  deriving (Eq, Show)
-- ANCHOR_END: input

-- 📋 Everything the one-liner flags said.
-- ワンライナーのフラグが言ったこと全部
data OneLiner = OneLiner
  { oneProgram :: [Text]      -- each -e, in order
  , oneMode    :: Mode
  , oneSplit   :: Split
  , oneInPlace :: Maybe Text  -- -i: Just "" (no backup) or Just ".bak"
  , oneBegin   :: Maybe Text  -- --begin: the first acc
  , oneEnd     :: Maybe Text  -- --end: what to do with the last acc
  , oneInput   :: Input       -- --json, --jsonl
  , onePath    :: [Text]      -- --path=orders.items: ["orders", "items"]
  -- --print-script: show the same script, and run nothing
  , oneScript  :: Bool
  }
  deriving (Eq, Show)

noOneLiner :: OneLiner
noOneLiner =
  OneLiner [] Once NoSplit Nothing Nothing Nothing TextLines [] False

-- 🧭 What lam was asked to do.
-- lam が頼まれたこと
data Command
  = Repl
  | RunFile FilePath [String]       -- lam file.lam args...
  | RunOneLiner OneLiner [String]   -- lam -e '...' files or args...
  deriving (Eq, Show)
-- ANCHOR_END: types

-- ANCHOR: readCommand
-- 🚩 Read the flags before the file (or before the input files of a
--    one-liner). Short flags go together, as in Perl:  -ane '...'  is
--    -a -n -e '...'. -e, -F and -i take the rest of their word (-F,
--    -i.bak); -e and -F take the next word when nothing is left.
--    The long flags (--allow-net, --trace, ...) go to readOptions.
-- ファイル（ワンライナーなら入力のファイル）より前のフラグを読む。
-- 短いフラグは、Perl と同じくまとめて書ける。-e・-F・-i は、その語の
-- 残りを取る（-F,  -i.bak）。-e と -F は、残りがなければ次の語を取る。
-- 長いフラグは readOptions に渡す
readCommand :: [String] -> Either String (Options, Command)
readCommand = go [] noOneLiner
  where
    go guards one args = case args of
      "--begin" : x : rest -> go guards one { oneBegin = Just (T.pack x) }
                                rest
      "--end"   : x : rest -> go guards one { oneEnd = Just (T.pack x) } rest
      [flag] | flag `elem` ["--begin", "--end"] ->
        Left (flag <> " needs a Lam expression after it")
      -- ANCHOR: scriptFlag
      "--print-script" : rest -> go guards one { oneScript = True } rest
      -- ANCHOR_END: scriptFlag
      -- ANCHOR: jsonFlags
      -- --json and --jsonl mean -n, as -a does.
      -- -a と同じく、--json と --jsonl は -n も意味する
      "--json"  : rest -> reading WholeJson rest
      "--jsonl" : rest -> reading JsonLines rest
      flag : rest | Just keys <- T.stripPrefix "--path=" (T.pack flag) ->
        if T.null keys || any T.null (T.splitOn "." keys)
          then Left "--path needs keys: --path=orders.items"
          else go guards one { onePath = T.splitOn "." keys } rest
      -- ANCHOR_END: jsonFlags
      flag@('-' : '-' : _) : rest -> go (guards <> [flag]) one rest
      ('-' : c : cs) : rest | isAlpha c -> short guards one (c : cs) rest
      _ -> finish guards one args

      where
        reading input rest
          | oneInput one /= TextLines && oneInput one /= input =
              Left "--json and --jsonl cannot go together"
          | otherwise =
              go guards (atLeast EachLine one) { oneInput = input } rest

    -- One word of short flags, one letter at a time.
    -- 短いフラグの1語を、1文字ずつ
    short guards one letters rest = case letters of
      [] -> go guards one rest
      'n' : more -> short guards (atLeast EachLine one) more rest
      'p' : more -> short guards (atLeast Printing one) more rest
      'a' : more -> short guards (splitting ByWords one) more rest
      'F' : more -> taking more rest $ \pat rest' ->
        go guards (splitting (ByRegex (T.pack pat)) one) rest'
      'e' : more -> taking more rest $ \prog rest' ->
        go guards one { oneProgram = oneProgram one <> [T.pack prog] } rest'
      'i' : more -> go guards one { oneInPlace = Just (T.pack more) } rest
      c : _ -> Left ("unknown flag: -" <> [c])

    -- The rest of the word, or (when it is empty) the next word.
    -- 語の残り。空なら次の語
    taking more rest k = case (more, rest) of
      ("", next : rest') -> k next rest'
      ("", [])           -> Left "-e and -F need something after them"
      _                  -> k more rest

    atLeast m one = one { oneMode = max m (oneMode one) }
    -- -a and -F mean -n, as in Perl (5.20 and later).
    -- Perl（5.20 から）と同じく、-a と -F は -n も意味する
    splitting s one = atLeast EachLine one
      { oneSplit = case (s, oneSplit one) of
          (ByWords, ByRegex p) -> ByRegex p
          _                    -> s }

    finish guards one rest = do
      (opts, _) <- readOptions guards
      command <- if one == noOneLiner
        then Right (case rest of
                      []          -> Repl
                      file : more -> RunFile file more)
        else RunOneLiner <$> checked one rest <*> Right rest
      Right (opts, command)
-- ANCHOR_END: readCommand

-- ANCHOR: checked
-- ✅ Flags that cannot go together are an error before anything runs.
-- 一緒に使えないフラグは、何かを動かす前にエラーにする
checked :: OneLiner -> [String] -> Either String OneLiner
checked one rest
  | null (oneProgram one) =
      Left "-n, -p, -a, -F, -i, --begin and --end need -e 'program'"
  | oneScript one, oneMode one == Once, not folding =
      Left "--print-script goes with -n or -p (with -e alone, the program \
           \is a script already)"
  | Just _ <- oneInPlace one, oneMode one /= Printing =
      Left "-i needs -p (each line becomes what the program gives)"
  | Just _ <- oneInPlace one, null rest =
      Left "-i needs the files to change"
  | oneMode one == Printing, folding =
      Left "--begin and --end go with -n, not with -p"
  | Just _ <- oneEnd one, Nothing <- oneBegin one =
      Left "--end needs --begin (the first acc)"
  | ByRegex p <- oneSplit one, Left msg <- compileRegex p =
      Left ("-F: " <> T.unpack msg)
  -- ANCHOR: jsonChecks
  | oneInput one == TextLines, not (null (onePath one)) =
      Left "--path goes with --json or --jsonl"
  | oneInput one /= TextLines, oneSplit one /= NoSplit =
      Left "-a and -F go with text, not with --json or --jsonl"
  | Just _ <- oneInPlace one
  , oneInput one == WholeJson || not (null (onePath one)) =
      Left "-i goes with text or --jsonl, without --path"
  -- ANCHOR_END: jsonChecks
  | folding   = Right one { oneMode = max EachLine (oneMode one) }
  | otherwise = Right one
  where
    folding = oneBegin one /= Nothing || oneEnd one /= Nothing
-- ANCHOR_END: checked

-- ANCHOR: showValueOnly
-- 👀 lam -e '1 + 2' shows 3: an expression that is not IO is printed
--    with print (display, without the type). An IO is done, and what
--    it gives is printed, too (but not ()):  getArgs  shows ["a", "b"].
-- IO でない式は print で見せる（display で、型なし）。IO ならやって、
-- くれた値も print で見せる（() は見せない）
showValueOnly :: Concurrent m => Handler m -> Session -> Expr -> m Ending
showValueOnly handle session e =
  case inferType (sessionData session) (sessionTypes session) e of
    Right (Forall _ _ (TCon "IO" [r]))
      | r == tUnit || isVar r -> evaluate handle session e
      | otherwise -> evaluate handle session (call "bind" [e, Var "print"])
    _ -> evaluate handle session (App (Var "print") e)
  where
    isVar t = case t of
      TVar _ -> True
      _      -> False
-- ANCHOR_END: showValueOnly

-- ANCHOR: wrapLine
-- 🍬 The names a program gets for each line:
--      line                  \line -> program
--      -a  fields, field     \line -> let fields = words line in
--                                     let field = $field line fields in
--                                     program
--    -F, cuts with Regex.split re"," line instead of words line.
-- 1行ごとに、プログラムがもらう名前。-F, なら words の代わりに
-- Regex.split で切る
wrapLine :: Split -> Expr -> Expr
wrapLine = wrapWith "line"

-- ANCHOR: wrapWith
-- 🍬 The same with another name: json, for --json and --jsonl.
-- 同じことを、別の名前で。--json と --jsonl なら json
wrapWith :: Name -> Split -> Expr -> Expr
wrapWith name split body = Lam name $ case split of
  NoSplit -> body
  _       ->
    Let "fields" (cut split)
      (Let "field" (call "$field" [Var "line", Var "fields"]) body)
  where
    cut s = case s of
      ByRegex p -> call "Regex.split"
                     [App (Var "$regex") (Lit (LString p)), Var "line"]
      _         -> App (Var "words") (Var "line")

-- 🏷️ The name a piece of the input gets.
-- 入力の1つ分がもらう名前
recordName :: Input -> Name
recordName input = case input of
  TextLines -> "line"
  _         -> "json"
-- ANCHOR_END: wrapWith

call :: Name -> [Expr] -> Expr
call f = foldl App (Var f)
-- ANCHOR_END: wrapLine

-- ANCHOR: job
-- 🧳 The Lam values a one-liner runs with, made and checked once.
-- ワンライナーが動かす Lam の値。一度だけ作って、型を確かめる
data Job = Job
  { jobStep  :: Value          -- line -> ...   (or acc -> line -> acc)
  , jobBegin :: Maybe Value    -- the first acc
  , jobEnd   :: Maybe Value    -- acc -> IO a
  }
-- ANCHOR_END: job

-- ANCHOR: prepare
-- ANCHOR: prepareTop
-- 🔧 Read the program, wrap it, let the types decide, and compute.
-- プログラムを読み、包み、型に決めてもらい、計算する
prepare :: Session -> OneLiner -> Either Text Job
prepare session one = do
  body <- parseLam "-e" (T.intercalate "\n" (oneProgram one))
  let folding = oneBegin one /= Nothing
      -- With --begin, the acc comes before the line.
      -- --begin があれば、行の前に acc が来る
      name = recordName (oneInput one)
      wrap e | folding   = Lam "acc" (wrapWith name (oneSplit one) e)
             | otherwise = wrapWith name (oneSplit one) e
      arity = if folding then 2 else 1
  step <- byType arity wrap body (Var name)
  begin <- traverse (parseLam "--begin") (oneBegin one)
  end <- case oneEnd one of
    Nothing -> Right Nothing
    Just text -> do
      e <- parseLam "--end" text
      Just <$> byType 1 (Lam "acc") e (Var "acc")
  checkShape step begin end
  Job <$> value step <*> traverse value begin <*> traverse value end
  where
    typeOf e =
      case inferType (sessionData session) (sessionTypes session) e of
        Left err -> Left (joined (placedType (explain session err)))
        Right (Forall _ _ t) -> Right t
    value e = either (Left . joined . placedEval) Right
                (eval (sessionValues session) e)
    joined (place, msg) = place <> msg
    -- ANCHOR_END: prepareTop

    -- ANCHOR: byType
    -- 🎯 If the program is still a function after its names, give it
    --    the last name, too:  s/cat/dog/g  becomes  s/cat/dog/g line.
    -- 名前をもらっても、まだ関数なら、最後の名前も渡す
    byType arity wrap e lastName = do
      t <- typeOf (wrap e)
      Right $ case after arity t of
        TFun _ _ -> wrap (App e lastName)
        _        -> wrap e
    -- ANCHOR_END: byType

    -- ANCHOR: checkShape
    -- 📐 What each line must give: a String for -p, an IO for -n, and
    --    with --begin, a new acc of the same type as the first.
    -- 1行ごとに返すもの：-p なら String、-n なら IO。--begin があれば、
    -- 初めの acc と同じ型の、新しい acc
    checkShape step begin end = case (begin, oneMode one) of
      (Nothing, Printing) -> do
        t <- after 1 <$> typeOf step
        unless' (t == tString || t == tJson || isVar t) $
          "-e: type error: -p needs a String (or a Json) for each line, \
          \but this gives " <> render t <> " (for an IO, use -n)"
      (Nothing, _) -> do
        t <- after 1 <$> typeOf step
        unless' (isIO t || isVar t) $
          "-e: type error: -n needs an IO for each line, but this gives "
            <> render t <> " (to print each line, use -p)"
      (Just first, _) -> do
        -- if true then first else step first "" : the step must give
        -- back the type of the first acc. Then end must take it.
        -- step が、初めの acc と同じ型を返すことを、if の2つの枝で確かめる
        let acc = If (Lit (LBool True)) first
                    (call "$step" [first, sample])
            whole = Let "$step" step $ case end of
              Just e  -> App e acc
              Nothing -> App (Var "pure") acc
        -- An error with no place of its own: one of the three does not
        -- fit the others.
        -- 場所のないエラー：3つのどれかが、ほかと合わない
        t <- either (Left . placed) Right (typeOf whole)
        unless' (isIO t || isVar t) $
          "--end: type error: --end needs an IO, but this gives " <> render t
    -- ANCHOR_END: checkShape

    -- ANCHOR: shapeHelpers
    -- The type after n arguments:  after 1 (String -> IO ())  is  IO ().
    -- 引数を n 個渡したあとの型
    -- A piece of the input, only to fit the types: "" or JNull.
    -- 型を合わせるためだけの、入力の1つ分
    sample = case oneInput one of
      TextLines -> Lit (LString "")
      _         -> Var "JNull"
    tJson = TCon "Json" []

    after n t = case (n :: Int, t) of
      (0, _)          -> t
      (_, TFun _ res) -> after (n - 1) res
      _               -> t
    isIO t = case t of
      TCon "IO" _ -> True
      _           -> False
    isVar t = case t of
      TVar _ -> True
      _      -> False
    render t = renderScheme (closeOver t)
    unless' ok msg = if ok then Right () else Left msg
    placed msg
      | "type error" `T.isPrefixOf` msg = "--begin, -e, --end: " <> msg
      | otherwise                       = msg
    -- ANCHOR_END: shapeHelpers
-- ANCHOR_END: prepare

-- ANCHOR: halt
-- 🛑 Why a one-liner stopped: an error (exit code 1), or exit n.
-- ワンライナーが止まったわけ：エラー（終了コード 1）か、exit n か
data Halt = HaltError Text | HaltExit Int

type Run m = ExceptT Halt m

-- An error of the evaluator, with its place.
-- 評価器のエラーを、場所つきで
failed :: Monad m => EvalError -> Run m a
failed err =
  let (place, msg) = placedEval err in throwE (HaltError (place <> msg))

orFail :: Monad m => Either EvalError a -> Run m a
orFail = either failed pure

-- ▶️ Do an IO value with the handler.
-- IO の値を、ハンドラーでやる
doIO :: Concurrent m => Handler m -> Value -> Run m Value
doIO handle v = do
  action <- orFail (ioOf v)
  done <- lift (runAction handle action)
  case done of
    Right x          -> pure x
    Left (Failed e)  -> failed e
    Left (Exited n)  -> throwE (HaltExit n)
-- ANCHOR_END: halt

-- ANCHOR: runOneLiner
-- ANCHOR: runOneLinerTop
-- 🏃 Run a one-liner. The input comes from the files (each read with
--    readFile, so --allow-read guards them), or from the standard input
--    when there are no files: getLine, one line at a time, or
--    getContents, all at once, for --json.
-- ワンライナーを動かす。入力は、ファイル（readFile で読むので、
-- --allow-read が見張る）から、ファイルがなければ標準入力から（getLine で
-- 1行ずつ。--json なら getContents でまとめて）
runOneLiner
  :: Concurrent m => Handler m -> Files m -> Session -> OneLiner -> [String]
  -> m Ending
runOneLiner handle files session one args
  | oneMode one == Once =
      runProgramOnce handle files session one
  | otherwise = case prepare session one of
      Left err  -> pure (Errored err)
      Right job -> ending <$> runExceptT (go job)
  where
    go job = do
      let acc0 = maybe (VTuple []) id (jobBegin job)
      accN <- case (args, oneInput one, oneInPlace one) of
        ([], WholeJson, _) -> do
          text <- lift (handle GetContents)
          case text of
            VString t -> feed job acc0 "<stdin>" t
            _         -> pure acc0
        ([], _, _)         -> fromStdin job acc0 1
        (_, _, Just ext)   -> foldM (editFile job ext) acc0 (map T.pack args)
        (_, _, Nothing)    -> foldM (readEach job) acc0 (map T.pack args)
      forM_ (jobEnd job) $ \end -> orFail (apply end accN) >>= doIO handle

    -- The standard input, one line at a time, to the end.
    -- 標準入力を、1行ずつ、最後まで
    fromStdin job acc n = do
      next <- lift (handle GetLine)
      case next of
        VCon "Just" _ [VString l] -> do
          pieces <- piecesOf ("<stdin>:" <> number n) l
          acc' <- foldM (stepAndShow job) acc pieces
          fromStdin job acc' (n + 1 :: Int)
        _ -> pure acc

    -- Each file, as if its lines came one after another.
    -- ファイルを順に。行が続けて来たのと同じ
    readEach job acc file = do
      text <- readText file
      feed job acc file text

    -- All the pieces of a text: its lines, or its JSON.
    -- 文字列の中の、入力の1つ分を全部：行か、JSON か
    feed job acc name text = case oneInput one of
      WholeJson -> piecesOf name text >>= foldM (stepAndShow job) acc
      _ -> foldM (\a (n, l) -> do
                    pieces <- piecesOf (name <> ":" <> number n) l
                    foldM (stepAndShow job) a pieces)
                 acc (zip [1 :: Int ..] (T.lines text))

    stepAndShow job a piece = do
      (a', out) <- stepLine job a piece
      forM_ out (\o -> lift (handle (PutStr (o <> "\n"))))
      pure a'

    number = T.pack . show

    ending r = case r of
      Right ()            -> Finished ""
      Left (HaltError t)  -> Errored t
      Left (HaltExit n)   -> ExitedWith n

    readText file = do
      answer <- lift (handle (ReadFile file))
      case answer of
        VCon "Ok" _ [VString text] -> pure text
        VCon "Err" _ [VString msg] -> throwE (HaltError ("error: " <> msg))
        _ -> throwE (HaltError ("error: cannot read " <> file))
    writeText file text = do
      answer <- lift (handle (WriteFile file text))
      case answer of
        VCon "Err" _ [VString msg] -> throwE (HaltError ("error: " <> msg))
        _                          -> pure ()

    -- ANCHOR_END: runOneLinerTop

    -- ANCHOR: piecesOf
    -- 🧩 The pieces of the input in one text (a line, or all of --json):
    --    the line itself, or its JSON, at --path. When --json (or a
    --    --path) points at an array, each item is one piece. A line of
    --    --jsonl with only spaces is skipped.
    -- 文字列1つ（行か、--json なら全体）の中の、入力の1つ分：行そのもの
    -- か、その JSON の --path の所。--json（か --path）の指す先が配列なら、
    -- 要素の1つずつが1つ分。--jsonl の空白だけの行は飛ばす
    piecesOf place text = case oneInput one of
      TextLines -> pure [VString text]
      JsonLines | T.null (T.strip text) -> pure []
      _ -> case parseJson text of
        Left msg -> throwE (HaltError
          ("error: " <> place <> ": not JSON: " <> T.pack msg))
        Right j -> case selectPath (onePath one) (fromAeson j) of
          Left key -> throwE (HaltError
            ("error: " <> place <> ": --path="
               <> T.intercalate "." (onePath one) <> ": no " <> key))
          Right v
            | spread, VCon "JArr" _ [VList xs] <- v -> pure xs
            | otherwise                              -> pure [v]
      where
        spread = oneInput one == WholeJson || not (null (onePath one))
    -- ANCHOR_END: piecesOf

    -- ANCHOR: editFile
    -- -i: the new lines go back into the file, not to the screen. With
    --     a backup name (-i.bak), the old text is written there first.
    -- -i：新しい行は、画面ではなくファイルに戻す。-i.bak なら、先に
    -- 元の中身をそこへ書く
    editFile job ext acc file = do
      text <- readText file
      (acc', outs) <- foldM (\(a, os) (n, l) -> do
                               pieces <- piecesOf
                                 (file <> ":" <> number n) l
                               foldM (\(b, ps) piece -> do
                                        (b', out) <- stepLine job b piece
                                        pure (b', ps <> maybe [] pure out))
                                     (a, os) pieces)
                            (acc, []) (zip [1 :: Int ..] (T.lines text))
      when (not (T.null ext)) $ writeText (file <> ext) text
      writeText file (T.unlines outs)
      pure acc'
    -- ANCHOR_END: editFile

    -- ANCHOR: stepLine
    -- One piece: -p gives the text to print (a Json is printed as one
    -- line of JSON), -n does an IO, and with --begin, the step gives
    -- the next acc.
    -- 1つ分：-p は出す文字をくれ（Json なら1行の JSON にして出す）、
    -- -n は IO をやり、--begin があれば次の acc をくれる
    stepLine job acc piece = case (jobBegin job, oneMode one) of
      (Just _, _) -> do
        f <- orFail (apply (jobStep job) acc)
        acc' <- orFail (apply f piece)
        pure (acc', Nothing)
      (Nothing, Printing) -> do
        out <- orFail (apply (jobStep job) piece)
        case out of
          VString o -> pure (acc, Just o)
          json -> do
            j <- orFail (toAeson json)
            pure (acc, Just (TL.toStrict (encodeToLazyText j)))
      (Nothing, _) -> do
        io <- orFail (apply (jobStep job) piece)
        _ <- doIO handle io
        pure (acc, Nothing)
    -- ANCHOR_END: stepLine
-- ANCHOR_END: runOneLiner

-- ANCHOR: selectPath
-- 🧭 Walk down a JSON by keys: a key of an object, or the number of
--    an item of an array (from 0). Left is the key that is not there.
-- キーをたどって JSON を下りる：オブジェクトのキーか、配列の番号
-- （0 から）。Left は、なかったキー
selectPath :: [Text] -> Value -> Either Text Value
selectPath keys v = case keys of
  [] -> Right v
  k : rest -> case v of
    VCon "JObj" _ [VMap m] | Just x <- Map.lookup k m -> selectPath rest x
    VCon "JArr" _ [VList xs]
      -- Read as Integer: a long number must not wrap around.
      -- Integer で読む。長い数が、ひと回りしないように
      | not (T.null k), T.all isDigit k
      , i <- read (T.unpack k) :: Integer, i < toInteger (length xs) ->
          selectPath rest (xs !! fromInteger i)
    _ -> Left k
-- ANCHOR_END: selectPath

-- ANCHOR: runProgramOnce
-- 1️⃣ lam -e '...' without -n or -p: the text is a whole program, as
--    if it were a file named -e (main, let, import and all). The last
--    expression shows only its value. The words after it are its args.
-- -n も -p もない lam -e：文字列は、-e という名前のファイルと同じく、
-- プログラム全体（main も let も import も）。最後の式は値だけを見せる。
-- 後ろの語は、スクリプトの引数
runProgramOnce
  :: Concurrent m => Handler m -> Files m -> Session -> OneLiner -> m Ending
runProgramOnce handle files session one =
  runProgramWith handle (showValueOnly handle) files session "-e"
    (T.intercalate "\n" (oneProgram one))
-- ANCHOR_END: runProgramOnce
-- ANCHOR_END: all
