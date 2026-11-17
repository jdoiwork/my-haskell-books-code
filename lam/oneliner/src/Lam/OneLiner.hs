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
  , OneLiner (..)
  , noOneLiner
  , Command (..)
  , readCommand
  , wrapLine
  , Job (..)
  , prepare
  , runOneLiner
  , showValueOnly
  ) where

import Control.Monad (foldM, forM_, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Eval
import Lam.Guard (Options, readOptions)
import Lam.Io (Handler, Stop (..), ioOf, runAction)
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

-- 📋 Everything the one-liner flags said.
-- ワンライナーのフラグが言ったこと全部
data OneLiner = OneLiner
  { oneProgram :: [Text]      -- each -e, in order
  , oneMode    :: Mode
  , oneSplit   :: Split
  , oneInPlace :: Maybe Text  -- -i: Just "" (no backup) or Just ".bak"
  , oneBegin   :: Maybe Text  -- --begin: the first acc
  , oneEnd     :: Maybe Text  -- --end: what to do with the last acc
  }
  deriving (Eq, Show)

noOneLiner :: OneLiner
noOneLiner = OneLiner [] Once NoSplit Nothing Nothing Nothing

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
      flag@('-' : '-' : _) : rest -> go (guards <> [flag]) one rest
      ('-' : c : cs) : rest | isAlpha c -> short guards one (c : cs) rest
      _ -> finish guards one args

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
showValueOnly :: Monad m => Handler m -> Session -> Expr -> m Ending
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
wrapLine split body = Lam "line" $ case split of
  NoSplit -> body
  _       ->
    Let "fields" (cut split)
      (Let "field" (call "$field" [Var "line", Var "fields"]) body)
  where
    cut s = case s of
      ByRegex p -> call "Regex.split"
                     [App (Var "$regex") (Lit (LString p)), Var "line"]
      _         -> App (Var "words") (Var "line")

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
      wrap e | folding   = Lam "acc" (wrapLine (oneSplit one) e)
             | otherwise = wrapLine (oneSplit one) e
      arity = if folding then 2 else 1
  step <- byType arity wrap body (Var "line")
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
        unless' (t == tString || isVar t) $
          "-e: type error: -p needs a String for each line, but this gives "
            <> render t <> " (for an IO, use -n)"
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
                    (call "$step" [first, Lit (LString "")])
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
doIO :: Monad m => Handler m -> Value -> Run m Value
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
-- 🏃 Run a one-liner. The lines come from the files (each read with
--    readFile, so --allow-read guards them), or from the standard input
--    (getLine, one at a time) when there are no files.
-- ワンライナーを動かす。行は、ファイル（readFile で読むので、
-- --allow-read が見張る）から、ファイルがなければ標準入力（getLine で
-- 1行ずつ）から
runOneLiner
  :: Monad m => Handler m -> Files m -> Session -> OneLiner -> [String]
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
      accN <- case (args, oneInPlace one) of
        ([], _)       -> fromStdin job acc0
        (_, Just ext) -> foldM (editFile job ext) acc0 (map T.pack args)
        (_, Nothing)  -> foldM (readEach job) acc0 (map T.pack args)
      forM_ (jobEnd job) $ \end -> orFail (apply end accN) >>= doIO handle

    -- The standard input, one line at a time, to the end.
    -- 標準入力を、1行ずつ、最後まで
    fromStdin job acc = do
      next <- lift (handle GetLine)
      case next of
        VCon "Just" _ [VString l] -> do
          (acc', out) <- stepLine job acc l
          forM_ out (\o -> lift (handle (PutStr (o <> "\n"))))
          fromStdin job acc'
        _ -> pure acc

    -- Each file, as if its lines came one after another.
    -- ファイルを順に。行が続けて来たのと同じ
    readEach job acc file = do
      text <- readText file
      foldM (\a l -> do
               (a', out) <- stepLine job a l
               forM_ out (\o -> lift (handle (PutStr (o <> "\n"))))
               pure a')
            acc (T.lines text)

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

    -- ANCHOR: editFile
    -- -i: the new lines go back into the file, not to the screen. With
    --     a backup name (-i.bak), the old text is written there first.
    -- -i：新しい行は、画面ではなくファイルに戻す。-i.bak なら、先に
    -- 元の中身をそこへ書く
    editFile job ext acc file = do
      text <- readText file
      (acc', outs) <- foldM (\(a, os) l -> do
                               (a', out) <- stepLine job a l
                               pure (a', os <> maybe [] pure out))
                            (acc, []) (T.lines text)
      when (not (T.null ext)) $ writeText (file <> ext) text
      writeText file (T.unlines outs)
      pure acc'
    -- ANCHOR_END: editFile

    -- ANCHOR: stepLine
    -- One line: -p gives the text to print, -n does an IO, and with
    -- --begin, the step gives the next acc.
    -- 1行：-p は出す文字をくれ、-n は IO をやり、--begin があれば
    -- 次の acc をくれる
    stepLine job acc l = case (jobBegin job, oneMode one) of
      (Just _, _) -> do
        f <- orFail (apply (jobStep job) acc)
        acc' <- orFail (apply f (VString l))
        pure (acc', Nothing)
      (Nothing, Printing) -> do
        out <- orFail (apply (jobStep job) (VString l))
        case out of
          VString o -> pure (acc, Just o)
          _ -> throwE (HaltError "error: -p: a line became something else")
      (Nothing, _) -> do
        io <- orFail (apply (jobStep job) (VString l))
        _ <- doIO handle io
        pure (acc, Nothing)
    -- ANCHOR_END: stepLine
-- ANCHOR_END: runOneLiner

-- ANCHOR: runProgramOnce
-- 1️⃣ lam -e '...' without -n or -p: the text is a whole program, as
--    if it were a file named -e (main, let, import and all). The last
--    expression shows only its value. The words after it are its args.
-- -n も -p もない lam -e：文字列は、-e という名前のファイルと同じく、
-- プログラム全体（main も let も import も）。最後の式は値だけを見せる。
-- 後ろの語は、スクリプトの引数
runProgramOnce
  :: Monad m => Handler m -> Files m -> Session -> OneLiner -> m Ending
runProgramOnce handle files session one =
  runProgramWith handle (showValueOnly handle) files session "-e"
    (T.intercalate "\n" (oneProgram one))
-- ANCHOR_END: runProgramOnce
-- ANCHOR_END: all
