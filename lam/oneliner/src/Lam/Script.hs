-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | lam --print-script: write a Lam script that does what a one-liner does,
--   to read it, keep it, or grow it. Like Perl's -MO=Deparse, which
--   shows the loop that -n and -p wrap around a program.
-- lam --print-script：ワンライナーと同じことをする Lam のスクリプトを
-- 書き出す。
-- 読むため、とっておくため、育てるため。Perl の -MO=Deparse が、-n・-p
-- がプログラムのまわりに作るループを見せるのと同じ考え方
module Lam.Script
  ( Shape (..)
  , shapeOf
  , scriptOf
  , commandLine
  , runScriptFlag
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Lam.Eval (Request (..))
import Lam.Io (Handler)
import Lam.OneLiner
import Lam.Parser (parseLam)
import Lam.Program
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: shape
-- 📐 What the types decided for a one-liner, as in prepare: does the
--    program still wait for its piece (s/a/b/g), does --end still wait
--    for the acc (print), and does -p give a Json.
-- 型が決めたこと（prepare と同じ）：プログラムがまだ1つ分を待っているか
-- （s/a/b/g）、--end がまだ acc を待っているか（print）、-p が Json を
-- 返すか
data Shape = Shape
  { takesPiece :: Bool
  , takesAcc   :: Bool
  , givesJson  :: Bool
  }
  deriving (Eq, Show)

shapeOf :: Session -> OneLiner -> Either Text Shape
shapeOf session one = do
  -- 「-e だけなら、プログラムがもうスクリプト」
  if oneMode one == Once
    then Left "--print-script goes with -n or -p (with -e alone, the program \
              \is a script already)"
    else Right ()
  _ <- prepare session one     -- the same checks and errors
  body <- parseLam "-e" (T.intercalate "\n" (oneProgram one))
  let folding = oneBegin one /= Nothing
      name = recordName (oneInput one)
      wrap e | folding   = Lam "acc" (wrapWith name (oneSplit one) e)
             | otherwise = wrapWith name (oneSplit one) e
      arity = if folding then 2 else 1
  t <- typeOf (wrap body)
  let piece = isFunction (after arity t)
  final <- if piece then after arity <$> typeOf (wrap (App body (Var name)))
                    else Right (after arity t)
  acc <- case oneEnd one of
    Nothing -> Right False
    Just text -> do
      e <- parseLam "--end" text
      isFunction . after 1 <$> typeOf (Lam "acc" e)
  -- A type variable (-pe 'id') gives back the piece itself: a Json
  -- when the input is JSON.
  -- 型変数（-pe 'id'）は、もらった1つ分をそのまま返す。入力が JSON なら Json
  let json = final == TCon "Json" []
          || (isVar final && oneInput one /= TextLines)
  Right (Shape piece acc json)
  where
    typeOf e =
      case inferType (sessionData session) (sessionTypes session) e of
        Left err -> Left (T.pack (show err))
        Right (Forall _ _ t) -> Right t
    after n t = case (n :: Int, t) of
      (0, _)          -> t
      (_, TFun _ res) -> after (n - 1) res
      _               -> t
    isFunction t = case t of
      TFun _ _ -> True
      _        -> False
    isVar t = case t of
      TVar _ -> True
      _      -> False
-- ANCHOR_END: shape

-- ANCHOR: commandLine
-- 💬 The one-liner again, as one command line (for the comment at the
--    top). Each value is put in '...' for the shell.
-- ワンライナーを、コマンドの1行に戻す（頭のコメントのため）。値は
-- シェルのために '...' で包む
commandLine :: OneLiner -> Text
commandLine one = T.unwords $
  ["lam"]
  <> [ "--json"  | oneInput one == WholeJson ]
  <> [ "--jsonl" | oneInput one == JsonLines ]
  <> [ "--path=" <> T.intercalate "." (onePath one)
     | not (null (onePath one)) ]
  <> [ "-i" <> ext | Just ext <- [oneInPlace one] ]
  <> (case oneSplit one of
        ByRegex p -> ["-F" <> plain p]
        ByWords   -> ["-a"]
        NoSplit   -> [])
  <> [ if oneMode one == Printing then "-p" else "-n" ]
  <> concat [ ["--begin", quote b] | Just b <- [oneBegin one] ]
  <> concat [ ["-e", quote p] | p <- oneProgram one ]
  <> concat [ ["--end", quote e] | Just e <- [oneEnd one] ]
  where
    quote t = "'" <> T.replace "'" "'\\''" t <> "'"
    -- -F, needs no quotes; -F'\t' does.
    -- -F, は '...' が要らない。-F'\t' は要る
    plain t
      | T.all (`elem` (",.:;_/=+-" <> ['a' .. 'z'] <> ['A' .. 'Z']
                       <> ['0' .. '9'])) t = t
      | otherwise = quote t
-- ANCHOR_END: commandLine

-- ANCHOR: scriptOf
-- 📜 The script: the program as a function (step), the pieces of the
--    input (piecesOf), what to do with each piece (visit), the loops
--    that read, and main. Only the parts that this one-liner needs.
-- スクリプト：関数にしたプログラム（step）、入力の1つ分（piecesOf）、
-- 1つ分ごとにすること（visit）、読むループ、main。このワンライナーに
-- 要る部分だけ
scriptOf :: Session -> OneLiner -> Either Text Text
scriptOf session one = do
  shape <- shapeOf session one
  Right $ T.unlines $ concat
    [ header
    , stepPart shape
    , piecesPart
    , visitPart shape
    , if inPlace then editPart shape else readPart
    , mainPart shape ]
  where
    folding = oneBegin one /= Nothing
    inPlace = oneInPlace one /= Nothing
    name    = recordName (oneInput one)
    program = T.intercalate "\n" (oneProgram one)
    indent n t = T.intercalate "\n"
      [ if T.null l then l else T.replicate n " " <> l | l <- T.lines t ]
    -- In f"...": { and } are written twice.
    -- f"..." の中では、{ と } を2つ重ねる
    braces = T.replace "{" "{{" . T.replace "}" "}}"
    pathText = T.intercalate "." (onePath one)

    header =
      [ "#!/usr/bin/env lam"
      , "-- 📜 Made with lam --print-script: the same as this one-liner:"
      , "-- --print-script で作った、このワンライナーと同じ動きのスクリプト："
      , "--   " <> commandLine one
      , "-- Run it:  lam this.lam [files...]   (no files: the standard input)"
      , "-- 動かし方：lam このファイル.lam [ファイル…]（なければ標準入力）"
      , "" ]

    -- ANCHOR: stepPart
    stepPart shape =
      fieldPart
      <> [ "-- 🔁 The program of -e, for one " <> name <> "."
         , "-- -e のプログラム。" <> name <> " を1つ受け取る"
         , "let step " <> (if folding then "acc " else "") <> name <> " ="
         ]
      <> [ splitLines | not (T.null splitLines) ]
      <> [ if takesPiece shape
             then "  (\n" <> indent 4 program <> "\n  ) " <> name
             else indent 2 program
         , "" ]
      <> beginEnd shape
    -- ANCHOR_END: stepPart

    fieldPart = case oneSplit one of
      NoSplit -> []
      _ ->
        [ "-- 🔢 field n is the column n: 1 is the first, 0 the whole line,"
        , "--    and a column that is not there is \"\"."
        , "-- field n は n 列目。1 が1列目、0 は行全体、ない列は \"\""
        , "let fieldOf line fields n ="
        , "  if n == 0 then line"
        , "  else if n < 0 then \"\""
        , "  else case drop (n - 1) fields of"
        , "    | x : _ -> x"
        , "    | [] -> \"\""
        , "" ]
    splitLines = case oneSplit one of
      NoSplit   -> ""
      ByWords   -> "  let fields = words line in\n\
                   \  let field = fieldOf line fields in"
      ByRegex p ->
        "  let fields = Regex.split re\"" <> T.replace "\"" "\\\"" p
        <> "\" line in\n  let field = fieldOf line fields in"

    beginEnd shape = case oneBegin one of
      Nothing -> []
      Just b ->
        [ "-- 🌱 --begin: the first acc. / 初めの acc"
        , "let start =", indent 2 b, ""
        , "-- 🏁 --end: what to do with the last acc. / 最後の acc ですること"
        , "let finish acc ="
        , case oneEnd one of
            Nothing -> "  pure ()"
            Just e | takesAcc shape -> "  (\n" <> indent 4 e <> "\n  ) acc"
                   | otherwise      -> indent 2 e
        , "" ]

    -- ANCHOR: piecesPart
    piecesPart = case oneInput one of
      TextLines ->
        [ "-- 🧩 A line is one piece. / 1行が1つ分"
        , "let piecesOf place text = Ok [text]", "" ]
      _ ->
        (if null (onePath one) then [] else pickPart) <>
        [ "-- 🧩 The JSON in a text" <> (if null (onePath one) then "" else
            ", at --path=" <> pathText) <> "."
            <> (if spread then " An array gives its items." else "")
        , "-- 文字列の中の JSON"
            <> (if spread then "。配列なら、要素の1つずつ" else "")
        , "let piecesOf place text ="
        , if oneInput one == JsonLines
            then "  if null (words text) then Ok []\n\
                 \  else case Json.parse text of"
            else "  case Json.parse text of"
        , "    | Err e -> Err f\"{place}: not JSON: {e}\""
        , if null (onePath one)
            then "    | Ok j -> Ok " <> spreadOf "j"
            else "    | Ok j ->\n\
                 \        pick [" <> T.intercalate ", "
                   [ "\"" <> k <> "\"" | k <- onePath one ] <> "] j\n\
                 \          |> Result.mapError (\\k -> f\"{place}: --path="
                 <> braces pathText <> ": no {k}\")\n\
                 \          |> Result.map (\\v -> " <> spreadOf "v" <> ")"
        , "" ]
    spread = oneInput one == WholeJson || not (null (onePath one))
    spreadOf v
      | spread    =
          "(case " <> v <> " of | JArr xs -> xs | _ -> [" <> v <> "])"
      | otherwise = "[" <> v <> "]"
    pickPart =
      [ "-- 🧭 Walk down by keys (a number is the place in an array)."
      , "-- キーをたどって下りる（数なら配列の何番目か）"
      , "let index k xs ="
      , "  case toInt k of"
      , "    | Just i ->"
      , "        if i < 0 || showInt i /= k then Nothing"
      , "        else (case drop i xs of"
      , "                | x : _ -> Just x"
      , "                | [] -> Nothing)"
      , "    | Nothing -> Nothing"
      , ""
      , "let pick keys j ="
      , "  case keys of"
      , "    | [] -> Ok j"
      , "    | k : rest ->"
      , "        let found ="
      , "          case j of"
      , "            | JObj m -> Map.lookup k m"
      , "            | JArr xs -> index k xs"
      , "            | _ -> Nothing"
      , "        in"
      , "        case found of"
      , "          | Just x -> pick rest x"
      , "          | Nothing -> Err k"
      , "" ]
    -- ANCHOR_END: piecesPart

    -- ANCHOR: visitPart
    shown shape e = if givesJson shape then "Json.show (" <> e <> ")" else e
    visitPart shape =
      [ "-- 👣 One piece: give the acc for the next one."
      , "-- 1つ分をやって、次の1つ分のための acc を返す"
      , case (folding, oneMode one) of
          (True, _) -> "let visit acc piece = pure (step acc piece)"
          (False, Printing) ->
            "let visit acc piece = do\n  putStrLn "
            <> paren (shown shape "step piece") <> "\n  pure acc"
          (False, _) -> "let visit acc piece = do\n  step piece\n  pure acc"
      , ""
      , "let visitAll acc pieces ="
      , "  case pieces of"
      , "    | [] -> pure acc"
      , "    | piece : rest -> do"
      , "        next <- visit acc piece"
      , "        visitAll next rest"
      , ""
      , "-- 🛑 Stop with a message, and the exit code 1."
      , "-- わけを言って、終了コード 1 で止まる"
      , "let stop message = do"
      , "  ePutStrLn f\"error: {message}\""
      , "  exit 1"
      , ""
      , "let orStop result ="
      , "  case result of"
      , "    | Ok x -> pure x"
      , "    | Err message -> stop message"
      , "" ]
    paren t = "(" <> t <> ")"
    -- ANCHOR_END: visitPart

    -- ANCHOR: readPart
    readPart =
      [ "-- 📥 The standard input" <> (if oneInput one == WholeJson
           then ", all at once." else ", one line at a time.")
      , "-- 標準入力を" <> (if oneInput one == WholeJson
           then "、まとめて" else "、1行ずつ")
      ]
      <> (if oneInput one == WholeJson
            then [ "let fromStdin acc = do"
                 , "  text <- getContents"
                 , "  pieces <- orStop (piecesOf \"<stdin>\" text)"
                 , "  visitAll acc pieces" ]
            else [ "let fromStdin acc n = do"
                 , "  next <- getLine"
                 , "  case next of"
                 , "    | Nothing -> pure acc"
                 , "    | Just line -> do"
                 , "        pieces <- orStop (piecesOf f\"<stdin>:{n}\" line)"
                 , "        acc2 <- visitAll acc pieces"
                 , "        fromStdin acc2 (n + 1)" ])
      <> [ "" ]
      <> (if oneInput one == WholeJson then [] else
            [ "let fromLines acc file n ls ="
            , "  case ls of"
            , "    | [] -> pure acc"
            , "    | line : rest -> do"
            , "        pieces <- orStop (piecesOf f\"{file}:{n}\" line)"
            , "        acc2 <- visitAll acc pieces"
            , "        fromLines acc2 file (n + 1) rest"
            , "" ])
      <> [ "-- 📂 The files, one after another."
         , "-- ファイルを、順に"
         , "let fromFiles acc files ="
         , "  case files of"
         , "    | [] -> pure acc"
         , "    | file : rest -> do"
         , "        read <- readFile file"
         , "        text <- orStop read"
         ]
      <> (if oneInput one == WholeJson
            then [ "        pieces <- orStop (piecesOf file text)"
                 , "        acc2 <- visitAll acc pieces" ]
            else [ "        acc2 <- fromLines acc file 1 (lines text)" ])
      <> [ "        fromFiles acc2 rest", "" ]
    -- ANCHOR_END: readPart

    -- ANCHOR: editPart
    editPart shape =
      [ "-- ✏️ -i: the new lines go back into the file"
        <> (if ext == "" then "." else ", and the old text into "
              <> "the file name with " <> ext <> ".")
      , "-- -i：新しい行をファイルに書き戻す"
        <> (if ext == "" then ""
              else "。元の中身は " <> ext <> " を付けた名前に")
      , "let outsOf file n ls ="
      , "  case ls of"
      , "    | [] -> Ok []"
      , "    | line : rest ->"
      , "        Result.bind (piecesOf f\"{file}:{n}\" line) (\\pieces ->"
      , "          Result.map (\\more -> map (\\piece -> "
          <> shown shape "step piece" <> ") pieces ++ more)"
      , "            (outsOf file (n + 1) rest))"
      , ""
      , "let editFile file = do"
      , "  read <- readFile file"
      , "  text <- orStop read"
      , "  outs <- orStop (outsOf file 1 (lines text))"
      ]
      <> (if ext == "" then [] else
            [ "  saved <- writeFile f\"{file}" <> braces ext <> "\" text"
            , "  orStop saved" ])
      <> [ "  written <-"
         , "    writeFile file (join \"\" (map (\\o -> o ++ \"\\n\") outs))"
         , "  orStop written"
         , "" ]
    ext = maybe "" id (oneInPlace one)
    -- ANCHOR_END: editPart

    -- ANCHOR: mainPart
    mainPart _
      | inPlace =
          [ "-- 🚪 Change each file. / ファイルを1つずつ書き換える"
          , "let main files = forEach files editFile" ]
      | otherwise =
          [ "-- 🚪 The files, or the standard input when there are none."
          , "-- ファイル、なければ標準入力"
          ]
          <> (if folding
                then [ "let main files = do"
                     , "  last <- if null files then " <> stdinCall
                       <> " else fromFiles start files"
                     , "  finish last" ]
                else [ "let main files ="
                     , "  if null files then " <> stdinCall
                       <> " else fromFiles () files" ])
      where
        first = if folding then "start" else "()"
        stdinCall
          | oneInput one == WholeJson = "fromStdin " <> first
          | otherwise                 = "fromStdin " <> first <> " 1"
    -- ANCHOR_END: mainPart
-- ANCHOR_END: scriptOf

-- ANCHOR: runScriptFlag
-- 🖨️ lam --print-script: show the script, and run nothing.
-- スクリプトを見せるだけで、何も動かさない
runScriptFlag :: Monad m => Handler m -> Session -> OneLiner -> m Ending
runScriptFlag handle session one = case scriptOf session one of
  Left err     -> pure (Errored err)
  Right script -> do
    _ <- handle (PutStr script)
    pure (Finished "")
-- ANCHOR_END: runScriptFlag
-- ANCHOR_END: all
