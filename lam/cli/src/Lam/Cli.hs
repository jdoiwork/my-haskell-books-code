-- ANCHOR: all
-- ANCHOR: cliHead
{-# LANGUAGE OverloadedStrings #-}
-- | The command line, made from the type of main (in the style of
--   Typer): which options and arguments there are, how to read them,
--   and the text of --help. It is made once, before main starts.
-- main の型から作るコマンドライン（Typer 風）：どんなオプションと引数が
-- あるか、その読み方、--help の文。main を始める前に、1度だけ作る。
module Lam.Cli
  ( Cli (..)
  , Runs (..)
  , Command (..)
  , Opt (..)
  , Kind (..)
  , Reading (..)
  , Outcome (..)
  , commandLine
  , decide
  , renderHelp
  , withCli
  , kebab
  , optionNames
  , aboutAbove
  ) where

import Control.Monad (foldM, unless, when)
import Data.Char (isUpper, toLower)
import Data.List (partition, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import Lam.Builtins (maybeValue)
import Lam.Eval
import Lam.Float (readFloat)
import Lam.Pretty (showFloat)
import Lam.Program (Session (..))
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: cliHead

-- ANCHOR: cliTypes
-- 🧭 The whole command line: the name of the program, the comments
--    above main, and what it runs.
-- コマンドラインの全体：プログラムの名前、main の上のコメント、動かすもの
data Cli = Cli
  { cliProgram :: Text          -- greet.lam
  , cliAbout   :: [Text]        -- the comments above main
  , cliRuns    :: Runs
  }
  deriving (Eq, Show)

-- 🧭 One command (main : { … } -> IO ()), or subcommands, each with
--    its name and its comments (main : { add : …, list : … }).
-- コマンド1つか、サブコマンド（それぞれ名前とコメントを持つ）
data Runs
  = One Command
  | Many [(Name, [Text], Command)]
  deriving (Eq, Show)

-- 🧭 One command: its options and arguments, in the written order.
--    takesRecord is False for a subcommand that is an IO () alone.
-- コマンド1つ：オプションと引数を、書いた順に。takesRecord は、
-- IO () だけのサブコマンドなら False
data Command = Command
  { commandOpts :: [Opt]
  , takesRecord :: Bool
  }
  deriving (Eq, Show)

-- 🏷️ One field of the record: an option or an argument.
-- レコードのフィールド1つ：オプションか引数
data Opt = Opt
  { optField :: Name            -- dryRun
  , optDoc   :: [Text]          -- the comments above the field
  , optShort :: [Name]          -- ["d"], from  alias d = dryRun
  , optKind  :: Kind
  }
  deriving (Eq, Show)

-- 🏷️ What the type (and the default) of a field make it.
-- フィールドの型（と既定値）が、それを何にするか
data Kind
  = Argument Reading            -- name : String            NAME
  | Rest Reading                -- files : [String]         FILES...
  | Valued Reading Literal      -- count : Int = 1          --count INT
  | Optional Reading            -- title : Maybe String     --title STRING
  | Repeated Reading [Literal]  -- tag : [String] = []      --tag STRING
  | Flag Bool                   -- loud : Bool (= true)     --loud, --no-loud
  deriving (Eq, Show)

-- 🔤 How to read one word: String, Int or Float.
-- 語1つの読み方
data Reading = AString | AnInt | AFloat
  deriving (Eq, Show)
-- ANCHOR_END: cliTypes

-- ANCHOR: commandLine
-- 🧭 Make the command line of a program, from the type of main and
--    its annotation (if there is one). Without an annotation there are
--    no comments or defaults, and the fields come in the order of
--    their names.
-- プログラムのコマンドラインを、main の型と注釈（あれば）から作る。
-- 注釈がなければ、コメントも既定値もなく、フィールドは名前の順
commandLine
  :: Session -> MainShape -> FilePath -> Text -> Either Text Cli
commandLine s shape file source = do
  Forall _ _ t <- maybe (Left "there is no main") (Right . defaultNumbers)
                    (Map.lookup "main" (sessionTypes s))
  let sig    = Map.lookup "main" (sessionSigs s)
      known  = sessionData s
      about  = maybe [] (\(Loc _ line _, _) -> aboutAbove source line) sig
      shown  = fmap (\(_, Signature _ te) -> te) sig
      record = recordSyntax known
  runs <- case (shape, t) of
    (MainOptions, TFun (TRecord fs _) _) ->
      One <$> command fs (record =<< argumentOf =<< shown)
    (MainCommands, TRecord cs _) ->
      fmap Many $ traverse (subcommand record) $
        inOrder cs (record =<< shown)
    _ -> Left "main does not read the command line"
  pure (Cli (T.pack (takeName file)) about runs)
  where
    argumentOf te = case te of
      TyFun a _ -> Just a
      _         -> Nothing
    subcommand record (f, ct) = case ct of
      TFun (TRecord fs _) _ -> do
        c <- command fs (record =<< argumentOf (fieldType f))
        pure (fieldName f, fieldDoc f, c)
      _ -> pure (fieldName f, fieldDoc f, Command [] False)
    takeName = reverse . takeWhile (/= '/') . reverse
-- ANCHOR_END: commandLine

-- ANCHOR: recordSyntax
-- 📇 The fields of a record type as written: { … } itself, or the
--    alias that stands for it (type alias Options = { … }).
-- 書いたとおりのレコードの型のフィールド：{ … } そのものか、それを表す別名
recordSyntax :: DataTable -> TypeExpr -> Maybe [FieldType]
recordSyntax known te = case te of
  TyRecord fs Nothing -> Just fs
  TyCon c [] | Just (Alias 0 _ body) <- Map.lookup c (aliases known) ->
    recordSyntax known body
  _ -> Nothing

-- 📇 The fields with their types: in the written order (with the
--    comments and defaults) when the syntax is there, otherwise in the
--    order of the names.
-- フィールドと型：書いたとおりのものがあれば、その順に（コメントと
-- 既定値も）。なければ、名前の順に
inOrder :: Map Name Type -> Maybe [FieldType] -> [(FieldType, Type)]
inOrder types written = case written of
  Just fs | map fieldName fs `sameAs` Map.keys types ->
    [ (f, types Map.! fieldName f) | f <- fs ]
  _ -> [ (plainField x (TyCon "?" []), t) | (x, t) <- Map.toList types ]
  where
    sameAs xs ys = sortOn id xs == ys
-- ANCHOR_END: recordSyntax

-- ANCHOR: command
-- 🧭 One command, from the fields of its record. Checked here, once:
--    each field's type can be read, at most one takes the rest, and
--    no two options have the same name.
-- レコードのフィールドから、コマンド1つを作る。ここで1度だけ確かめる：
-- どのフィールドの型も読める、残り全部を取るのは1つまで、
-- 同じ名前のオプションが2つない
command :: Map Name Type -> Maybe [FieldType] -> Either Text Command
command types written = do
  opts <- traverse option (inOrder types written)
  let positional = [ o | o <- opts, isPositional (optKind o) ]
  case dropWhile (not . isRest . optKind) positional of
    _ : _ : _ -> Left ("the argument " <> metavar (optField (head'
                   (filter (isRest . optKind) positional)))
                   <> " takes all the rest, so it must be the last")
    _ -> Right ()
  case [ o | o <- positional, not (null (optShort o)) ] of
    -- 「NAME は引数なので、短い名前は付けられません」
    o : _ -> Left (metavar (optField o)
                   <> " is an argument, so it cannot have a short name")
    [] -> Right ()
  let names = concatMap optionNames opts
  case [ n | (i, n) <- zip [0 :: Int ..] names
             , n `elem` drop (i + 1) names ] of
    -- 「--help は、2回使われています」
    n : _ -> Left ("the option " <> n <> " is used twice")
    []    -> Right ()
  when ("--help" `elem` names) $
    Left "the option --help is made by Lam (a field cannot be called help)"
  pure (Command opts True)
  where
    head' xs = case xs of
      x : _ -> x
      []    -> Opt "" [] [] (Flag False)

-- 🏷️ One field: what its type and default make it.
-- フィールド1つ：型と既定値が、それを何にするか
option :: (FieldType, Type) -> Either Text Opt
option (f, t) = Opt x (fieldDoc f) (fieldShort f) <$> kind
  where
    x = fieldName f
    kind = case (t, fieldDefault f) of
      (TBool, Nothing)                      -> Right (Flag False)
      (TBool, Just (DefaultOne (LBool b)))  -> Right (Flag b)
      (TCon "Maybe" [a], Nothing)           -> Optional <$> reading a
      (TCon "List" [a], Nothing)            -> Rest <$> reading a
      (TCon "List" [a], Just (DefaultList ls)) ->
        (`Repeated` ls) <$> reading a
      (_, Nothing)                          -> Argument <$> reading t
      (_, Just (DefaultOne l))              -> (`Valued` l) <$> reading t
      _ -> Left ("the default of " <> x <> " does not fit its type")
    -- 「--when の型 Time は、コマンドラインから読めません」
    reading a = case a of
      TInt                -> Right AnInt
      TCon "Float" []     -> Right AFloat
      TCon "String" []    -> Right AString
      _ -> Left ("the field " <> x <> " has the type " <> renderType t
                 <> ", which cannot be read from the command line")
-- ANCHOR_END: command

isPositional, isRest :: Kind -> Bool
isPositional k = case k of
  Argument _ -> True
  Rest _     -> True
  _          -> False
isRest k = case k of
  Rest _ -> True
  _      -> False

-- ANCHOR: names
-- 🔠 dryRun is --dry-run on the command line (and max_count is
--    --max-count).
-- dryRun は、コマンドラインでは --dry-run
kebab :: Name -> Text
kebab = T.pack . go . T.unpack
  where
    go cs = case cs of
      []                  -> []
      c : rest
        | isUpper c       -> '-' : toLower c : go rest
        | c == '_'        -> '-' : go rest
        | otherwise       -> c : go rest

-- 🔠 The names of an option: --count and -n, or --no-loud for a flag
--    that is on at first. One letter is -n; a longer alias is --nm.
-- オプションの名前：--count と -n。はじめから true のフラグは --no-loud。
-- 1文字なら -n、長い別名なら --nm
optionNames :: Opt -> [Text]
optionNames o
  | isPositional (optKind o) = []
  | otherwise = long : map short (optShort o)
  where
    long = case optKind o of
      Flag True -> "--no-" <> kebab (optField o)
      _         -> "--" <> kebab (optField o)
    short s
      | T.length s == 1 = "-" <> s
      | otherwise       = "--" <> kebab s

-- 🔠 The name of an argument in the help: fileName is FILE_NAME.
-- ヘルプでの引数の名前
metavar :: Name -> Text
metavar = T.toUpper . T.replace "-" "_" . kebab
-- ANCHOR_END: names

-- ANCHOR: about
-- 💬 The comment lines right above a line (the line of main's
--    annotation): what --help says about the program.
-- ある行（main の注釈の行）のすぐ上のコメントの行：--help が
-- プログラムについて言うこと
aboutAbove :: Text -> Int -> [Text]
aboutAbove source line =
  filter (not . isMark) $ map (T.strip . T.drop 2)
    (reverse (takeWhile isComment (reverse (take (line - 1) ls))))
  where
    ls = map T.strip (T.lines source)
    isComment l = "--" `T.isPrefixOf` l
    -- The marks of the book's code (ANCHOR: main) are not words for
    -- the reader of --help.
    -- 本のコードの印（ANCHOR: main）は、--help を読む人への言葉ではない
    isMark l = any (`T.isPrefixOf` l) ["ANCHOR: ", "ANCHOR_END: "]
-- ANCHOR_END: about

-- ANCHOR: outcome
-- 🚦 What the words of the command line come to: the help (to the
--    screen, exit 0), a mistake (to the standard error, exit 2), or a
--    call of main with its record.
-- コマンドラインの語の行き先：ヘルプ（画面へ、終了コード 0）、まちがい
-- （標準エラーへ、終了コード 2）、main の呼び出しとそのレコード
data Outcome
  = Help Text
  | Mistake Text
  | Call (Maybe Name) (Maybe Value)   -- the subcommand, and the record

decide :: Cli -> [Text] -> Outcome
decide cli ws = case cliRuns cli of
  One c   -> runOne cli Nothing c ws
  Many cs -> pickCommand cli cs ws

-- 🚦 One command: --help, or read the words into its record.
-- コマンド1つ：--help か、語を読んでレコードにする
runOne :: Cli -> Maybe Name -> Command -> [Text] -> Outcome
runOne cli sub c ws
  | wantsHelp ws = Help (renderHelp cli sub)
  | otherwise    = either (Mistake . wrong cli sub) (Call sub) (readArgs c ws)

-- 🚦 What is wrong, with the usage, for the standard error:
--      error: unknown option --cont
--      Usage: greet.lam [OPTIONS] NAME
--      Try "greet.lam --help" for help.
-- まちがいと使い方。標準エラーに出す
wrong :: Cli -> Maybe Name -> Text -> Text
wrong cli sub msg = "error: " <> msg <> "\n"
  <> usage cli sub <> "\n"
  <> "Try \"" <> runName cli sub <> " --help\" for help.\n"
-- ANCHOR_END: outcome

-- ANCHOR: subcommands
-- 🚦 Subcommands: --help before the name of a command is the help of
--    the program. Otherwise the first word is the command, and the
--    rest are its words.
-- サブコマンド：コマンドの名前より前の --help は、プログラムのヘルプ。
-- そうでなければ、最初の語がコマンドで、残りがその語
pickCommand :: Cli -> [(Name, [Text], Command)] -> [Text] -> Outcome
pickCommand cli cs ws = case break (not . isOption) ws of
  (before, _) | wantsHelp before -> Help (renderHelp cli Nothing)
  (w : _, _) -> Mistake (wrong cli Nothing ("unknown option " <> w))
  ([], [])   -> Mistake (wrong cli Nothing "missing a command")
  ([], name : rest) ->
    case [ (n, c) | (n, _, c) <- cs, kebab n == name ] of
      (n, c) : _ -> runOne cli (Just n) c rest
      []         -> Mistake (wrong cli Nothing ("unknown command " <> name))
-- ANCHOR_END: subcommands

-- ANCHOR: isOption
-- ❓ --help before the -- (after it, it is just a word).
-- -- より前の --help（後ろなら、ただの語）
wantsHelp :: [Text] -> Bool
wantsHelp ws = "--help" `elem` takeWhile (/= "--") ws

-- ❓ An option is a word that starts with - but is not a number.
-- オプションは、- で始まる語のうち、数でないもの
isOption :: Text -> Bool
isOption w =
  "-" `T.isPrefixOf` w && T.length w > 1 && not (isNumber w)
  where
    isNumber x = isJust (wholeNumber x) || isJust (readFloat x)
-- ANCHOR_END: isOption

-- ANCHOR: readArgs
-- 📥 Read the words for one command: the options by their names, the
--    rest by their places. The answer is the record for main (Nothing
--    for a subcommand that takes none), or what is wrong.
-- コマンド1つの語を読む：オプションは名前で、ほかは場所で。答えは
-- main に渡すレコード（受け取らないサブコマンドなら Nothing）か、
-- どこがいけないか
readArgs :: Command -> [Text] -> Either Text (Maybe Value)
readArgs (Command opts takes) ws = do
  (given, places) <- collect Map.empty [] ws
  values <- fill given places opts
  pure (if takes then Just (VRecord values) else Nothing)
  where
    byName = Map.fromList [ (n, o) | o <- opts, n <- optionNames o ]
    -- Walk the words. given: the words of each option, in order.
    -- 語を順に見る。given：オプションごとの語（順に）
    collect given places rest = case rest of
      [] -> Right (given, reverse places)
      "--" : more -> Right (given, reverse places <> more)
      w : more
        | isOption w -> do
            let (n, inline) = T.breakOn "=" w
                value = T.drop 1 inline <$ (if T.null inline then Nothing
                                                               else Just ())
            o <- maybe (Left ("unknown option " <> n)) Right
                   (Map.lookup n byName)
            case (optKind o, value) of
              (Flag _, Just _) -> Left (n <> " takes no value")
              (Flag _, Nothing) ->
                collect (add o "" given) places more
              (_, Just v) -> collect (add o v given) places more
              -- The next word is the value, even if it starts with -.
              -- 次の語が値（- で始まっていても）
              (_, Nothing) -> case more of
                v : more' -> collect (add o v given) places more'
                []        -> Left (n <> " needs a value")
        | otherwise -> collect given (w : places) more
    add o v = Map.insertWith (flip (<>)) (optField o) [v]

-- 📥 Give every field its value: from the words, or the default.
-- どのフィールドにも値を渡す：語からか、既定値から
fill :: Map Name [Text] -> [Text] -> [Opt] -> Either Text (Map Name Value)
fill given places opts = do
  (left, values) <- foldM one (places, Map.empty) opts
  unless (null left) $
    -- 「よけいな引数：x y」
    Left ("unexpected argument: " <> T.unwords left)
  pure values
  where
    one (left, acc) o = do
      let x    = optField o
          put v = Right (left, Map.insert x v acc)
          seen = Map.findWithDefault [] x given
          name = optionNames o
          shownName = case name of
            n : _ -> n
            []    -> metavar x
      case optKind o of
        Argument r -> case left of
          w : more -> do
            v <- readWord r (metavar x) w
            Right (more, Map.insert x v acc)
          -- 「引数 NAME がありません」
          [] -> Left ("missing the argument " <> metavar x)
        Rest r -> do
          vs <- traverse (readWord r (metavar x)) left
          Right ([], Map.insert x (VList vs) acc)
        Valued r d -> case reverse seen of
          w : _ -> readWord r shownName w >>= put
          []    -> put (literalValue r d)
        Optional r -> case reverse seen of
          w : _ -> readWord r shownName w >>= put . maybeValue . Just
          []    -> put (maybeValue Nothing)
        Repeated r ds -> case seen of
          [] -> put (VList (map (literalValue r) ds))
          _  -> traverse (readWord r shownName) seen >>= put . VList
        Flag b -> put (VBool (if null seen then b else not b))

-- 🔤 Read one word as the field wants it.
-- 語1つを、フィールドが欲しい形に読む
readWord :: Reading -> Text -> Text -> Either Text Value
readWord r who w = case r of
  AString -> Right (VString w)
  -- 「--count には整数が要ります。"abc" ではなく」
  AnInt   -> maybe (Left (who <> " needs a whole number, not "
                          <> quoted w)) (Right . VInt) (wholeNumber w)
  AFloat  -> maybe (Left (who <> " needs a number, not " <> quoted w))
               (Right . VFloat) (readFloat w)
  where
    quoted t = "\"" <> t <> "\""

wholeNumber :: Text -> Maybe Integer
wholeNumber w = case TR.signed TR.decimal w of
  Right (n, rest) | T.null rest -> Just n
  _                             -> Nothing

-- 🎚️ The value of a default.
-- 既定値の値
literalValue :: Reading -> Literal -> Value
literalValue r l = case (r, l) of
  (AFloat, LInt n) -> VFloat (fromInteger n)
  (_, LInt n)      -> VInt n
  (_, LFloat x)    -> VFloat x
  (_, LBool b)     -> VBool b
  (_, LString t)   -> VString t
-- ANCHOR_END: readArgs

-- ANCHOR: help
-- 📖 The text of --help: the usage, the comments above main (or the
--    subcommand), the arguments, and the options (or the commands).
-- --help の文：使い方、main（かサブコマンド）の上のコメント、引数、
-- オプション（かコマンド）
renderHelp :: Cli -> Maybe Name -> Text
renderHelp cli sub = T.unlines $
  [ usage cli sub ] <> paragraph about <> sections
  where
    (about, sections) = case (cliRuns cli, sub) of
      (One c, _) -> (cliAbout cli, commandSections c)
      (Many cs, Nothing) ->
        ( cliAbout cli
        , section "Commands" [ (kebab n, T.unwords d) | (n, d, _) <- cs ]
          <> section "Options" [helpRow] )
      (Many cs, Just n) ->
        case [ (d, c) | (m, d, c) <- cs, m == n ] of
          (d, c) : _ -> (d, commandSections c)
          []         -> ([], [])
    paragraph ls
      | null ls   = []
      | otherwise = [""] <> map indent ls
    indent l = if T.null l then "" else "  " <> l
    commandSections c =
      let (args, opts) = partition (isPositional . optKind) (commandOpts c)
      in section "Arguments" (map argumentRow args)
           <> section "Options" (map optionRow opts <> [helpRow])
    helpRow = ("--help", "Show this message and exit")

-- 📖 A section: its title, then the rows, the texts lined up.
-- 節：題と、行。説明の頭をそろえる
section :: Text -> [(Text, Text)] -> [Text]
section _ [] = []
section title rows =
  ["", title <> ":"] <> [ T.stripEnd ("  " <> T.justifyLeft width ' ' l
                                        <> "  " <> r)
                        | (l, r) <- rows ]
  where
    width = maximum (map (T.length . fst) rows)

-- 📖 One row of an argument, and of an option.
-- 引数の1行と、オプションの1行
argumentRow :: Opt -> (Text, Text)
argumentRow o = case optKind o of
  Rest _ -> (metavar (optField o) <> "...", T.unwords (optDoc o))
  _      -> ( metavar (optField o)
            , T.intercalate "  " (filter (not . T.null)
                [T.unwords (optDoc o), "[required]"]) )

optionRow :: Opt -> (Text, Text)
optionRow o = (T.intercalate ", " (shorts <> [long]) <> value, about)
  where
    names  = optionNames o
    long   = case names of
      n : _ -> n
      []    -> ""
    shorts = drop 1 names
    value = case optKind o of
      Valued r _   -> " " <> typeName r
      Optional r   -> " " <> typeName r
      Repeated r _ -> " " <> typeName r
      _            -> ""
    about = T.intercalate "  " (filter (not . T.null)
              [T.unwords (optDoc o), T.unwords extra])
    extra = case optKind o of
      Valued _ d     -> ["[default: " <> shownLiteral d <> "]"]
      Repeated _ []  -> ["[can repeat]"]
      Repeated _ ds  -> ["[can repeat; default: "
                         <> T.intercalate ", " (map shownLiteral ds) <> "]"]
      _              -> []
    typeName r = case r of
      AString -> "STRING"
      AnInt   -> "INT"
      AFloat  -> "FLOAT"

shownLiteral :: Literal -> Text
shownLiteral l = case l of
  LInt n    -> T.pack (show n)
  LFloat x  -> showFloat x
  LBool b   -> if b then "true" else "false"
  LString t -> t

-- 📖 Usage: greet.lam [OPTIONS] NAME
usage :: Cli -> Maybe Name -> Text
usage cli sub = "Usage: " <> runName cli sub <> case (cliRuns cli, sub) of
  (Many _, Nothing) -> " COMMAND [ARGS]..."
  (One c, _)        -> shape c
  (Many cs, Just n) -> case [ c | (m, _, c) <- cs, m == n ] of
    c : _ -> shape c
    []    -> ""
  where
    shape c = " [OPTIONS]" <> T.concat
      [ " " <> metavar (optField o) <> (if isRest (optKind o) then "..."
                                                              else "")
      | o <- commandOpts c, isPositional (optKind o) ]

-- 📖 greet.lam, or todo.lam add for a subcommand.
runName :: Cli -> Maybe Name -> Text
runName cli sub = cliProgram cli <> maybe "" ((" " <>) . kebab) sub
-- ANCHOR_END: help

-- ANCHOR: withCli
-- 🔌 Put $cli into the session: a builtin that takes the words of the
--    command line, and gives the IO action to do. It is what
--    startMain calls (bind getArgs $cli).
-- セッションに $cli を入れる：コマンドラインの語を受け取り、やる IO の
-- 頼みごとをくれる組み込み。startMain が呼ぶもの（bind getArgs $cli）
withCli :: Cli -> Session -> Session
withCli cli s = s
  { sessionTypes  = Map.insert "$cli" (Forall [] Map.empty
                      (TFun (tList tString) (tIO tUnit))) (sessionTypes s)
  , sessionValues = Map.insert "$cli" (VPrim "$cli" 1 run [])
                      (sessionValues s) }
  where
    mainValue = fromMaybe (VTuple []) (Map.lookup "main" (sessionValues s))
    run args = case args of
      [VList ws] -> do
        words' <- traverse text ws
        case decide cli words' of
          Help t    -> Right (say PutStr t 0)
          Mistake t -> Right (say PutErr t 2)
          Call sub record -> do
            f <- case (sub, mainValue) of
              (Nothing, m)           -> Right m
              (Just n, VRecord fs)   -> maybe (Left (NoField n "main")) Right
                                          (Map.lookup n fs)
              (Just n, _)            -> Left (NoField n "main")
            maybe (Right f) (apply f) record
      _ -> Left (NotAFunction "$cli")
    -- Write the text, then end with the code.
    -- 文字を書いて、終了コードで終わる
    say put t code = VIO (Bind (Ask (put t)) (\_ -> Right (Exit code)))
    text v = case v of
      VString t -> Right t
      other     -> Left (NotAFunction (renderValue other))
-- ANCHOR_END: withCli
-- ANCHOR_END: all
