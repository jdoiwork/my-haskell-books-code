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
  , prepareCli
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
import Data.List (partition, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import Lam.Builtins (maybeValue)
import Lam.Eval
import Lam.Float (readFloat)
import Lam.Pretty (showFloat)
import Lam.Program (Code (..), MainShape (..), Session (..), mainCli, mainShape)
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: cliHead

-- ANCHOR: cliTypes
-- 🧭 The whole command line: the name of the program, the help above
--    main, and what it runs.
-- コマンドラインの全体：プログラムの名前、main の上のヘルプ、動かすもの
data Cli = Cli
  { cliProgram :: Text          -- greet.lam
  , cliAbout   :: [Text]        -- the help above main
  , cliRuns    :: Runs
  }

-- 🧭 One command (main : { … } -> IO ()), or subcommands, each with
--    its name and its help (main : { add : …, list : … }).
-- コマンド1つか、サブコマンド（それぞれ名前とヘルプを持つ）
data Runs
  = One Command
  | Many [(Name, [Text], Command)]

-- 🧭 One command: its options and arguments, in the written order.
--    takesRecord is False for a subcommand that is an IO () alone.
-- コマンド1つ：オプションと引数を、書いた順に。takesRecord は、
-- IO () だけのサブコマンドなら False
data Command = Command
  { commandOpts :: [Opt]
  , takesRecord :: Bool
  }

-- 🏷️ One field of the record: an option or an argument.
-- レコードのフィールド1つ：オプションか引数
data Opt = Opt
  { optField :: Name            -- dryRun
  , optDoc   :: [Text]          -- the help above the field
  , optShort :: [Name]          -- ["d"], from  alias d = dryRun
  , optKind  :: Kind
  }

-- 🏷️ What the type (and the default) of a field make it. A default
--    is kept as a value, and as the text --help shows.
-- フィールドの型（と既定値）が、それを何にするか。既定値は、値と、
-- --help が見せる文字で持つ
data Kind
  = Argument Reading                  -- name : String       NAME
  | Rest Reading                      -- files : [String]    FILES...
  | Valued Reading (Value, Text)      -- count : Int = 1     --count INT
  | Optional Reading                  -- title : Maybe String
  | Repeated Reading [(Value, Text)]  -- tag : [String] = []
  | Flag Bool                         -- loud : Bool (= true)
-- ANCHOR_END: cliTypes

-- ANCHOR: reading
-- 🔤 How to read one word: the name in the help (INT, LEVEL), and the
--    method fromArg of the instance of FromArg for the type.
-- 語1つの読み方：ヘルプでの名前（INT、LEVEL）と、その型の FromArg の
-- インスタンスのメソッド fromArg
data Reading = Reading
  { readingName :: Text
  , readWith    :: Text -> Either Text Value
  }
-- ANCHOR_END: reading

-- ANCHOR: prepareCli
-- 🔌 Before main starts: when main takes a record (or is a record of
--    subcommands), make its command line, and put $main.cli into the
--    session. Any other main goes on as it is.
-- main を始める前に：main がレコードを受け取る（かサブコマンドの
-- レコードな）ら、コマンドラインを作り、セッションに $main.cli を入れる。
-- ほかの main は、そのまま
prepareCli :: Session -> Either Text Session
prepareCli s = case mainShape <$> Map.lookup "main" (sessionTypes s) of
  Just shape | shape `elem` [MainOptions, MainCommands] ->
    either (\err -> Left ("in main: " <> err)) (\cli -> Right (withCli cli s))
      (commandLine s)
  _ -> Right s
-- ANCHOR_END: prepareCli

-- ANCHOR: commandLine
-- 🧭 Make the command line of a program, from the type of main and
--    its annotation (if there is one). Without an annotation there is
--    no help and no default, and the fields come in the order of
--    their names.
-- プログラムのコマンドラインを、main の型と注釈（あれば）から作る。
-- 注釈がなければ、ヘルプも既定値もなく、フィールドは名前の順
commandLine :: Session -> Either Text Cli
commandLine s = do
  Forall _ _ t <- maybe (Left "there is no main") (Right . defaultNumbers)
                    (Map.lookup "main" (sessionTypes s))
  let (file, source) = maybe ("main.lam", "") id (sessionProgram s)
      shown  = fmap (\(Signature _ te) -> te)
                    (Map.lookup "main" (sessionSigs s))
      record = recordSyntax (sessionData s)
  runs <- case t of
    TFun (TRecord fs _) _ ->
      One <$> command s fs (record =<< argumentOf =<< shown)
    TRecord cs _ ->
      fmap Many $ traverse (subcommand record) $
        inOrder cs (record =<< shown)
    _ -> Left "main does not read the command line"
  pure (Cli (T.pack (takeName file)) (aboutAbove source) runs)
  where
    argumentOf te = case te of
      TyFun a _ -> Just a
      _         -> Nothing
    subcommand record (f, ct) = case ct of
      TFun (TRecord fs _) _ -> do
        c <- command s fs (record =<< argumentOf (fieldType f))
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

-- 📇 The fields with their types: in the written order (with the help
--    and the defaults) when the syntax is there, otherwise in the
--    order of the names.
-- フィールドと型：書いたとおりのものがあれば、その順に（ヘルプと
-- 既定値も）。なければ、名前の順に
inOrder :: Map Name Type -> Maybe [FieldType] -> [(FieldType, Type)]
inOrder types written = case written of
  Just fs | sort (map fieldName fs) == Map.keys types ->
    [ (f, types Map.! fieldName f) | f <- fs ]
  _ -> [ (plainField x (TyCon "?" []), t) | (x, t) <- Map.toList types ]
-- ANCHOR_END: recordSyntax

-- ANCHOR: command
-- 🧭 One command, from the fields of its record. Checked here, once:
--    each field's type can be read, at most one takes the rest, and
--    no two options have the same name.
-- レコードのフィールドから、コマンド1つを作る。ここで1度だけ確かめる：
-- どのフィールドの型も読める、残り全部を取るのは1つまで、
-- 同じ名前のオプションが2つない
command :: Session -> Map Name Type -> Maybe [FieldType]
        -> Either Text Command
command s types written = do
  opts <- traverse (option s) (inOrder types written)
  let positional = [ o | o <- opts, isPositional (optKind o) ]
  case dropWhile (not . isRest . optKind) positional of
    r : _ : _ -> Left ("the argument " <> metavar (optField r)
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
-- ANCHOR_END: command

-- ANCHOR: option
-- 🏷️ One field: what its type and default make it.
-- フィールド1つ：型と既定値が、それを何にするか
option :: Session -> (FieldType, Type) -> Either Text Opt
option s (f, t) = Opt x (fieldDoc f) (fieldShort f) <$> kind
  where
    x = fieldName f
    kind = case (t, fieldDefault f) of
      (TBool, Nothing)                      -> Right (Flag False)
      (TBool, Just (DefaultOne (LBool b)))  -> Right (Flag b)
      (TCon "Maybe" [a], Nothing)           -> Optional <$> reading a
      (TCon "List" [a], Nothing)            -> Rest <$> reading a
      (TCon "List" [a], Just (DefaultList ds)) ->
        Repeated <$> reading a <*> traverse (defaultOf a) ds
      (_, Nothing)                          -> Argument <$> reading t
      (_, Just d)                           ->
        Valued <$> reading t <*> defaultOf t d
    reading a = readingFor s x t a
    defaultOf a d = defaultValueOf s a d
-- ANCHOR_END: option

-- ANCHOR: readingFor
-- 🔤 How to read a word as type a: by the instance of FromArg for a.
--    There is one for String, Int and Float in the module Cli, and a
--    program may write one for its own type (instance FromArg Level).
--    The dictionary is found once, here, before main starts.
-- 語を型 a として読む方法：a の FromArg のインスタンスで。String・Int・
-- Float のものはモジュール Cli にあり、プログラムは自分の型のものを
-- 書ける。辞書は、main を始める前に、ここで1度だけ探す
readingFor :: Session -> Name -> Type -> Type -> Either Text Reading
readingFor s x whole a =
  case dictFor (sessionData s) [] (Declared "FromArg") a of
    -- 「--when の型 Time は、コマンドラインから読めません
    --  （FromArg のインスタンスがありません）」
    Left _ -> Left ("the field " <> x <> " has the type "
                    <> renderType whole
                    <> ", which cannot be read from the command line "
                    <> "(there is no instance FromArg "
                    <> renderType a <> ")")
    Right dict -> Right (Reading (nameOf a) (readBy dict))
  where
    readBy dict w =
      let call = App (Field dict "fromArg") (Lit (LString w))
      in case eval (sessionValues s) call of
        Right (VCon "Ok" _ [v])            -> Right v
        Right (VCon "Err" _ [VString msg]) -> Left msg
        Right other                        -> Left (renderValue other)
        Left err                           -> Left (renderError err)
    nameOf ty = case ty of
      TInt      -> "INT"
      TCon c _  -> T.toUpper c
      _         -> "VALUE"

-- 🎚️ The value of a default, and its text in --help. A whole number
--    for a Float is a Float; a constructor (Normal) is its value.
-- 既定値の値と、--help での文字。Float の整数は Float、構成子（Normal）
-- は、その値
defaultValueOf :: Session -> Type -> Default -> Either Text (Value, Text)
defaultValueOf s t d = case d of
  DefaultOne l -> Right (literalValue t l, shownLiteral l)
  DefaultCon c -> case eval (sessionValues s) (Var c) of
    Right v  -> Right (v, c)
    Left err -> Left (renderError err)
  DefaultList _ -> Left "a list cannot be the default of one value"

literalValue :: Type -> Literal -> Value
literalValue t l = case (t, l) of
  (TCon "Float" [], LInt n) -> VFloat (fromInteger n)
  (_, LInt n)               -> VInt n
  (_, LFloat x)             -> VFloat x
  (_, LBool b)              -> VBool b
  (_, LString w)            -> VString w

shownLiteral :: Literal -> Text
shownLiteral l = case l of
  LInt n    -> T.pack (show n)
  LFloat x  -> showFloat x
  LBool b   -> if b then "true" else "false"
  LString w -> w
-- ANCHOR_END: readingFor

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
-- 💬 The lines of help (---) among the comments right above the
--    annotation of main (the line that starts with "main :"): what
--    --help says about the program. The other comments there
--    (-- ANCHOR: main, the Japanese line) are skipped, but do not end
--    the comments.
-- main の注釈の行（"main :" で始まる行）のすぐ上のコメントのうち、
-- ヘルプの行（---）：--help がプログラムについて言うこと。そこのほかの
-- コメント（-- ANCHOR: main や日本語の行）は飛ばすが、コメントの続きと
-- して数える
aboutAbove :: Text -> [Text]
aboutAbove source =
  [ h | Just h <- map helpLine
          (reverse (takeWhile isComment (reverse above))) ]
  where
    ls    = T.lines source
    above = map T.strip (takeWhile (not . isMainLine) ls)
    isMainLine l = "main :" `T.isPrefixOf` l || "main:" `T.isPrefixOf` l
    isComment l = "--" `T.isPrefixOf` l
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

wholeNumber :: Text -> Maybe Integer
wholeNumber w = case TR.signed TR.decimal w of
  Right (n, rest) | T.null rest -> Just n
  _                             -> Nothing
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
  (given, places) <- collect opts Map.empty [] ws
  values <- fill given places opts
  pure (if takes then Just (VRecord values) else Nothing)

-- 📥 Walk the words. given: the words of each option, in order;
--    places: the other words, for the arguments.
-- 語を順に見る。given：オプションごとの語（順に）。places：ほかの語
-- （引数のため）
collect :: [Opt] -> Map Name [Text] -> [Text] -> [Text]
        -> Either Text (Map Name [Text], [Text])
collect opts given places rest = case rest of
  [] -> Right (given, reverse places)
  "--" : more -> Right (given, reverse places <> more)
  w : more
    | isOption w -> do
        (o, v, more') <- optionWord opts w more
        collect opts (Map.insertWith (flip (<>)) (optField o) [v] given)
                places more'
    | otherwise -> collect opts given (w : places) more

-- 📥 One option, and its value: --count=2, or --count 2 (the next word
--    is the value, even if it starts with -), or a flag (no value).
-- オプション1つと、その値：--count=2、--count 2（次の語が値。- で
-- 始まっていても）、またはフラグ（値なし）
optionWord :: [Opt] -> Text -> [Text] -> Either Text (Opt, Text, [Text])
optionWord opts w more = do
  o <- maybe (Left ("unknown option " <> n)) Right (Map.lookup n byName)
  case (optKind o, inline, more) of
    (Flag _, Just _, _)  -> Left (n <> " takes no value")
    (Flag _, Nothing, _) -> Right (o, "", more)
    (_, Just v, _)       -> Right (o, v, more)
    (_, Nothing, v : more') -> Right (o, v, more')
    (_, Nothing, [])     -> Left (n <> " needs a value")
  where
    byName = Map.fromList [ (m, o) | o <- opts, m <- optionNames o ]
    (n, rest) = T.breakOn "=" w
    inline = if T.null rest then Nothing else Just (T.drop 1 rest)

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
          shownName = case optionNames o of
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
        Valued r (d, _) -> case reverse seen of
          w : _ -> readWord r shownName w >>= put
          []    -> put d
        Optional r -> case reverse seen of
          w : _ -> readWord r shownName w >>= put . maybeValue . Just
          []    -> put (maybeValue Nothing)
        Repeated r ds -> case seen of
          [] -> put (VList (map fst ds))
          _  -> traverse (readWord r shownName) seen >>= put . VList
        Flag b -> put (VBool (if null seen then b else not b))

-- 🔤 Read one word with the instance of FromArg. Its Err says what is
--    needed:  --count needs a whole number, not "abc"
-- 語1つを FromArg のインスタンスで読む。Err は、何が要るかを言う
readWord :: Reading -> Text -> Text -> Either Text Value
readWord r who w = case readWith r w of
  Right v   -> Right v
  Left need -> Left (who <> " needs " <> need <> ", not \"" <> w <> "\"")
-- ANCHOR_END: readArgs

-- ANCHOR: help
-- 📖 The text of --help: the usage, the help above main (or the
--    subcommand), the arguments, and the options (or the commands).
-- --help の文：使い方、main（かサブコマンド）の上のヘルプ、引数、
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
      Valued r _   -> " " <> readingName r
      Optional r   -> " " <> readingName r
      Repeated r _ -> " " <> readingName r
      _            -> ""
    about = T.intercalate "  " (filter (not . T.null)
              [T.unwords (optDoc o), T.unwords extra])
    extra = case optKind o of
      Valued _ (_, d) -> ["[default: " <> d <> "]"]
      Repeated _ []   -> ["[can repeat]"]
      Repeated _ ds   -> ["[can repeat; default: "
                          <> T.intercalate ", " (map snd ds) <> "]"]
      _               -> []

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
-- 🔌 Put $main.cli into the session: a builtin that takes main and the
--    words of the command line, and gives the IO action to do. It is
--    what startMain calls (bind getArgs ($main.cli main)). Its type
--    says that it gives what main gives: IO (), or IO (Result e ()).
-- セッションに $main.cli を入れる：main とコマンドラインの語を受け取り、
-- やる IO の頼みごとをくれる組み込み。startMain が呼ぶもの。型は、main が
-- くれるものをくれる、と言う：IO () か IO (Result e ())
withCli :: Cli -> Session -> Session
withCli cli s = s
  { sessionTypes  = Map.insert mainCli (cliType (cliRuns cli))
                      (sessionTypes s)
  , sessionValues = Map.insert mainCli (VPrim mainCli 2 run [])
                      (sessionValues s)
  -- ANCHOR: cliCode
  , sessionCode   = Map.insert mainCli (Primitive mainCli)
                      (sessionCode s) }
  -- ANCHOR_END: cliCode
  where
    run args = case args of
      [mainValue, VList ws] -> do
        words' <- traverse text ws
        case decide cli words' of
          Help t          -> Right (say PutStr t 0)
          Mistake t       -> Right (say PutErr t 2)
          Call sub record -> callMain mainValue sub record
      _ -> Left (NotAFunction mainCli)
    -- Write the text, then end with the code.
    -- 文字を書いて、終了コードで終わる
    say put t code = VIO (Bind (Ask (put t)) (\_ -> Right (Exit code)))
    text v = case v of
      VString t -> Right t
      other     -> Left (NotAFunction (renderValue other))

-- 🔌 Call main (or its subcommand) with the record, if it takes one.
-- main（かそのサブコマンド）を、レコードを受け取るなら、それで呼ぶ
callMain :: Value -> Maybe Name -> Maybe Value -> Either EvalError Value
callMain mainValue sub record = do
  f <- case (sub, mainValue) of
    (Nothing, m)         -> Right m
    (Just n, VRecord fs) -> maybe (Left (NoField n "main")) Right
                              (Map.lookup n fs)
    (Just n, _)          -> Left (NoField n "main")
  maybe (Right f) (apply f) record

-- 🔌 The type of $main.cli: main, then the words, then what main gives.
--      One command:  (r -> IO b) -> [String] -> IO b
--      Subcommands:  { add : r1 -> IO b, list : IO b } -> [String] -> IO b
--    Every subcommand gives the same b.
-- $main.cli の型：main、語、main がくれるもの。サブコマンドは、どれも
-- 同じ b をくれる
cliType :: Runs -> Scheme
cliType runs = case runs of
  One _ -> Forall [0, 1] Map.empty
    (TFun (TFun (TVar 1) (tIO answer)) (TFun (tList tString) (tIO answer)))
  Many cs ->
    let field (i, (n, _, c))
          | takesRecord c = (n, TFun (TVar i) (tIO answer))
          | otherwise     = (n, tIO answer)
        fields = map field (zip [1 ..] cs)
    in Forall [0 .. length cs] Map.empty
         (TFun (TRecord (Map.fromList fields) Nothing)
               (TFun (tList tString) (tIO answer)))
  where
    answer = TVar 0
-- ANCHOR_END: withCli
-- ANCHOR_END: all
