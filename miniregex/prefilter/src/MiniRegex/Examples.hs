-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE MultilineStrings #-}
-- | The records of the guild's archive room, a search by hand, and the
--   code we want to write with MiniRegex.
-- ギルドの記録室の記録と、手で書いた探しものと、MiniRegex で書きたい
-- コード
module MiniRegex.Examples
  ( records
  , datesByHand
  , looksLikeDate
  , findDates
  , countDragons
  , renameDragons
  , splitLoot
  , surely
  , see
  , endsOf
  , endsAtStart
  , datePattern
  , dateParts
  , toDayFirst
  , toDayFirstT
  , notes
  , doubled
  , doubledWords
  , oldRecords
  , yearsIn
  , datePatternX
  , monthOf
  , ledger
  , goldAmounts
  , goodPassword
  , prices
  , commas
  , pricesK
  , windowsNote
  , linesR
  , seeProgram
  , stepsOf
  , nameField
  , wordsOnly
  , windowLimit
  , checkName
  , stepsBoth
  , drawers
  , memo
  , innerNotes
  , wholeNotes
  , namesWithNotes
  , palindrome
  , period
  , periodOf
  , backtracking
  , stepsWith
  , fieldOf
  , checkNameSlowly
  , userPatterns
  , safeCompile
  , everywhere
  , byKmp
  , seePrefilter
  , describePrefilter
  , triedPlaces
  , archive
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T

import Data.Text.IO qualified as TIO

import MiniRegex
import MiniRegex.Candidates (ends)
import MiniRegex.Compile (listing, showInst)
import MiniRegex.Cursor (Cursor (..), cursorAt)
import MiniRegex.Engine (runnerFor, searchCounting, searchCountingBy)
import MiniRegex.Parser (parsePattern)
import MiniRegex.Pretty (drawPattern)
import MiniRegex.STVM (scanVMST, slotsOf)
import MiniRegex.Pike (blocker)
import MiniRegex.Prefilter
  ( Finder (..), Prefilter (..), Start (..), noPrefilter, places )
import MiniRegex.Program (Inst (Take))
import MiniRegex.Types (Regex (..), program, tree)
import MiniRegex.VM (scanVM)
-- ANCHOR_END: imports

-- ANCHOR: records
-- 📜 A page of the old records.
-- 古い記録の1ページ
records :: Text
records = T.unlines
  [ "2026-11-19 Mira sold a dragon scale: 300 gold"
  , "2026-11-20 Ruby bought herb, sword, shield"
  , "On 2026-11-21 a dragon was seen near the lake"
  , "Room 1234-56-789 is closed"
  ]
-- ANCHOR_END: records

-- ANCHOR: byhand
-- ✋ The dates like 2026-11-19, found by hand: look at 10 characters
--    from each place, and keep them if they look like a date.
-- 2026-11-19 のような日付を、手で探す：1文字ずつずらしながら10文字を
-- 取り出し、日付に見えたら残す
datesByHand :: Text -> [Text]
datesByHand t
  | T.length t < 10         = []
  | looksLikeDate candidate = candidate : datesByHand (T.drop 10 t)
  | otherwise               = datesByHand (T.drop 1 t)
  where
    candidate = T.take 10 t

-- 🗓️ Four digits, -, two digits, -, two digits.
-- 数字4つ、-、数字2つ、-、数字2つ
looksLikeDate :: Text -> Bool
looksLikeDate d =
  T.length d == 10
    && T.all isDigit year
    && T.index d 4 == '-'
    && T.all isDigit month
    && T.index d 7 == '-'
    && T.all isDigit day
  where
    year  = T.take 4 d
    month = T.take 2 (T.drop 5 d)
    day   = T.take 2 (T.drop 8 d)
-- ANCHOR_END: byhand

-- ANCHOR: wanted
-- 📅 The dates, the way we want to write it. A wrong pattern stops
--    at compile, before any record is read.
-- 書きたい形で日付を探す。まちがったパターンは compile で止まり、
-- 記録を読む前にわかる
-- From the chapter on looking around, \b at both ends: not in the
-- middle of a run of digits.
-- まわりを見る章から、両端に \b：数字が続く途中では切らない
findDates :: Text -> Either RegexError [Text]
findDates t = do
  date <- compile "\\b[0-9]{4}-[0-9]{2}-[0-9]{2}\\b"
  pure (map matchText (findAll date t))

-- 🐉 How many times a dragon is in the records.
-- 記録の中に、ドラゴンが何回出てくるか
countDragons :: Text -> Either RegexError Int
countDragons t = do
  dragon <- compile "dragon"
  pure (length (findAll dragon t))

-- 🦎 The guild calls them wyverns now.
-- ギルドは、いまはワイバーンと呼ぶことにした
renameDragons :: Text -> Either RegexError Text
renameDragons t = do
  dragon <- compile "dragon"
  pure (replaceAll dragon "wyvern" t)

-- 🎒 "herb, sword, shield" into the items.
-- "herb, sword, shield" を、品物ごとに切る
splitLoot :: Text -> Either RegexError [Text]
splitLoot t = do
  comma <- compile ", "
  pure (split comma t)
-- ANCHOR_END: wanted

-- ANCHOR: surely
-- 🧪 For GHCi: a pattern we know is fine.
-- GHCi で試す用：正しいとわかっているパターン
surely :: Text -> Regex
surely src = either (error . show) id (compile src)
-- ANCHOR_END: surely

-- ANCHOR: see
-- 👀 For GHCi: read a pattern, and draw its tree, or point at the
--    trouble. It prints, so it is an IO.
-- GHCi で試す用：パターンを読んで、木を描く。まちがいなら、その場所を
-- 指す。画面に書くので IO
see :: Text -> IO ()
see src = case parsePattern [] src of
  Right tree -> TIO.putStr (drawPattern tree)
  Left err   -> TIO.putStr (explain src err)
-- ANCHOR_END: see

-- ANCHOR: ends
-- 🧺 For GHCi: the places (character positions) that the list matcher
--    lists, in order, when the pattern starts at the head of the text.
-- GHCi で試す用：パターンが文字列の頭から始まるとき、候補のリストの
-- マッチャーが並べる場所（何文字目か）を、順に
endsOf :: Text -> Text -> [Int]
endsOf p t = map (at . fst) (ends (tree (surely p)) (Cursor 0 Nothing t t))

-- 🧮 How many of them. The pieces after the pattern are tried once at
--    each of them.
-- その数。パターンのうしろの部品は、その1つずつで試される
endsAtStart :: Text -> Text -> Int
endsAtStart p t = length (endsOf p t)
-- ANCHOR_END: ends

-- ANCHOR: dates
-- 📅 A date, with a name for each part (and \b at both ends, from the
--    chapter on looking around).
-- 日付。部分ごとに名前を付けた（まわりを見る章から、両端に \b）
datePattern :: Regex
datePattern =
  surely "\\b(?<year>[0-9]{4})-(?<month>[0-9]{2})-(?<day>[0-9]{2})\\b"

-- 🗓️ Each date in the text, as (year, month, day).
-- 文字列の中の日付を、(年, 月, 日) の組で
dateParts :: Text -> [(Text, Text, Text)]
dateParts t =
  [ (part "year" m, part "month" m, part "day" m)
  | m <- findAll datePattern t ]

-- 🧩 A named group, or "" (the three of datePattern always take part).
-- 名前付きのグループ。なければ ""（datePattern の3つは、いつも使われる）
part :: Text -> Match -> Text
part name m = fromMaybe "" (named name m)
-- ANCHOR_END: dates

-- ANCHOR: dayfirst
-- 🔁 2026-11-19 into 19/11/2026, with a function.
-- 2026-11-19 を 19/11/2026 に。関数で
toDayFirst :: Text -> Text
toDayFirst = replaceWith datePattern dayFirst
  where
    dayFirst m = part "day" m <> "/" <> part "month" m <> "/" <> part "year" m
-- ANCHOR_END: dayfirst

-- ANCHOR: dayfirstT
-- 🧾 The same, with a template. A wrong template stops here, before
--    any record is read.
-- 同じことを、テンプレートで。まちがったテンプレートは、記録を読む前に、
-- ここで止まる
toDayFirstT :: Text -> Either RegexError Text
toDayFirstT t = do
  tpl <- template datePattern "${day}/${month}/${year}"
  pure (replaceAllTemplate datePattern tpl t)
-- ANCHOR_END: dayfirstT

-- ANCHOR: doubled
-- 📝 A clerk's note, with some words written twice.
-- 係の人のメモ。2回書いてしまった言葉がある
notes :: Text
notes = "Mira saw the the dragon near near the lake"

-- 🦜 A word, a space, and the same word again; a whole word each time
--    (\b, from the chapter on looking around).
-- 言葉、空白、そして同じ言葉をもう一度。どちらも言葉まるごと
-- （まわりを見る章から \b）
doubled :: Regex
doubled = surely "\\b([a-z]+) \\1\\b"

-- 🦜 The words written twice.
-- 2回書いてしまった言葉
doubledWords :: Text -> [Text]
doubledWords t = [w | m <- findAll doubled t, Just w <- [group 1 m]]
-- ANCHOR_END: doubled

-- ANCHOR: oldrecords
-- 📜 A page written by a clerk long ago: full-width digits, Japanese
--    names, and words in capitals.
-- ずっと昔の係の人が書いたページ：全角の数字、日本語の名前、大文字の言葉
oldRecords :: Text
oldRecords = T.unlines
  [ "２０２６年１１月１９日　ミラ　ドラゴンの鱗　３００ゴールド"
  , "2026-11-20 Ruby bought a NOTE at the CAFÉ"
  , "１１/２１　ルビー　ノート　２冊"
  ]

-- 📅 The years: four digits in a row, of any kind.
-- 年：どの種類でもよい、4つ並んだ数字
yearsIn :: Text -> [Text]
yearsIn t = map matchText (findAll (surely "\\d{4}") t)
-- ANCHOR_END: oldrecords

-- ANCHOR: xmode
-- 📝 A date of either kind, with notes in the pattern itself (?x):
--    the spaces and the # comments are skipped.
-- どちらの書き方の日付も。パターンの中に説明を書く（(?x)）：空白と
-- # のコメントは読み飛ばされる
datePatternX :: Regex
datePatternX = surely """
  (?x)
  (?<year>  \\d{4} )  [-年]      # 2026- or ２０２６年
  (?<month> \\d{1,2} ) [-月]     # 11- or １１月
  (?<day>   \\d{1,2} ) 日?       # 19 or １９日
  """
-- ANCHOR_END: xmode

-- ANCHOR: reset
-- 🗓️ The month, from 11月19日, 2026-11-19 or 11/19: (?| ... ) gives
--    each branch's group the same number, 1.
-- 11月19日・2026-11-19・11/19 から月を取り出す。(?| ... ) は、どの枝の
-- グループにも、同じ1番を付ける
monthOf :: Text -> Maybe Text
monthOf t = find months t >>= group 1
  where
    months = surely "(?|(\\d+)月|\\d{4}-(\\d+)-|(\\d+)/\\d+)"
-- ANCHOR_END: reset

-- ANCHOR: ledger
-- 📒 The page of the sixth day: prices, written in many ways.
-- 6日目のページ：いろんな書き方の値段
ledger :: Text
ledger = T.unlines
  [ "2026-11-22 Mira sold a scale for 300 gold, a herb for 5 silver"
  , "2026-11-22 Ruby: price:1200 gold, price: 80 gold, price:   7 gold"
  , "2026-11-23 The guild paid 1500000 gold for the new hall"
  ]
-- ANCHOR_END: ledger

-- ANCHOR: gold
-- 🪙 The numbers just before " gold": look ahead, take nothing.
-- " gold" のすぐ前の数：先を見るが、食べない
goldAmounts :: Text -> [Text]
goldAmounts t = map matchText (findAll (surely "\\d+(?= gold)") t)
-- ANCHOR_END: gold

-- ANCHOR: password
-- 🔑 A word for the guild's vault: 8 characters or more, with a digit
--    and a capital letter somewhere, and no "dragon" anywhere. Each
--    look checks the whole word from the start, and moves nothing.
-- ギルドの金庫の合言葉：8文字以上、どこかに数字と大文字があり、
-- どこにも "dragon" がない。どの「見る」も、頭から全体を確かめ、
-- しおりを動かさない
goodPassword :: Text -> Bool
goodPassword = isMatch (surely "^(?=.*\\d)(?=.*[A-Z])(?!.*dragon).{8,}$")
-- ANCHOR_END: password

-- ANCHOR: prices
-- 🏷️ The numbers just after "price:" and maybe a space: look behind.
-- "price:" と、もしかすると空白のすぐうしろの数：後ろを見る
prices :: Text -> [Text]
prices t = map matchText (findAll (surely "(?<=price:\\s?)\\d+") t)
-- ANCHOR_END: prices

-- ANCHOR: commas
-- 💰 A comma before every three digits from the end: a digit that has
--    some threes of digits after it, up to the end of the number.
-- うしろから3けたごとにカンマ：うしろに、3けたの組がいくつか続いて、
-- 数が終わる、そんな数字
commas :: Text -> Text
commas = replaceWith beforeThrees (\m -> matchText m <> ",")
  where
    beforeThrees = surely "\\d(?=(?:\\d{3})+\\b)"
-- ANCHOR_END: commas

-- ANCHOR: keep
-- ✂️ The same as prices, with any number of spaces: \K forgets what is
--    before it, so there is no limit to how long that can be.
-- prices と同じものを、空白がいくつでも。\K は手前を忘れるので、
-- 手前の長さに上限がない
pricesK :: Text -> [Text]
pricesK t = map matchText (findAll (surely "price:\\s*\\K\\d+") t)
-- ANCHOR_END: keep

-- ANCHOR: newlines
-- 🪟 A note written on three kinds of machines: \r\n, \n and \r.
-- 3種類の機械で書かれたメモ：\r\n・\n・\r
windowsNote :: Text
windowsNote = "Mira\r\nRuby\nLam\rGHC"

-- ↩️ The lines, whatever ends them.
-- 行に切る。何で終わっていても
linesR :: Text -> [Text]
linesR = split (surely "\\R")
-- ANCHOR_END: newlines

-- ANCHOR: program
-- 📋 For GHCi: the program the VM runs for a pattern.
-- GHCi で試す用：パターンから VM が動かすプログラム
seeProgram :: Text -> IO ()
seeProgram p = TIO.putStr (listing (program (surely p)))

-- 👣 For GHCi: how many steps the VM takes to search the text (with
--    no limit). From the linear-time chapter on, the VM is the one the
--    regex chose.
-- GHCi で試す用：VM が文字列を探すのに使うステップ数（上限なし）。線形時間の
-- 章からは、正規表現が選んだ VM で
stepsOf :: Text -> Text -> Int
stepsOf p t = snd (searchCounting maxBound (surely p) t 0)
-- ANCHOR_END: program

-- ANCHOR: window
-- 🪪 What someone wrote in the name field of the window: eight names,
--    and a ! by mistake.
-- 窓口の名前の欄に書かれたもの：名前が8つと、まちがえて入った !
nameField :: Text
nameField = "Mira Ruby Lam GHC Perl Mira Ruby Lam!"

-- 🔤 The check of the name field: words, each with a space after it,
--    or not.
-- 名前の欄の確かめ：言葉。うしろに空白があっても、なくてもよい
wordsOnly :: Regex
wordsOnly = surely "^(\\w+\\s?)+$"

-- 🪟 The window takes at most a hundred thousand steps for a check.
-- 窓口は、1回の確かめに、多くても10万ステップ
windowLimit :: Int
windowLimit = 100_000

-- ✅ Is the field only words? Left when the check gave up.
-- 欄は言葉だけか。確かめをあきらめたら Left
checkName :: Text -> Either GaveUp Bool
checkName t = isJust <$> findWithin windowLimit wordsOnly t
-- ANCHOR_END: window

-- ANCHOR: twovms
-- 👣 For GHCi: the steps of the pure VM, and of the VM with ST (with
--    no limit). They are always the same.
-- GHCi で試す用：純粋な VM と ST の VM のステップ数（上限なしで）。いつも同じ
stepsBoth :: Text -> Text -> (Int, Int)
stepsBoth p t = (steps scanVM, steps scanVMST)
  where
    steps vm = snd (searchCountingBy vm maxBound (surely p) t 0)

-- 🗄️ For GHCi: how many drawers the VM with ST makes for a pattern
--    (for the groups, with drawer 0 for \K; and for the marks).
-- GHCi で試す用：ST の VM が、パターンのために作る引き出しの数
-- （グループのぶん。0番は \K のため。それと、印のぶん）
drawers :: Text -> (Int, Int)
drawers p = slotsOf (program (surely p))
-- ANCHOR_END: twovms

-- ANCHOR: memo
-- 🗒️ A line of the old records, with notes in the notes.
-- 古い記録の1行。注記の中に、また注記がある
memo :: Text
memo = "Mira (healer (2nd class) from Lam (east)) paid 120 gold (cash)"

-- 🔍 A note with no ( or ) inside: only the innermost ones.
-- 中に ( も ) もない注記：いちばん内側のものだけ
innerNotes :: Regex
innerNotes = surely "\\([^()]*\\)"

-- 🪆 A note: (, then letters that are not ( or ), or a note again
--    (the whole pattern, called from inside itself), and ).
-- 注記：(、そのあと ( でも ) でもない文字か、また注記（パターン全体を、
-- 自分の中から呼ぶ）、そして )
wholeNotes :: Regex
wholeNotes = surely "\\((?:[^()]++|(?R))*\\)"
-- ANCHOR_END: memo

-- ANCHOR: subroutine
-- 🏷️ A name and the note right after it. The note is group 2, and it
--    calls group 2 (not the whole pattern) for a note inside.
-- 名前と、そのすぐうしろの注記。注記は2番のグループで、中の注記には
-- （パターン全体ではなく）2番のグループを呼ぶ
namesWithNotes :: Regex
namesWithNotes = surely "(\\w+) (\\((?:[^()]++|(?2))*\\))"
-- ANCHOR_END: subroutine

-- ANCHOR: palindrome
-- 🪞 Reads the same both ways: a character, the same inside, the same
--    character again; or, in the middle, one character or none.
-- 前からも後ろからも同じ：1文字、中もまた同じ形、同じ文字をもう一度。
-- まんなかは、1文字か、何もなし
palindrome :: Regex
palindrome = surely "^((.)(?1)\\2|.?)$"
-- ANCHOR_END: palindrome

-- ANCHOR: define
-- 📚 From one day to another: the parts get names in (?(DEFINE) ...),
--    and the pattern is put together from them. (?&date) runs the
--    group date, but does not set it: from and to keep the dates.
-- ある日から、ある日まで：部品に (?(DEFINE) ...) で名前を付け、それを
-- 組み立ててパターンにする。(?&date) は date のグループを動かすが、
-- 書き換えはしない。日付を持つのは from と to
period :: Regex
period = surely """
  (?x)
  (?(DEFINE)
    (?<year>  \\d{4} )
    (?<month> 0[1-9] | 1[0-2] )
    (?<day>   0[1-9] | [12]\\d | 3[01] )
    (?<date>  (?&year) - (?&month) - (?&day) )
  )
  (?<from> (?&date) ) \\.\\. (?<to> (?&date) )
  """

-- 🗓️ The two ends of a period, if there is one.
-- 期間の両端（あれば）
periodOf :: Text -> Maybe (Text, Text)
periodOf t = do
  m <- find period t
  (,) <$> named "from" m <*> named "to" m
-- ANCHOR_END: define

-- ANCHOR: slowly
-- 🐢 The same regex, run by the backtracking VM, even if the Pike VM
--    could run it (to compare the two).
-- 同じ正規表現を、Pike VM で動かせても、バックトラックの VM で動かす
-- （2つをくらべるため）
backtracking :: Regex -> Regex
backtracking re = re { engine = Backtracking }

-- 👣 For GHCi: the steps of a search with the engine you name (with no
--    limit), trying every place. The Pike VM cannot run every pattern.
-- GHCi で試す用：名前を言ったエンジンで探したときのステップ数（上限なしで、
-- 全部の場所を試して）。Pike VM は、どのパターンでも動かせるわけでは
-- ない
stepsWith :: Engine -> Text -> Text -> Int
stepsWith e p t
  | e == PikeVM, Just i <- blocker (program re) =
      error ("the Pike VM cannot run " ++ show (() <$ i))
  | otherwise =
      snd (searchCountingBy (runnerFor re { engine = e }) maxBound re t 0)
  where
    -- every place, as in the chapter before (the VM's own steps)
    -- 前の章と同じく、全部の場所で（VM だけのステップ数）
    re = everywhere (surely p)

-- 🪪 A name field with n names, and the ! by mistake.
-- 名前が n 個と、まちがえて入った ! の名前の欄
fieldOf :: Int -> Text
fieldOf n = T.unwords (take n (cycle names)) <> "!"
  where
    names = ["Mira", "Ruby", "Lam", "GHC", "Perl"]

-- 🐢 The check of the chapter before: the backtracking VM, with the
--    window's limit.
-- 前の章の確かめ：バックトラックの VM で、窓口の上限で
checkNameSlowly :: Text -> Either GaveUp Bool
checkNameSlowly t =
  isJust <$> findWithin windowLimit (backtracking wordsOnly) t
-- ANCHOR_END: slowly

-- ANCHOR: everywhere
-- 🐢 The same regex, with no prefilter: every place is tried, as in
--    the chapters before.
-- 同じ正規表現を、先に見ずに：前の章までと同じく、全部の場所を試す
everywhere :: Regex -> Regex
everywhere re = re { prefilter = noPrefilter }

-- 🧮 The same regex, looking for its prefix with KMP.
-- 同じ正規表現で、先頭の文字列を KMP で探す
byKmp :: Regex -> Regex
byKmp re = re { prefilter = (prefilter re) { finder = ByKmp } }

-- 🔍 For GHCi: what is known before the VM runs, as pcre2test's /I
--    shows it.
-- GHCi で試す用：VM を動かす前にわかっていること。pcre2test の /I の
-- ように見せる
seePrefilter :: Text -> IO ()
seePrefilter p = do
  let (s, n) = describePrefilter p
  putStrLn ("start:  " ++ s)
  putStrLn ("needed: " ++ n)

-- 📝 The same, as two lines of text.
-- 同じものを、2行の文字で
describePrefilter :: Text -> (String, String)
describePrefilter p = (showStart (start pf), show (needed pf))
  where
    pf = prefilter (surely p)
    showStart s = case s of
      Anywhere     -> "anywhere"
      OnlyFirst    -> "only the first place"
      Prefix t     -> "prefix " ++ show t
      FirstChar as -> "one of " ++ unwords (map atomText as)
    -- the atom, as `take` shows it in a listing
    -- アトムを、listing の take と同じ書き方で
    atomText a = T.unpack (T.drop 5 (showInst (Take a)))

-- 🗺️ For GHCi: the places the VM is handed, from 0.
-- GHCi で試す用：VM に渡す場所。0 から
triedPlaces :: Text -> Text -> [Int]
triedPlaces p t = case cursorAt t 0 of
  Nothing -> []
  Just c  -> map at (places (prefilter (surely p)) c)

-- ANCHOR: archive
-- 📚 The archive: n quiet lines of the shop's book, and then the page
--    of the old records at the end.
-- 記録の山：店のログの静かな行が n 行、最後に古い記録の1ページ
archive :: Int -> Text
archive n = T.replicate n quiet <> records
  where
    quiet = "Mira sold 3 herbs to Lam, and Ruby paid 12 gold\n"
-- ANCHOR_END: archive
-- ANCHOR_END: everywhere

-- ANCHOR: users
-- 🔎 Patterns that guild members typed into the search box of the
--    records room.
-- ギルドの人たちが、記録室の検索の欄に打ちこんだパターン
userPatterns :: [Text]
userPatterns =
  [ "Mira|Ruby"
  , "(\\w+\\s?)+!"
  , "(\\w+) \\1"
  , "\\d{4}-(?=\\d)"
  ]

-- 🛡️ A pattern from someone else: only if it runs in linear time.
-- ほかの人のパターン：線形時間で動くものだけ
safeCompile :: Text -> Either RegexError Regex
safeCompile = compileWith [Linear]
-- ANCHOR_END: users
-- ANCHOR_END: all
