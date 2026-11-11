-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 🧪 My own attributes and states: the gates, the GADT, and the search.
-- 自分で決める属性と状態のテスト：門、GADT、絞り込み
module CustomSpec (customSpec) where

import Control.Exception (try)
import Database.SQLite.Simple qualified as SQLite

import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day (..), fromGregorian)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Tsundoku.Card
import Tsundoku.Custom
import Tsundoku.Examples
import Tsundoku.Field
import Tsundoku.Form
import Tsundoku.Query
import Tsundoku.Shelf
import Tsundoku.Shelf.Rows (BrokenRow (..))
import Tsundoku.Shelf.Sqlite
import Tsundoku.Types

import ShelfAnswers.OrNot
import ShelfAnswers.Redefine
import ShelfAnswers.Url
-- ANCHOR_END: imports

-- ANCHOR: spec
customSpec :: Spec
customSpec = do
  -- 「値を読む」
  describe "values" $ do
    -- 「見せた文字を読み戻すと、もとの値」
    prop "reads back what it shows" $
      forAll kindAndValue $ \(kind, value) ->
        readValue kind (showValue value) === Just value
    -- 「数でないものは、数として読まない」
    it "reads only whole numbers as numbers" $
      map (readValue NumberKind) ["12", "-3", "3.5", "12冊", "", "すごく"]
        `shouldBe` [Just (NumberV 12), Just (NumberV (-3))
                   , Nothing, Nothing, Nothing, Nothing]
    -- 「Int に入らない数と、頭の + も、数として読まない」
    it "reads no number too big for Int, and no +" $
      map (readValue NumberKind)
          [ "9223372036854775807", "9223372036854775808"
          , "18446744073709551621", "+5" ]
        `shouldBe` [Just (NumberV maxBound), Nothing, Nothing, Nothing]

  -- 「定義の門」
  describe "definitions" $ do
    -- 「★と、使った名前は、使えない」
    it "refuses a name in use, and ★" $ do
      defineAttr (AttrDef "推し度" TextKind) librarySchema
        `shouldBe` Left (BadDefinition [NameTaken "推し度"])
      defineAttr (AttrDef "★" NumberKind) emptySchema
        `shouldBe` Left (BadDefinition [NameTaken "★"])
    -- 「選択肢は、少なくとも1つ。同じものは1回だけ。全部まとめて断る」
    it "reports every problem of a choice at once" $ do
      defineAttr (AttrDef "版" (ChoiceKind [])) emptySchema
        `shouldBe` Left (BadDefinition [NothingToPick])
      defineAttr (AttrDef "形" (ChoiceKind ["紙", "紙", "Web", "Web"]))
                 librarySchema
        `shouldBe` Left (BadDefinition
          [NameTaken "形", Twice "紙", Twice "Web"])
    -- 「名前は、絞り込みの単語に使えるもの。選択肢と状態は、空白で
    --   始まったり終わったりしない」
    it "refuses names the search cannot use" $ do
      defineAttr (AttrDef "" TextKind) emptySchema
        `shouldBe` Left (BadDefinition [BadName ""])
      defineAttr (AttrDef "買った 店" TextKind) emptySchema
        `shouldBe` Left (BadDefinition [BadName "買った 店"])
      defineAttr (AttrDef "色" (ChoiceKind ["赤", " 青"])) emptySchema
        `shouldBe` Left (BadDefinition [BadName " 青"])
      defineFlow (Flow "A=B" "上" [""] [("上", "")]) emptySchema
        `shouldBe` Left (BadDefinition [BadName "A=B", BadName ""])
    -- 「属性と流れで、同じ名前は使えない（どちら向きでも）」
    it "keeps the names of attributes and flows apart" $ do
      defineAttr (AttrDef "連載の状態" TextKind) librarySchema
        `shouldBe` Left (BadDefinition [NameTaken "連載の状態"])
      defineFlow serialFlow { flowName = "推し度" } librarySchema
        `shouldBe` Left (BadDefinition [NameTaken "推し度"])
    -- 「矢印の元がない流れと、同じ状態が2つある流れは、断る」
    it "refuses an arrow from nowhere, and a state twice" $ do
      defineFlow (Flow "棚" "上" ["下"] [("上", "下"), ("横", "下")])
                 emptySchema
        `shouldBe` Left (BadDefinition [NoSuchState "横"])
      defineFlow (Flow "棚" "上" ["下", "下"] [("上", "下")]) emptySchema
        `shouldBe` Left (BadDefinition [Twice "下"])
    -- 「どの状態にも行けるときだけ、流れを受け取る」
    prop "accepts a flow exactly when every state can be reached" $
      forAll flowGen $ \flow ->
        let alone = [ s | s <- flowStates flow
                        , s `Set.notMember` reachableNaive flow ]
        in  defineFlow flow emptySchema === case alone of
              [] -> Right emptySchema { flows = [flow] }
              _  -> Left (BadDefinition (map Unreachable alone))

  -- 「フォームの門」
  describe "the gate for a form" $ do
    -- 「欄の値は、前後の空白を取ってから読む」
    it "reads a field after taking off the spaces" $
      readFields librarySchema (Map.fromList [("推し度", " 4 ")])
        `shouldBe` Right (Map.fromList [("推し度", Just (NumberV 4))])
    -- 「まずい欄を、1つも取りこぼさずに知らせる」
    prop "reports every bad field, not just the first" $
      forAll formGen $ \fields ->
        let bad = length [ () | (_, _, False) <- fields ]
            typed = Map.fromList [ (n, v) | (n, v, _) <- fields ]
        in  case readFields librarySchema typed of
              Right _               -> bad === 0
              Left (BadAttrs found) -> length found === bad .&&. bad > 0
              Left other            -> counterexample (show other) False

  -- 「GADT で取り出す」
  describe "fields with types" $ do
    -- 「属性の型で取り出せる」
    it "gives values as the field's type" $ do
      get oshi dragonFilled `shouldBe` Just 4
      get boughtOn dragonFilled `shouldBe` Just (fromGregorian 2026 10 30)
      get oshi librarianCard `shouldBe` Nothing
    -- 「型を言いまちがえたら、取り出せない」
    it "gives nothing for a field of the wrong kind" $
      get (Field "推し度" TextK) dragonFilled `shouldBe` Nothing

  -- 「絞り込みの言葉」
  describe "searching" $ do
    -- 「書き出した言葉を読むと、もとの形」
    prop "reads back a search it wrote down" $
      forAll queryGen $ \q -> parseQuery (written q) === Right q
    -- ANCHOR: parseTest
    -- 「空白がなくても読める」
    it "reads a search without spaces" $
      parseQuery "推し度>=3 and 形=Web"
        `shouldBe` Right (And (Compare "推し度" Ge "3")
                              (Compare "形" Eq "Web"))
    -- ANCHOR_END: parseTest
    -- 「確かめた絞り込みを、文字に書き戻す」
    it "writes a checked search back as text" $
      fmap showCond (parseQuery "★>=4 and 買った日 < 2026-11-01"
                       >>= checkQuery librarySchema)
        `shouldBe` Right "★ >= ★4 and 買った日 < 2026-11-01"
    -- 「状態と選択肢は、大小で比べられない。流れにない状態は読まない」
    it "refuses bigger or smaller on states and choices" $ do
      let refusal typed = either Just (const Nothing)
                            (parseQuery typed >>= checkQuery librarySchema)
      refusal "連載の状態 > 連載中"
        `shouldBe` Just (BadAttrs [Unordered "連載の状態"])
      refusal "形 > 紙" `shouldBe` Just (BadAttrs [Unordered "形"])
      refusal "連載の状態 = 完桔"
        `shouldBe` Just (BadAttrs [Unreadable "連載の状態" "完桔"])
    -- 「日付・文字・はい/いいえでも、カードを選ぶ」
    it "picks cards by dates, text, and yes or no" $ do
      let picked typed = map (title . work . snd)
                           <$> search librarySchema typed filledCards
          dragon = Right ["はじめてのドラゴン学"]
      picked "買った日 < 2026-11-01" `shouldBe` dragon
      picked "買った日 > 2026-11-01" `shouldBe` Right []
      picked "サイン本 = はい" `shouldBe` dragon
      picked "サイン本 = いいえ" `shouldBe` Right []
      picked "どこで買った = 古本市" `shouldBe` dragon
      picked "どこで買った = 駅前" `shouldBe` Right []
    -- 「空白のある値は、引用符でくくって書き戻す。書き戻した文字は読める」
    it "quotes a value with spaces, so it reads back" $ do
      let checked = parseQuery "どこで買った = \"神保町 の古本市\""
                      >>= checkQuery librarySchema
      fmap showCond checked
        `shouldBe` Right "どこで買った = \"神保町 の古本市\""
      fmap (fmap (const ()) . parseQuery . showCond) checked
        `shouldBe` Right (Right ())
    -- 「推し度の絞り込みは、GADT で取り出した推し度と同じカードを選ぶ」
    prop "picks the same cards as get oshi" $
      \(oshis :: [Maybe (Small Int)]) (Small n) ->
        forAll (elements [minBound .. maxBound]) $ \how ->
          let cards  = zip (map WorkId [1 ..]) (map withOshi oshis)
              typed  = "推し度 " <> symbol how <> " " <> T.pack (show n)
              wanted = [ c | c@(_, card) <- cards
                           , Just o <- [get oshi card], plain how o n ]
          in  search librarySchema typed cards === Right wanted

  -- 「定義から組み立てたフォーム」
  describe "the form" $
    -- 「定義の順に、属性と流れの行が並ぶ」
    it "has a row for each definition" $
      formLines (formFor librarySchema copyistFilled) `shouldBe`
        [ "推し度：[ 3 ]（数）"
        , "どこで買った：[  ]"
        , "買った日：[  ]（日付）"
        , "サイン本：[ ]"
        , "形：紙 電子 (Web)"
        , "連載の状態：休止  → 連載中 / 完結"
        , "貸し出し：（まだ入っていない）  → 本棚にある"
        ]

  -- 「トレーニングの解答」
  describe "answers (custom)" customAnswers
-- ANCHOR_END: spec

-- ANCHOR: answers
-- 🏋️ The answers of the training.
-- トレーニングの解答
customAnswers :: Spec
customAnswers = do
  -- 「URL は http:// か https:// で始まり、空白がない」
  it "reads URLs" $
    map readUrl [ "https://ncode.syosetu.com/n0000aa/", "http://x"
                , "https://", "ftp://x", "https://a b" ]
      `shouldBe` [ Just "https://ncode.syosetu.com/n0000aa/", Just "http://x"
                 , Nothing, Nothing, Nothing ]
  -- 「推し度を文字にしても値は引き継げるが、★の選択肢にすると合わない」
  it "finds the values a new definition would not fit" $ do
    let cards = zip (map WorkId [1 ..])
                    [dragonFilled, librarianFilled, copyistFilled]
        stars = ChoiceKind ["★1", "★2", "★3", "★4", "★5"]
    misfits (AttrDef "推し度" TextKind) cards `shouldBe` []
    misfits (AttrDef "推し度" stars) cards `shouldBe` map WorkId [1, 2, 3]
    carryOver TextKind (NumberV 4) `shouldBe` Just (TextV "4")
  -- 「表の定義だけを書き換えると、カードが読めなくなる」
  it "cannot read a card after the definition changed under it" $
    SQLite.withConnection ":memory:" $ \conn -> do
      shelf <- sqliteShelf conn
      _     <- shelf.defineAttr oshiDef
      wid   <- shelf.addWork dragonBook
      _     <- shelf.editAttrs wid (Map.fromList [("推し度", "4")])
      SQLite.execute_ conn
        "UPDATE definitions SET body = '{\"kind\": \"yesno\"}'"
      result <- try (shelf.getCard wid)
      case result of
        Left (BrokenRow what) -> what `shouldBe` "attrs: 推し度"
        Right _               -> expectationFailure "read a broken card"
  -- 「not は and より、and は or より強い」
  it "reads not, and, or in order of strength" $
    parseSearch "推し度 = 1 or 推し度 = 2 and not 推し度 = 3"
      `shouldBe` Right (AnyOf (Term (Compare "推し度" Eq "1"))
                              (AllOf (Term (Compare "推し度" Eq "2"))
                                     (Not (Term (Compare "推し度" Eq "3")))))
  -- 「or と not とカッコで絞り込む」
  it "picks cards with or, not, and parentheses" $ do
    let cards = zip (map WorkId [1 ..])
                    [dragonFilled, librarianFilled, copyistFilled]
        found typed = do
          s    <- parseSearch typed
          pick <- checkSearch librarySchema s
          pure [ title (work c) | (_, c) <- cards, picks pick c ]
    found "推し度 >= 5 or 連載の状態 = 休止"
      `shouldBe` Right ["図書室の司書に転生した件", "迷宮図書館の写本係"]
    found "not (形 = Web and 推し度 < 4)"
      `shouldBe` Right ["はじめてのドラゴン学", "図書室の司書に転生した件"]
    found "not 色 = 赤 or 推し度 > すごく"
      `shouldBe` Left (BadAttrs
        [UnknownName "色", Unreadable "推し度" "すごく"])
-- ANCHOR_END: answers

-- ANCHOR: helpers
-- 🎲 A kind, and a value of that kind.
-- 種類と、その種類の値
kindAndValue :: Gen (AttrKind, AttrValue)
kindAndValue = oneof
  [ (\t -> (TextKind,   TextV   t)) <$> nonBlank
  , (\n -> (NumberKind, NumberV n)) <$> arbitrary
  , (\d -> (DateKind,   DateV   d)) <$> day
  , (\b -> (YesNoKind,  YesNoV  b)) <$> arbitrary
  , (\c -> (formats,    ChoiceV c)) <$> elements ["紙", "電子", "Web"]
  ]
  where
    formats  = ChoiceKind ["紙", "電子", "Web"]
    day      = ModifiedJulianDay <$> choose (0, 99999)
    nonBlank = T.pack <$> listOf1 (elements "古本市 abc推")
                            `suchThat` any (/= ' ')

-- 🎲 A form: for some fields, a good value or a bad one (True = good).
-- フォーム：いくつかの欄に、よい値かまずい値（True がよい値）
formGen :: Gen [(Text, Text, Bool)]
formGen = catMaybes <$> mapM pick fields
  where
    pick (name, good, bad)
      | name == "色" = elements [ Nothing, Just (name, bad, False) ]
      | otherwise   = elements
          [ Nothing, Just (name, good, True), Just (name, bad, False) ]
    -- (name, a good value, a bad value). 色 is not defined at all.
    -- （名前、よい値、まずい値）。色は、定義していない
    fields =
      [ ("推し度",   "4",          "すごく")
      , ("買った日", "2026-11-01", "昨日")
      , ("サイン本", "いいえ",     "たぶん")
      , ("形",       "紙",         "巻物")
      , ("色",       "赤",         "赤")
      ]

-- 🎲 A flow of the states a to e, with arrows between them.
-- a〜e の状態の流れ。矢印は、その中だけ
flowGen :: Gen Flow
flowGen = do
  rest <- sublistOf ["b", "c", "d", "e"]
  let states' = "a" : rest
  arrows' <- listOf ((,) <$> elements states' <*> elements states')
  pure (Flow "流れ" "a" rest arrows')

-- 🧭 Reachable states, found another way: add the next states again and
--    again until nothing changes.
-- 行ける状態を、別のやり方で：変わらなくなるまで、次の状態を足し続ける
reachableNaive :: Flow -> Set.Set Text
reachableNaive flow = go (Set.singleton (start flow))
  where
    go seen =
      let more = Set.union seen (Set.fromList
                   [ to | (from, to) <- arrows flow, from `Set.member` seen ])
      in  if more == seen then seen else go more

-- 🎲 A search of 1 to 3 terms, joined by "and" from the left.
-- 項が1〜3個の絞り込み。"and" で左からつなぐ
queryGen :: Gen Query
queryGen = foldl1 And <$> resize 3 (listOf1 termGen)
  where
    termGen = Compare
      <$> elements ["推し度", "形", "★", "連載の状態", "どこで買った"]
      <*> elements [minBound .. maxBound]
      <*> elements ["4", "紙", "★4.5", "連載中", "2026-11-01", "駅前 の 本屋"]

-- ✍️ A search written down, with quotes around a value with spaces.
-- 絞り込みを書き出す。空白のある値は、引用符でくくる
written :: Query -> Text
written (And a b)          = written a <> " and " <> written b
written (Compare n how v)  = T.unwords [n, symbol how, quote v]
  where
    quote x | T.any (== ' ') x = "\"" <> x <> "\""
            | otherwise        = x

-- 🃏 A card with 推し度 (or none).
-- 推し度のある（またはない）カード
withOshi :: Maybe (Small Int) -> Card
withOshi Nothing          = newCard dragonBook
withOshi (Just (Small o)) = surely
  (editAttrs (Map.fromList [("推し度", T.pack (show o))]) librarySchema
             (newCard dragonBook))

-- ⚖️ The ways to compare, as Haskell's own operators.
-- 比べ方を、Haskell の演算子で
plain :: Op -> Int -> Int -> Bool
plain how = case how of
  Eq -> (==)
  Ne -> (/=)
  Lt -> (<)
  Le -> (<=)
  Gt -> (>)
  Ge -> (>=)
-- ANCHOR_END: helpers
-- ANCHOR_END: all
