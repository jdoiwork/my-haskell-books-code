{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- 🧪 The release of the workshop, in the fake world: the files, the
--    commands of git and their answers are all made up, so the same
--    script gives the same answer every time, and nothing real moves.
-- 工房のリリースを、偽物の世界で：ファイルも git のコマンドとその答えも
-- 作りもの。だから同じスクリプトはいつも同じ答えを出し、本物は何も動かない
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Hex
import qualified Crypto.Hash.SHA256 as Sha
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import Data.Text.Encoding (decodeUtf8)
import Test.Hspec
import Test.QuickCheck

import Lam.Eval (ShellCommand (..))
import Lam.Module (Files (..), loadProgram, runProgram)
import Lam.Prelude (preludeSession)
import Lam.Program (Ending (..))
import Lam.World
-- ANCHOR_END: head

-- ANCHOR: world
-- 🗂️ The workshop just before the release: lam.toml, CHANGELOG.md, and
--    the archive that git archive would make (the fake git makes
--    nothing, so it is there already).
-- リリースの直前の工房：lam.toml・CHANGELOG.md と、git archive が作るはずの
-- ファイル（偽物の git は何も作らないので、はじめから置いておく）
workshop :: [Text] -> FakeWorld
workshop subjects = emptyWorld
  { worldFiles = textFiles
      [ ("lam.toml", "[package]\nname = \"lam\"\nversion = \"0.25.0\"\n")
      , ("CHANGELOG.md", oldNotes) ]
      <> Map.fromList [ (tarOf v, tarBytes) | v <- versions ]
  , worldCommands = Map.fromList
      [ (["git", "describe", "--tags", "--abbrev=0"], (0, "v0.25.0\n", ""))
      , (["git", "log", "--reverse", "--format=%s", "v0.25.0..HEAD"],
         (0, T.unlines subjects, ""))
      , (["git", "log", "-1", "--format=%cs"], (0, "2026-11-29\n", "")) ]
      <> Map.fromList (concatMap answered versions)
  }
  where
    -- The versions a release can make: a patch, a minor, a major.
    -- リリースが作りうる版：patch・minor・major
    versions = ["0.25.1", "0.26.0", "1.0.0"]
    answered v =
      [ (["git", "commit", "-q", "-am", "release " <> v], (0, "", ""))
      , (["git", "tag", "v" <> v], (0, "", ""))
      , ([ "git", "archive", "--format=tar", "--prefix=lam-" <> v <> "/"
         , "-o", tarOf v, "HEAD" ], (0, "", "")) ]

oldNotes :: Text
oldNotes =
  "# Changelog\n\n## 0.25.0 (2026-11-23)\n\n- **oneliner**: one line\n"

tarOf :: Text -> Text
tarOf v = "dist/lam-" <> v <> ".tar"

tarBytes :: B.ByteString
tarBytes = "the bytes of the archive"
-- ANCHOR_END: world

-- ANCHOR: run
-- 🎭 Run release.lam (it imports Version.lam) in a fake world, with
--    these arguments and these lines typed in.
-- release.lam（Version.lam を import する）を、偽物の世界で動かす
release
  :: Text -> Text -> [Text] -> [Text] -> FakeWorld -> (Ending, FakeWorld)
release script version args input world =
  runFake world { worldArgs = args, worldInput = input } $
    case runIdentity (loadProgram files preludeSession preludeSession
                        "release.lam" script) of
      Left err              -> pure (Errored err)
      Right (loaded, final) -> runProgram fakeWorld loaded final
  where
    files = Files (\f -> pure (if f == "Version.lam" then Just version
                               else Nothing)) []

-- 🌅 The next day: the last tag is v<version>, and these commits came
--    after it.
-- 次の日：最後のタグは v<版> で、そのあとにこのコミットが来た
nextDay :: Text -> [Text] -> FakeWorld -> FakeWorld
nextDay v subjects w = w
  { worldCommands =
      Map.insert ["git", "describe", "--tags", "--abbrev=0"]
        (0, "v" <> v <> "\n", "")
      (Map.insert ["git", "log", "--reverse", "--format=%s", since]
        (0, T.unlines subjects, "") (worldCommands w)) }
  where
    since = "v" <> v <> "..HEAD"

-- 📄 A file of the world after, as text.
-- あとの世界のファイルを、文字で
fileOf :: Text -> FakeWorld -> Maybe Text
fileOf name w = decodeUtf8 <$> Map.lookup name (worldFiles w)

-- 🐚 The commands that ran, in order: name and arguments.
-- 動いたコマンドを順に：名前と引数
ran :: FakeWorld -> [[Text]]
ran w = reverse [ commandName c : commandArgs c | c <- worldRan w ]
-- ANCHOR_END: run

-- ANCHOR: oracle
-- 🔮 What the version should become, worked out again in Haskell, in
--    another way: Nothing if no subject is a feat or a fix.
-- 版がどうなるべきかを、Haskell で別のやり方でもう一度：feat も fix も
-- なければ Nothing
expected :: [Text] -> Maybe Text
expected subjects =
  case maximum (0 : map levelOf subjects) of
    3 -> Just "1.0.0"
    2 -> Just "0.26.0"
    1 -> Just "0.25.1"
    _ -> Nothing
  where
    levelOf s = case T.breakOn ": " s of
      (front, rest) | T.length rest > 2 -> kind front
      _ -> 0 :: Int
    kind front
      | Just f <- T.stripSuffix "!" front, base f > 0 = 3
      | otherwise = base front
    base :: Text -> Int
    base f = case T.breakOn "(" f of
      ("feat", scope) | scoped scope -> 2
      ("fix", scope)  | scoped scope -> 1
      _ -> 0
    scoped s = T.null s || ("(" `T.isPrefixOf` s && T.count ")" s == 1
                            && ")" `T.isSuffixOf` s)

-- 🎲 Subjects of commits: some go in the notes, some look like they
--    do but do not.
-- コミットの題：記録に入るものと、入りそうで入らないもの
genSubject :: Gen Text
genSubject = elements
  [ "feat: a", "fix: b", "feat(shell): c", "fix(表): d", "feat!: e"
  , "fix(io)!: f", "docs: g", "chore: h", "feature: i", "Fix: j"
  , "feat:k", "fixup! l", "feat(x: m", "fix(): n" ]
-- ANCHOR_END: oracle

main :: IO ()
main = do
  script  <- Utf8.readFile "release/release.lam"
  version <- Utf8.readFile "release/Version.lam"
  let subjects = [ "feat(shell): pass a value as one word"
                 , "fix(表): count a kanji as two columns"
                 , "docs: explain --print-script" ]
      run = release script version
  hspec $ do
    -- ANCHOR: examples
    describe "the release" $ do
      -- 「--yes なら、記録と lam.toml を書き、コミットしてタグを打つ」
      it "writes the notes and the version, then commits and tags" $ do
        let (ending, later) = run ["--yes"] [] (workshop subjects)
        ending `shouldBe` Finished ""
        fileOf "lam.toml" later `shouldBe`
          Just "[package]\nname = \"lam\"\nversion = \"0.26.0\"\n"
        fmap (T.lines) (fileOf "CHANGELOG.md" later) `shouldBe` Just
          [ "# Changelog", "", "## 0.26.0 (2026-11-29)", "", "### Features"
          , "", "- **shell**: pass a value as one word", "", "### Fixes", ""
          , "- **表**: count a kanji as two columns", ""
          , "## 0.25.0 (2026-11-23)", "", "- **oneliner**: one line" ]
        map (take 2) (ran later) `shouldBe`
          [ ["git", "describe"], ["git", "log"], ["git", "log"]
          , ["git", "commit"], ["git", "tag"], ["git", "archive"] ]
      -- 「SHA256SUMS は、固めたファイルの本物の SHA-256」
      it "writes the real SHA-256 of the archive" $ do
        let (_, later) = run ["--yes"] [] (workshop subjects)
            hex = decodeUtf8 (Hex.encode (Sha.hash tarBytes))
        fileOf "dist/SHA256SUMS" later `shouldBe`
          Just (hex <> "  lam-0.26.0.tar\n")
      -- 「n と答えたら、何も書かず、git は読むだけ」
      it "changes nothing when the answer is no" $ do
        let world = workshop subjects
            (ending, later) = run [] ["n"] world
        ending `shouldBe` Finished ""
        worldFiles later `shouldBe` worldFiles world
        map (take 2) (ran later) `shouldBe`
          [["git", "describe"], ["git", "log"], ["git", "log"]]
      -- 「書きかけのファイル（.new）は残らない」
      it "leaves no half-written file" $ do
        let (_, later) = run ["--yes"] [] (workshop subjects)
        filter (".new" `T.isSuffixOf`) (Map.keys (worldFiles later))
          `shouldBe` []
    -- ANCHOR_END: examples

    -- ANCHOR: draft
    describe "the draft without (?m)" $
      -- 「下書きの setVersion は lam.toml を直さず、次のリリースが止まる」
      it "leaves lam.toml behind, and the next release stops" $ do
        let draft = T.replace "re\"(?m)^version" "re\"^version" script
            (first, later) =
              release draft version ["--yes"] [] (workshop subjects)
            -- The next day: the tag is v0.26.0, and a new fix.
            -- 次の日：タグは v0.26.0 で、新しい fix が1つ
            (second, _) = release draft version ["--yes"] []
                            (nextDay "0.26.0" ["fix: a"] later)
        first `shouldBe` Finished ""
        fileOf "lam.toml" later `shouldBe`
          Just "[package]\nname = \"lam\"\nversion = \"0.25.0\"\n"
        second `shouldBe`
          Errored "error: lam.toml says 0.25.0, but the last tag is v0.26.0"
    -- ANCHOR_END: draft

    -- ANCHOR: props
    describe "releases of any commits" $ do
      -- 「lam.toml の版は、Haskell で数え直した版と同じ」
      prop "raise the version as the commits say" $
        forAll (listOf genSubject) $ \subs ->
          let (ending, later) = run ["--yes"] [] (workshop subs)
              toml = fileOf "lam.toml" later
          in case expected subs of
               Just v  -> ending === Finished ""
                            .&&. toml === Just ("[package]\nname = \"lam\"\n\
                                                \version = \"" <> v <> "\"\n")
               Nothing -> ending === Errored "error: nothing to release \
                                             \(no feat: or fix: since then)"
                            .&&. toml === fileOf "lam.toml" (workshop subs)
      -- 「前の版の記録は、新しい記録の下に、そのまま残る」
      prop "keep the old notes below the new ones" $
        forAll (listOf genSubject) $ \subs ->
          let (_, later) = run ["--yes"] [] (workshop subs)
          in fmap (oldNotesKept . T.lines) (fileOf "CHANGELOG.md" later)
               === Just True
    -- ANCHOR_END: props

    -- ANCHOR: answer
    describe "the answer of step 4" $
      -- 「リリースのすぐあとにもう一度打っても、何も変えずに止まる」
      prop "stop the release right after a release" $
        forAll (listOf genSubject) $ \subs ->
          let (first, later) = run ["--yes"] [] (workshop subs)
              next = fromMaybe "0.25.0" (expected subs)
              (second, twice) = run ["--yes"] [] (nextDay next [] later)
          in first == Finished "" ==>
               second === Errored "error: nothing to release \
                                  \(no feat: or fix: since then)"
               .&&. worldFiles twice === worldFiles later
    -- ANCHOR_END: answer
  where
    prop name = it name . property
    oldNotesKept ls =
      drop (length ls - length (T.lines oldNotes) + 1) ls
        == drop 1 (T.lines oldNotes)
-- ANCHOR_END: all
