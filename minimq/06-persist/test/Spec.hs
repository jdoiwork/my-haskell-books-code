module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (isPrefixOf, sort)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Test.Hspec
import Test.QuickCheck hiding (replay)

import MiniMq.Broker
import MiniMq.Client
import MiniMq.Examples
import MiniMq.Journal
import MiniMq.Protocol
import MiniMq.Server
import MiniMq.Types
import MqAnswers

-- 🎲 A day at the post office: send, take, answer, leave, in any order.
--    (The same model as in the acknowledgements chapter.)
-- 伝令所の1日：送る、取る、返事をする、切る。どんな順でも。
-- （受け取りの確認の章と同じモデル）
data Event = Send | Take | Answer Settlement | Leave
  deriving Show

instance Arbitrary Event where
  arbitrary = frequency
    [ (3, pure Send), (3, pure Take)
    , (3, Answer <$> elements [minBound ..]), (1, pure Leave) ]

-- 📒 Play the events; keep the broker and every note it wrote.
-- 出来事を順に起こす。ブローカーと、書いた覚え書きの全部をとっておく
day :: [Event] -> (Broker, [Entry])
day events = go events [] start []
  where
    (who, start) = surely (subscribe "jobs" (surely cursedPost))
    go [] _ broker written =
      let (fresh, broker') = takeNotes broker in (broker', written ++ fresh)
    go (e : es) held broker written = case e of
      Send -> go es held (snd (surely (publish "quests" job broker))) written
      Take -> case surely (takeNext who 2 "jobs" broker) of
        (Just d, b) -> go es (tag d : held) b written
        (Nothing, b) -> go es held b written
      Answer how -> case held of
        t : rest -> go es rest (surely (settle [who] how t broker)) written
        []       -> go es held broker written
      Leave -> go es [] (abandon who broker) written
    job = Message "job" "Slay the slime"

-- ✉️ The letters, in a form easy to compare.
-- 比べやすい形にした手紙
contents :: Broker -> [(QueueName, Message)]
contents = sort . letters

main :: IO ()
main = hspec $ do
  -- ANCHOR: spec-journal
  describe "the journal" $ do
    -- 日誌を当て直すと、まだ終わっていない手紙が全部そろう
    it "replays into the same letters" $
      forAll (listOf arbitrary) $ \events ->
        let (broker, written) = day events
        in  contents (replay written) === contents broker
    -- 短く書き直した日誌からも、同じ手紙がそろう
    it "compacts into the same letters" $
      forAll (listOf arbitrary) $ \events ->
        let (broker, _) = day events
        in  contents (replay (compact broker)) === contents broker
    -- どこで切れた日誌も、読めるところまでは、正しく読める
    it "reads a journal cut anywhere as a prefix of it" $
      forAll (listOf arbitrary) $ \events ->
        let bytes = encodeEntries (snd (day events))
        in  forAll (choose (0, BS.length bytes)) $ \cut ->
              decodeEntries (BS.take cut bytes)
                `isPrefixOf` decodeEntries bytes
  -- ANCHOR_END: spec-journal

  -- ANCHOR: spec-server
  describe "the server with a journal" $ do
    -- ROUTED と言う前に、手紙はもう日誌にある
    it "writes a letter down before it says ROUTED" $
      inTempDir $ \path -> withServerAt (Just path) $ \port -> do
        setUp port
        -- GHC locks a file we are writing, even against ourselves,
        -- so read it from another process.
        -- 書いている最中のファイルは、GHC が自分自身にも錠をかけるので、
        -- 別のプロセスから読む
        written <- decodeEntries . BC.pack <$> readProcess "cat" [path] ""
        [m | Stored "jobs" _ m <- written]
          `shouldBe` [Message "job" "Find a herb"]
    -- 電源を抜いても、終わっていない手紙は戻ってくる（again の印つきで）
    it "brings back the unfinished letters after the plug is pulled" $
      inTempDir $ \path -> do
        withServerAt (Just path) $ \port -> do
          setUp port
          withClient port $ \c -> do
            mapM_ (request c . Publish "quests" . Message "job")
                  ["Slay a slime", "Open the box"]
            _ <- request c (Consume "jobs" 1)
            -- "Find a herb" is done; "Slay a slime" is held when the
            -- plug is pulled.
            -- Find a herb は終わり、Slay a slime は持ったまま電源が抜ける
            nextDelivery c >>= answerWith Ack c
            _ <- nextDelivery c
            pure ()
        withServerAt (Just path) $ \port ->
          withClient port $ \c -> do
            _ <- request c (Consume "jobs" 2)
            a <- nextDelivery c
            b <- nextDelivery c
            map (\d -> (body (message d), redelivered d)) [a, b]
              `shouldBe` [("Slay a slime", True), ("Open the box", True)]
    -- 残さないキューの手紙は、電源を抜くと消える
    it "forgets a transient queue" $
      inTempDir $ \path -> do
        withServerAt (Just path) $ \port -> withClient port $ \c -> do
          _ <- request c (DeclareQueue "scratch" plainQueue
                                       { durability = Transient })
          pure ()
        withServerAt (Just path) $ \port -> withClient port $ \c ->
          request c (Consume "scratch" 1)
            `shouldReturn` Refused "no such queue: scratch"
  -- ANCHOR_END: spec-server

  -- 「トレーニング」
  describe "answers" $ do
    it "wasted counts the lines compacting removes" $ do
      let (_, written) = day (replicate 3 Send ++ replicate 3 Take
                              ++ replicate 3 (Answer Ack))
      wasted written
        `shouldBe` length written - length (compact (replay written))
      (wasted written > 0) `shouldBe` True
    it "worthCompacting leaves a small journal alone" $
      worthCompacting 0.5 [] `shouldBe` False
  where
    inTempDir act = withSystemTempDirectory "minimq" $ \dir ->
      act (dir </> "minimq.journal")
    setUp port = withClient port $ \c -> do
      _ <- request c (DeclareExchange "quests" Direct)
      _ <- request c (DeclareQueue "jobs" plainQueue)
      _ <- request c (Bind "quests" "job" "jobs")
      _ <- request c (Publish "quests" (Message "job" "Find a herb"))
      pure ()
