module Main (main) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Exception (IOException, try)
import Control.Monad (forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Either (isLeft)
import Data.List (isPrefixOf, sort)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import System.Timeout (timeout)
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

-- ANCHOR: model
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

-- 📒 Play the events. Every step is one transaction, like `step` in the
--    server: keep the broker after it, and the notes it wrote. The
--    first step makes the post office.
-- 出来事を順に起こす。1つの出来事が、サーバーの step と同じく1つの
-- 取引。そのあとのブローカーと、書いた覚え書きをとっておく。
-- 最初の一歩は、伝令所を作るところ
day :: [Event] -> [(Broker, [Entry])]
day events = (start, made) : go events [] start
  where
    (who, opened) = surely (subscribe "jobs" (surely cursedPost))
    (made, start) = takeNotes opened
    go [] _ _ = []
    go (e : es) held broker =
      let (held', changed) = act e held broker
          (written, clean) = takeNotes changed
      in  (clean, written) : go es held' clean
    act e held broker = case e of
      Send -> (held, snd (surely (publish "quests" job broker)))
      Take -> case surely (takeNext who 2 "jobs" broker) of
        (Just d, b)  -> (tag d : held, b)
        (Nothing, b) -> (held, b)
      Answer how -> case held of
        t : rest -> (rest, surely (settle [who] how t broker))
        []       -> (held, broker)
      Leave -> ([], abandon who broker)
    job = Message "job" "Slay the slime"

-- 📜 The journal of a day: one line for each step that wrote something
--    (`handIn` hands in nothing for the others), with the broker after.
-- 1日の日誌：何かを書いた一歩ごとに1行（ほかの一歩では、handIn は
-- 何も渡さない）。そのあとのブローカーつき
journalOf :: [Event] -> [(Broker, [Entry])]
journalOf events = [ s | s@(_, written) <- day events, not (null written) ]

-- ✉️ The letters, in a form easy to compare.
-- 比べやすい形にした手紙
contents :: Broker -> [(QueueName, Message)]
contents = sort . letters
-- ANCHOR_END: model

main :: IO ()
main = hspec $ do
  -- ANCHOR: spec-journal
  describe "the journal" $ do
    -- 日誌を当て直すと、まだ終わっていない手紙が全部そろう
    it "replays into the same letters" $
      forAll (listOf arbitrary) $ \events ->
        let steps = day events
        in  contents (replay (concatMap snd steps))
              === contents (fst (last steps))
    -- 短く書き直した日誌からも、同じ手紙がそろう
    it "compacts into the same letters" $
      forAll (listOf arbitrary) $ \events ->
        let broker = fst (last (day events))
        in  contents (replay (compact broker)) === contents broker
    -- どこで切れた日誌も、読めるのは先頭の何行か。当て直すと、
    -- その行を書いた時点の伝令所の手紙がそろう
    it "reads a journal cut anywhere as the post office at some step" $
      forAll (listOf arbitrary) $ \events ->
        let steps  = journalOf events
            bytes  = encodeLines (map snd steps)
            states = [] : map (contents . fst) steps
        in  forAll (choose (0, BS.length bytes)) $ \cut ->
              let got = decodeLines (BS.take cut bytes)
              in  (got `isPrefixOf` map snd steps)
                    .&&. contents (replay (concat got))
                           === states !! length got
    -- 配れなかった手紙の部屋へ移る途中で、どこで切れても、
    -- 手紙はなくならない（jobs か dead-letters の、どちらかにある）
    it "never loses a letter on its way to the dead letters" $ do
      let (made, empty)  = takeNotes (surely guildPost)
          (sent, posted) = takeNotes (sendBox empty)
          (moved, _)     = takeNotes (requeueThrice posted)
          bytes          = encodeLines [made, sent, moved]
          answered       = BS.length (encodeLines [made, sent])
          placesAt cut   = map fst (letters (replay (concat
                             (decodeLines (BS.take cut bytes)))))
      moved `shouldBe` [ Finished (LetterId 1)
                       , Stored "dead-letters" (LetterId 2) box ]
      map placesAt [answered .. BS.length bytes]
        `shouldSatisfy` all (`elem` [["jobs"], ["dead-letters"]])
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
        raw <- readProcess "cat" [path] ""
        let written = concat (decodeLines (BC.pack raw))
        [m | Stored "jobs" _ m <- written]
          `shouldBe` [Message "job" "Find a herb"]
    -- 再起動しても、終わっていない手紙は戻ってくる（again の印つきで）
    it "brings back the unfinished letters after a restart" $
      inTempDir $ \path -> do
        withServerAt (Just path) $ \port -> do
          setUp port
          withClient port $ \c -> do
            mapM_ (request c . Publish "quests" . Message "job")
                  ["Slay a slime", "Open the box"]
            _ <- request c (Consume "jobs" 1)
            -- "Find a herb" is done; "Slay a slime" is held when the
            -- server stops.
            -- Find a herb は終わり、Slay a slime は持ったままサーバーが止まる
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
    -- 残さないキューは、再起動すると消える
    it "forgets a transient queue" $
      inTempDir $ \path -> do
        withServerAt (Just path) $ \port -> withClient port $ \c -> do
          _ <- request c (DeclareQueue "scratch" plainQueue
                                       { durability = Transient })
          pure ()
        withServerAt (Just path) $ \port -> withClient port $ \c ->
          request c (Consume "scratch" 1)
            `shouldReturn` Refused "no such queue: scratch"
    -- サーバーが止まったら、つながっている客にも電話を切る
    it "hangs up on its clients when it stops" $ do
      started <- newEmptyMVar
      let server = withServer $ \port -> do
            putMVar started port
            forever (threadDelay 1000000)
      withAsync server $ \running -> do
        port <- takeMVar started
        withClient port $ \c -> do
          request c (DeclareQueue "jobs" plainQueue) `shouldReturn` Ok
          cancel running
          -- Not left waiting for an answer that never comes: told at once.
          -- 来ない答えを待たされず、すぐに切れたとわかる
          late <- timeout 5000000 $
            try @IOException (request c (DeclareQueue "late" plainQueue))
          fmap isLeft late `shouldBe` Just True
  -- ANCHOR_END: spec-server

  -- 「トレーニング」
  describe "answers" $ do
    it "wasted counts the lines compacting removes" $ do
      let written = concatMap snd (day (replicate 3 Send ++ replicate 3 Take
                                        ++ replicate 3 (Answer Ack)))
      wasted written
        `shouldBe` length written - length (compact (replay written))
      (wasted written > 0) `shouldBe` True
    it "worthCompacting leaves a small journal alone" $
      worthCompacting 0.5 [] `shouldBe` False
  -- ANCHOR: helpers
  where
    -- 🧟 The cursed letter: sent once, then requeued three times.
    -- 呪いの手紙：1回送って、3回 Requeue される
    box           = Message "job" "Open the cursed box"
    sendBox       = snd . surely . publish "quests" box
    requeueThrice = snd . surely . answerEach (replicate 3 Requeue)
    -- 📁 A journal file in a fresh directory, gone after the test.
    -- 新しいディレクトリーの日誌のファイル。テストのあとで消える
    inTempDir act = withSystemTempDirectory "minimq" $ \dir ->
      act (dir </> "minimq.journal")
    -- 🏗️ A desk, a mailbox, a binding, and one letter.
    -- 仕分け台、郵便受け、結びつけ、そして手紙を1通
    setUp port = withClient port $ \c -> do
      _ <- request c (DeclareExchange "quests" Direct)
      _ <- request c (DeclareQueue "jobs" plainQueue)
      _ <- request c (Bind "quests" "job" "jobs")
      _ <- request c (Publish "quests" (Message "job" "Find a herb"))
      pure ()
  -- ANCHOR_END: helpers
