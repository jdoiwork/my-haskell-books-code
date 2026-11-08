-- ANCHOR: all
-- | The dragon team at work: a consumer that is a child of a supervisor.
-- 働くドラゴン班：監督役の子になった受け手
-- ANCHOR: header
module MiniMq.Crew
  ( dragonCrew
  ) where

import Network.Socket (PortNumber)

import MiniMq.Client
import MiniMq.Guild
import MiniMq.Supervisor
import MiniMq.Typed
import MiniMq.Wire (Conn)
-- ANCHOR_END: header

-- ANCHOR: crew
-- 🐉 Each run of the child opens its own connections, and closes them
--    when it falls: that is what gives the letter back. `inbox` only
--    consumes; to send, `handle` gets `outbox`, a second connection.
-- 子は、動くたびに自分のつながりを開き、倒れたら閉じる。閉じることが、
-- 手紙を戻す合図になる。inbox は受け取るだけ。送りたければ、handle は
-- もう1本のつながり outbox を使う
dragonCrew :: PortNumber -> (Conn -> Quest -> IO ()) -> IO ()
dragonCrew port handle = supervise Plan
  { strategy  = OneForOne
  , intensity = Intensity { restarts = 3, seconds = 10 }
  , children  = [Child "dragon-team" oneRun]
  }
  where
    oneRun =
      withClient port $ \inbox ->
        withClient port $ \outbox ->
          consume inbox dragonTeam (handle outbox)
-- ANCHOR_END: crew
-- ANCHOR_END: all
