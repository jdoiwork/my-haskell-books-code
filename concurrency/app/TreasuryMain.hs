-- ANCHOR: all
-- 💰 Count with many clerks at once, and pay everyone on pay day.
-- たくさんの係で同時に数え、給料日に全員へ払う
module Main (main) where

import Treasury

main :: IO ()
main = do
  countWithIORef  >>= print
  countWithAtomic >>= print
  payDay          >>= print
-- ANCHOR_END: all
