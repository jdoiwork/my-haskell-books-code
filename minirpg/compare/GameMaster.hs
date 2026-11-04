{-# LANGUAGE OverloadedRecordDot #-}
-- The mini RPG as an experienced Haskeller might write it (for the column).
-- 慣れた人が書いたミニ RPG（コラム用）
module Main (main) where

import System.IO     (hFlush, stdout)
import System.Random (randomRIO)

-- ANCHOR: body
data Action = Hit Int | Cure Int | Escape
data Battle = Battle { hero, slime :: Int }

command :: String -> Maybe (IO Action)
command = \case
  c | c `elem` ["attack", "a"] -> Just (Hit  <$> randomRIO (8, 14))
    | c `elem` ["heal", "h"]   -> Just (Cure <$> randomRIO (5, 10))
    | c `elem` ["run", "r"]    -> Just (pure Escape)
  _                            -> Nothing

act :: Action -> Battle -> Battle
act (Hit d)  b = b { slime = max 0 (b.slime - d) }
act (Cure h) b = b { hero  = min 50 (b.hero + h) }
act Escape   b = b

main :: IO ()
main = loop (Battle 50 40)
  where
    loop b = do
      putStr ("Hero HP: " <> show b.hero
                <> "  ⚔️  Slime HP: " <> show b.slime <> "\n> ")
      hFlush stdout
      maybe (putStrLn "Huh? Try again." >> loop b) (>>= turn b) . command
        =<< getLine

    turn _ Escape = putStrLn "You ran away safely."
    turn b a
      | b'.slime == 0 = putStrLn "The Slime is defeated. You win!"
      | otherwise     = do
          bite <- randomRIO (3, 9)
          let b'' = b' { hero = max 0 (b'.hero - bite) }
          if b''.hero == 0
            then putStrLn "You were defeated... Game over."
            else loop b''
      where b' = act a b
-- ANCHOR_END: body
