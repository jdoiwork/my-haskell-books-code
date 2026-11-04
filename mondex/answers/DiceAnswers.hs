-- トレーニングの解答：「CLI ツールを作る」と
-- 「サブコマンドのある CLI ツール」の章
module DiceAnswers
  ( Roll (..)
  , DiceCommand (..)
  , rollInfo
  , safeRollInfo
  , diceInfo
  , DiceOptions (..)
  , diceOptionsInfo
  ) where

import Options.Applicative
import Text.Read (readMaybe)

-- ANCHOR: roll
-- 🎲 Roll some dice: how many sides, how many times.
-- サイコロを振る：何面のサイコロを、何回
data Roll = Roll
  { sides :: Int
  , times :: Int
  }
  deriving (Show, Eq)

-- 🎲 dice [--sides N] [--times K]
rollParser :: Parser Roll
rollParser =
  Roll
    <$> option auto sidesSettings
    <*> option auto timesSettings
  where
    sidesSettings = mconcat
      [ long "sides"
      , metavar "N"
      , value 6
      , showDefault
      , help "How many sides the die has"
      ]
    timesSettings = mconcat
      [ long "times"
      , metavar "K"
      , value 1
      , showDefault
      , help "How many times to roll"
      ]

rollInfo :: ParserInfo Roll
rollInfo = info (rollParser <**> helper) (progDesc "Roll some dice")
-- ANCHOR_END: roll

-- ANCHOR: safeRoll
-- 🛡️ A die needs 2 sides or more.
-- サイコロには、2面以上が必要
sidesReader :: ReadM Int
sidesReader = eitherReader $ \word ->
  case readMaybe word of
    Just n | n >= 2 -> Right n
    -- 「サイコロは2面以上だよ」
    _               -> Left ("a die needs 2 sides or more, not " ++ word)

-- 🎲 The same as rollParser, but --sides is checked.
-- rollParser と同じ。ただし --sides を確かめる
safeRollParser :: Parser Roll
safeRollParser =
  Roll
    <$> option sidesReader sidesSettings
    <*> option auto timesSettings
  where
    sidesSettings = mconcat
      [ long "sides"
      , metavar "N"
      , value 6
      , showDefault
      , help "How many sides the die has (2 or more)"
      ]
    timesSettings = mconcat
      [ long "times"
      , metavar "K"
      , value 1
      , showDefault
      , help "How many times to roll"
      ]

safeRollInfo :: ParserInfo Roll
safeRollInfo = info (safeRollParser <**> helper) (progDesc "Roll some dice")
-- ANCHOR_END: safeRoll

-- ANCHOR: subcommands
-- 🪙 Roll dice, or flip a coin.
-- サイコロを振るか、コインを投げるか
data DiceCommand
  = RollDice Roll
  | FlipCoin
  deriving (Show, Eq)

-- 🎲 dice roll [--sides N] [--times K] | dice flip
diceParser :: Parser DiceCommand
diceParser = hsubparser commands
  where
    commands = mconcat
      [ command "roll"
          (info (RollDice <$> safeRollParser) (progDesc "Roll some dice"))
      , command "flip"
          (info (pure FlipCoin) (progDesc "Flip a coin"))
      ]

diceInfo :: ParserInfo DiceCommand
diceInfo = info (diceParser <**> helper) (progDesc "Dice and coins")
-- ANCHOR_END: subcommands

-- ANCHOR: globalOption
-- 🗣️ Options for the whole command, and the subcommand.
-- コマンド全体のオプションと、サブコマンド
data DiceOptions = DiceOptions
  { verbose     :: Bool
  , diceCommand :: DiceCommand
  }
  deriving (Show, Eq)

-- 🗣️ dice [--verbose] COMMAND
diceOptionsParser :: Parser DiceOptions
diceOptionsParser =
  DiceOptions
    <$> switch verboseSettings
    <*> diceParser
  where
    verboseSettings = mconcat
      [ long "verbose"
      , short 'v'
      , help "Tell what is happening"
      ]

diceOptionsInfo :: ParserInfo DiceOptions
diceOptionsInfo =
  info (diceOptionsParser <**> helper) (progDesc "Dice and coins")
-- ANCHOR_END: globalOption
