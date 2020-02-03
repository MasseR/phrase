{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Diceware where

import           GHC.Natural

import           Data.Text            (Text)

import           Data.Attoparsec.Text (Parser, char, many1, parseOnly, takeText)

import           Data.Foldable        (asum)

import           Data.Map             (Map)
import qualified Data.Map.Strict      as M

newtype Dice = Dice Natural
  deriving (Show, Eq, Ord, Num)

dice :: Integral a => a -> Dice
dice n = Dice ((fromIntegral n `mod` 6) + 1)

data Dices = Dices !Dice !Dice !Dice !Dice !Dice
           deriving (Show, Eq, Ord)

newtype Diceware a = Diceware (Map Dices a)
  deriving Show

lookup :: Dices -> Diceware a -> Maybe a
lookup d (Diceware m) = M.lookup d m

-- Suitable for folding
insert :: (Dices, a) -> Diceware a -> Diceware a
insert (k,v) (Diceware m) = Diceware (M.insert k v m)

empty :: Diceware a
empty = Diceware M.empty

parseDice :: Parser Dices
parseDice =
  Dices <$> parse
        <*> parse
        <*> parse
        <*> parse
        <*> parse
  where
    parse :: Parser Dice
    parse = Dice <$> asum [ 1 <$ char '1'
                          , 2 <$ char '2'
                          , 3 <$ char '3'
                          , 4 <$ char '4'
                          , 5 <$ char '5'
                          , 6 <$ char '6' ]

parseDiceware :: Parser (Dices, Text)
parseDiceware = do
  d <- parseDice
  _ <- many1 (char ' ')
  t <- takeText
  return (d,t)

parseLine :: Text -> Either String (Dices, Text)
parseLine = parseOnly parseDiceware
