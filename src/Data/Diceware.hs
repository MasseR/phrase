{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Diceware where

import           GHC.Natural

import           Data.Text                 (Text)

import           Data.Attoparsec.Text

import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Foldable             (asum)

import           Text.Read                 (readMaybe)

import           Data.Map                  (Map)
import qualified Data.Map.Strict           as M

newtype Dice = Dice Int
  deriving (Show, Eq, Ord, Num)

data Dices = Dices !Dice !Dice !Dice !Dice !Dice
           deriving (Show, Eq, Ord)

newtype Dict a = Dict (Map Dices a)

lookup :: Dices -> Dict a -> Maybe a
lookup d (Dict m) = M.lookup d m

-- Suitable for folding
insert :: (Dices, a) -> Dict a -> Dict a
insert (k,v) (Dict m) = Dict (M.insert k v m)

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

