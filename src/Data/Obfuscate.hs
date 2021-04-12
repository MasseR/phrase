{-# LANGUAGE LambdaCase #-}
module Data.Obfuscate
  ( Obfuscate(..)
  , obfuscate
  )
  where

import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Tree (Forest, Tree (..))

data Obfuscate
  = Obfuscate
  | NoObfuscation
  deriving (Show)

-- | Obfuscate the sentence
--
-- Obfuscates the sentence by replacing some characters with similar characters.
--
-- For example a word "leet" could be transformed into "l33t".
--
-- This makes random guessing of the sentence a little bit more difficult, but
-- more importantly it adds numbers, special characters and uppercase
-- characters to satisfy some password requirements
obfuscate :: (Integral a, Num a) => Obfuscate -> [a] -> Text -> Text
obfuscate NoObfuscation _ original = original
obfuscate Obfuscate indexes original =
  maybe original T.pack . paths "" indexes . variations . T.unpack $ original
  where
    paths :: (Integral a, Num a) => String -> [a] -> Forest Char -> Maybe String
    paths _ [] _ = Nothing
    paths acc _ [] = Just acc
    paths acc (x:xs) fs =
      case fs !! (fromIntegral x `mod` length fs) of
           Node c fs' -> paths (acc <> [c]) xs fs'
    variations :: String -> Forest Char
    variations = \case
      [] -> []
      x:xs ->
        map (\c -> Node c $ variations xs) (replacements x)

-- | Replacement characters
replacements :: Char -> [Char]
replacements = \case
  ' ' -> " !()?/&%"
  'o' -> "oO0"
  'a' -> "aA4"
  'i' -> "iI1"
  'e' -> "eE3"
  's' -> "sSz"
  x   -> [x]
