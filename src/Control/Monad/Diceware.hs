{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
module Control.Monad.Diceware where

import           Data.Diceware             (Diceware)
import qualified Data.Diceware             as DW

import           Control.Lens              (Lens', lens, set, view)

import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Crypto.Random             (MonadRandom, getRandomBytes)

import           Data.Conduit              (runConduit, (.|))
import qualified Data.Conduit.Combinators  as C

import qualified Data.ByteString           as B
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Control.Monad.Trans       (MonadIO, liftIO)
import           Data.Tree                 (Forest, Tree (..))
import           Data.Word                 (Word8)
import           System.IO.Unsafe          (unsafeInterleaveIO)

class HasDiceware a where
  {-# MINIMAL getDiceware, setDiceware | diceware #-}
  getDiceware :: a -> Diceware Text
  getDiceware = view diceware
  setDiceware :: a -> Diceware Text -> a
  setDiceware = flip (set diceware)
  diceware :: Lens' a (Diceware Text)
  diceware = lens getDiceware setDiceware

instance HasDiceware (Diceware Text) where
  getDiceware = id
  setDiceware = const

-- | Generate an infinite list of random numbers
randoms :: MonadIO m => m [Word8]
randoms = liftIO $ do
  [x] <- B.unpack <$> getRandomBytes 1
  xs <- unsafeInterleaveIO randoms
  pure $ x : xs

-- | Replacement characters
replacements :: Char -> [Char]
replacements = \case
  ' ' -> " !()?/&%"
  'o' -> "oO0"
  'a' -> "aA4"
  'i' -> "iI1"
  'e' -> "eE3"
  's' -> "sSz"
  x -> [x]

-- | Obfuscate the sentence
--
-- Obfuscates the sentence by replacing some characters with similar characters.
--
-- For example a word "leet" could be transformed into "l33t".
--
-- This makes random guessing of the sentence a little bit more difficult, but
-- more importantly it adds numbers, special characters and uppercase
-- characters to satisfy some password requirements
obfuscate :: (Integral a, Num a) => [a] -> Text -> Text
obfuscate indexes original = maybe original T.pack . paths "" indexes . variations . T.unpack $ original
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


randomWord :: (MonadReader r m, HasDiceware r, MonadRandom m) => m (Maybe Text)
randomWord = runMaybeT $ do
  dw <- view diceware
  MaybeT dices >>= \k -> MaybeT (pure (DW.lookup k dw))
  where
    dices = toD . map DW.dice . B.unpack <$> getRandomBytes 5
    toD x =
      case x of
           [a,b,c,d,e] -> Just (DW.Dices a b c d e)
           _           -> Nothing

randomSentence :: (MonadIO m, MonadReader r m, HasDiceware r, MonadRandom m) => Int -> m Text
randomSentence n = do
  rs <- randoms
  obfuscate rs . T.unwords <$> runConduit (C.repeatM randomWord .| C.concat .| C.take n .| C.sinkList)
