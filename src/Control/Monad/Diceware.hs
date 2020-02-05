{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
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

randomSentence :: (MonadReader r m, HasDiceware r, MonadRandom m) => Int -> m Text
randomSentence n =
  T.unwords <$> runConduit (C.repeatM randomWord .| C.concat .| C.take n .| C.sinkList)
