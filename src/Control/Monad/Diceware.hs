module Control.Monad.Diceware where

import           Data.Diceware             (Diceware)
import qualified Data.Diceware             as DW

import           Control.Lens              (Lens', lens, set, view)

import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Crypto.Random             (MonadRandom, getRandomBytes)

import qualified Data.ByteString           as B
import           Data.Text                 (Text)

class HasDiceware a where
  {-# MINIMAL getDiceware, setDiceware | diceware #-}
  getDiceware :: a -> Diceware Text
  getDiceware = view diceware
  setDiceware :: a -> Diceware Text -> a
  setDiceware = flip (set diceware)
  diceware :: Lens' a (Diceware Text)
  diceware = lens getDiceware setDiceware

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
