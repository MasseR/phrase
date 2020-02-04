{-# Language DeriveGeneric #-}
module Data.Environment where

import           Control.Monad.Diceware (HasDiceware (..))
import           Control.Monad.GnuPG    (HasRecipient (..), Recipient)
import           Data.Diceware          (Diceware)

import           Data.Text              (Text)

import           GHC.Generics           (Generic)

data Env
  = Env { appDiceware  :: Diceware Text
        , appRecipient :: Recipient
        }
  deriving Generic

instance HasRecipient Env where
  getRecipient = appRecipient
  setRecipient env b = env{appRecipient = b}
instance HasDiceware Env where
  getDiceware = appDiceware
  setDiceware env b = env{appDiceware = b}
