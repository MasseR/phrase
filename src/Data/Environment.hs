{-# LANGUAGE DeriveGeneric #-}
module Data.Environment where

import           Control.Monad.Diceware   (HasDiceware (..))
import           Control.Monad.GnuPG      (HasRecipient (..), Recipient (..))
import           Data.Diceware            (Diceware)
import           Data.Diceware            as DW
import           Data.Store               (HasStore (..), Store (..))

import           System.FilePath

import           Conduit                  (runResourceT)
import           Data.Conduit             (runConduit, (.|))
import           Data.Conduit.Combinators (decodeUtf8, sourceFile)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text        as Conduit.Text

import           Data.Char                (isAlphaNum)
import           Data.Text                (Text)

import           Data.Generics.Product    (typed)
import           GHC.Generics             (Generic)

data Env
  = Env { envDiceware  :: Diceware Text
        , envRecipient :: Recipient
        , envStore     :: Store
        }
  deriving Generic

instance HasRecipient Env where
  recipient = typed

instance HasDiceware Env where
  diceware = typed

instance HasStore Env where
  store = typed

withEnv
  :: FilePath -- ^ Path to the diceware dictionary
  -> FilePath -- ^ Path to the password store
  -> (Env -> IO a)
  -> IO a
withEnv dictFile storePath f = do
  dw <- runResourceT (runConduit (readUtf8 dictFile .| toDict))
  r <- filter isAlphaNum <$> readFile (storePath </> ".gpg-id")
  let env = Env dw (Recipient r) (Store storePath)
  f env
  where
    toDict = C.concatMap DW.parseLine .| C.foldl (flip insert) DW.empty
    readUtf8 source = sourceFile source .| decodeUtf8 .| Conduit.Text.lines
