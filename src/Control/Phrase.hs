module Control.Phrase where

-- import qualified Control.Monad.Diceware  as DW
import qualified Control.Monad.GnuPG     as GPG

import           Control.Monad.Except    (runExceptT, throwError)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Reader    (MonadReader)
import           Control.Monad.Trans     (liftIO, lift)

import           Control.Lens            (view)

import           System.Directory        (doesFileExist, renameFile)

import           Data.ByteString         (ByteString)

import Conduit (runConduit, yield, (.|), runResourceT)
import Data.Conduit.Combinators (sinkSystemTempFile)

data FileExists = FileExists

encryptFile
  :: (MonadUnliftIO m, MonadReader r m, GPG.HasRecipient r)
  => FilePath
  -> ByteString
  -> m (Either FileExists ())
encryptFile path content = runExceptT $ do
  r <- view GPG.recipient
  e <- liftIO (doesFileExist path)
  if e then throwError FileExists else lift (encryptAndWrite r)
  where
    encryptAndWrite r = do
      encrypted <- GPG.encrypt r content
      f <- runResourceT (runConduit (yield encrypted .| sinkSystemTempFile "phrase"))
      liftIO (renameFile f path)
