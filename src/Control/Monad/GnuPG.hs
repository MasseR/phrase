module Control.Monad.GnuPG where

import           Data.ByteString          (ByteString)
import           UnliftIO                 (MonadUnliftIO, throwIO)


import           Control.Lens             (Iso', Lens', iso, lens, set, strict,
                                           view)

import           Conduit                  (yield)
import           Data.Conduit.Combinators (sinkLazy, stderr)
import           Data.Conduit.Process

import           Control.Exception        (Exception (..))
import           Control.Monad            (when)
import           System.Exit              (ExitCode (..))

newtype Recipient = Recipient String
  deriving Show

-- | Lenses for recipient
class HasRecipient a where
  {-# MINIMAL getRecipient, setRecipient | recipient #-}
  getRecipient :: a -> Recipient
  getRecipient = view recipient

  setRecipient :: a -> Recipient -> a
  setRecipient = flip (set recipient)

  recipient :: Lens' a Recipient
  recipient = lens getRecipient setRecipient

-- | Accessor for recipient
_Recipient :: Iso' Recipient String
_Recipient = iso (\(Recipient r) -> r) Recipient

data GPGError = GPGError ExitCode
              deriving Show

instance Exception GPGError

-- | Encrypt with gpg (in memory)
encrypt :: MonadUnliftIO m => Recipient -> ByteString -> m ByteString
encrypt r content = do
  (ec, o, _) <- sourceProcessWithStreams gpg
                                         input
                                         sinkLazy
                                         stderr
  when (ec /= ExitSuccess) (throwIO (GPGError ec))
  pure (view strict o)
  where
    gpg :: CreateProcess
    gpg = proc "gpg" ["--encrypt", "--recipient", view _Recipient r, "--batch"]
    input = yield content
