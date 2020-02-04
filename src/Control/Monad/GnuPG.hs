module Control.Monad.GnuPG where

import           Control.Monad.IO.Unlift  (MonadUnliftIO)
import           Data.ByteString          (ByteString)

import           Control.Lens             (strict, view, _2)

import           Conduit                  (yield)
import           Data.Conduit.Combinators (sinkLazy)
import           Data.Conduit.Process

newtype Recipient = Recipient { getRecipient :: String }

-- | Encrypt with gpg (in memory)
encrypt :: MonadUnliftIO m => Recipient -> ByteString -> m ByteString
encrypt recipient content = view (_2 . strict) <$>
  sourceProcessWithStreams gpg
                           input
                           sinkLazy
                           sinkLazy
  where
    gpg :: CreateProcess
    gpg = proc "gpg" ["--encrypt", "--recipient", getRecipient recipient, "--batch"]
    input = yield content
