module Control.Monad.GnuPG where

import           Control.Monad.IO.Unlift  (MonadUnliftIO)
import           Data.ByteString          (ByteString)

import           Control.Lens             (Iso', Lens', iso, lens, set, strict,
                                           view, _2)

import           Conduit                  (yield)
import           Data.Conduit.Combinators (sinkLazy)
import           Data.Conduit.Process

newtype Recipient = Recipient String

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

-- | Encrypt with gpg (in memory)
encrypt :: MonadUnliftIO m => Recipient -> ByteString -> m ByteString
encrypt r content = view (_2 . strict) <$>
  sourceProcessWithStreams gpg
                           input
                           sinkLazy
                           sinkLazy
  where
    gpg :: CreateProcess
    gpg = proc "gpg" ["--encrypt", "--recipient", view _Recipient r, "--batch"]
    input = yield content
