{-# LANGUAGE OverloadedStrings #-}
module Control.Phrase where

import qualified Control.Monad.Diceware   as DW
import qualified Control.Monad.GnuPG      as GPG
import           Data.Store

import           Data.Obfuscate           (Obfuscate)

import           Control.Monad.Except     (ExceptT (..), runExceptT, throwError)
import           Control.Monad.IO.Unlift  (MonadUnliftIO)
import           Control.Monad.Reader     (MonadReader)
import           Control.Monad.Trans      (lift, liftIO)

import           Crypto.Random            (MonadRandom)

import           Control.Lens             (re, view)
import           Data.Text.Strict.Lens    (utf8)

import           System.Directory         (createDirectoryIfMissing,
                                           doesFileExist)
import           System.FilePath          (takeDirectory)

import           Data.ByteString          (ByteString)
import           Data.Text                (Text)

import           Conduit                  (runConduit, runResourceT, yield,
                                           (.|))
import           Data.Conduit.Combinators (sinkFile)

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
      encrypted <- GPG.encrypt r (content <> "\n")
      runResourceT $ do
        liftIO (createDirectoryIfMissing True (takeDirectory path))
        runConduit (yield encrypted .| sinkFile path)

generate
  :: (MonadUnliftIO m, MonadRandom m, MonadReader r m, GPG.HasRecipient r, DW.HasDiceware r)
  => Obfuscate
  -> Int
  -> FilePath
  -> m (Either FileExists Text)
generate obf n path = runExceptT $ do
  sentence <- lift (DW.randomSentence obf n)
  ExceptT (encryptFile path (view (re utf8) sentence))
  pure sentence

generateForName
  :: (MonadUnliftIO m, MonadRandom m, MonadReader r m, GPG.HasRecipient r, DW.HasDiceware r, HasStore r)
  => Obfuscate
  -> Int
  -> Text
  -> m (Either FileExists Text)
generateForName obf n name = do
  s <- view store
  generate obf n (pathForName s name)
