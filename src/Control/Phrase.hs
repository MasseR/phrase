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
import Control.Monad (when)

data FileExists = FileExists

data Force = Force | NoForce
  deriving Show

encryptFile
  :: (MonadUnliftIO m, MonadReader r m, GPG.HasRecipient r)
  => Force
  -> FilePath
  -> ByteString
  -> m (Either FileExists ())
encryptFile force path content = runExceptT $ do
  r <- view GPG.recipient
  e <- liftIO (doesFileExist path)
  case force of
    NoForce -> when e (throwError FileExists)
    Force   -> return ()
  lift (encryptAndWrite r)
  where
    encryptAndWrite r = do
      encrypted <- GPG.encrypt r (content <> "\n")
      runResourceT $ do
        liftIO (createDirectoryIfMissing True (takeDirectory path))
        runConduit (yield encrypted .| sinkFile path)

generate
  :: (MonadUnliftIO m, MonadRandom m, MonadReader r m, GPG.HasRecipient r, DW.HasDiceware r)
  => Force
  -> Obfuscate
  -> Int
  -> FilePath
  -> m (Either FileExists Text)
generate force obf n path = runExceptT $ do
  sentence <- lift (DW.randomSentence obf n)
  ExceptT (encryptFile force path (view (re utf8) sentence))
  pure sentence

generateForName
  :: (MonadUnliftIO m, MonadRandom m, MonadReader r m, GPG.HasRecipient r, DW.HasDiceware r, HasStore r)
  => Force
  -> Obfuscate
  -> Int
  -> Text
  -> m (Either FileExists Text)
generateForName force obf n name = do
  s <- view store
  generate force obf n (pathForName s name)
