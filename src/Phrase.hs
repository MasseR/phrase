{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Phrase where

import qualified Data.Text.IO        as T

import           Options.Applicative

import           Paths_phrase        (getDataFileName)

import           Data.Environment    (withEnv)

import           Control.Monad.App   (runAppM)
import           Control.Phrase      as Phrase

import           System.Directory    (getHomeDirectory)
import           System.Environment  (lookupEnv)
import           System.FilePath     ((</>))

import           Data.Maybe          (fromMaybe)

import           Control.Monad.Trans (liftIO)

import Command

defaultMain :: IO ()
defaultMain = do
  Args{..} <- execParser opts
  home <- getHomeDirectory
  storeDir <- fromMaybe (home </> ".password-store") <$> lookupEnv "PASSWORD_STORE_DIR"
  source <- getDataFileName "data/words"
  withEnv source storeDir $ \env ->
    runAppM env $ do
      s <- Phrase.generateForName argsObfuscate argsLength argsName
      liftIO (either (const (T.putStrLn "File already exists")) T.putStrLn s)
  where
    opts :: ParserInfo Args
    opts = info (parseArgs <**> helper) fullDesc
