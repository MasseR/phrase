{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NamedFieldPuns          #-}
module Main where

import           Data.Text                (Text)
import qualified Data.Text.IO as T

import           Options.Applicative

import           Paths_phrase             (getDataFileName)

import           Data.Diceware            (Diceware)
import qualified Data.Diceware            as DW

import           Control.Monad.Diceware
import           Control.Monad.Reader     (MonadReader, ReaderT, runReaderT)

import           Data.Maybe               (catMaybes)

import           Control.Monad            (replicateM)
import           Control.Monad.Trans      (MonadIO, lift)
import           Crypto.Random            (MonadRandom (..))

import           Conduit                  (runResourceT)
import           Data.Conduit             (runConduit, (.|))
import           Data.Conduit.Combinators (decodeUtf8, sourceFile)
import qualified Data.Conduit.Combinators as C
import           Data.Conduit.List        (unfoldM)
import qualified Data.Conduit.Text        as Conduit.Text


data Args
  = Args { argsName   :: Text
         , argsLength :: Int
         }
  deriving Show

parseArgs :: Parser Args
parseArgs =
  Args <$> parseName
       <*> parseLength
  where
    parseName :: Parser Text
    parseName = argument str (metavar "pass-name" <> help "Name of the secret")
    -- Not implemented yet
    -- parseName = pure ""
    parseLength :: Parser Int
    parseLength = argument auto (metavar "pass-size" <> help "Sentence size" <> value 3 <> showDefault)

newtype AppM a = AppM (ReaderT (Diceware Text) IO a)
  deriving (Functor, Applicative, Monad, MonadReader (Diceware Text), MonadIO)

instance MonadRandom AppM where
  getRandomBytes = AppM . lift . getRandomBytes

runAppM :: AppM a -> Diceware Text -> IO a
runAppM (AppM f) = runReaderT f

main :: IO ()
main = do
  Args{argsLength} <- execParser opts
  source <- getDataFileName "data/words"
  dw <- runResourceT (runConduit (readUtf8 source .| C.concatMap DW.parseLine .| C.foldl (flip DW.insert) DW.empty))
  runAppM (randomSentence argsLength) dw >>= T.putStrLn
  where
    readUtf8 source = sourceFile source .| decodeUtf8 .| Conduit.Text.lines
    opts :: ParserInfo Args
    opts = info (parseArgs <**> helper) fullDesc
