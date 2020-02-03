module Main where

import           Data.Text (Text)

import Options.Applicative

data Args
  = Args { argsName   :: Text
         , argsLength :: Maybe Int
         }
  deriving Show

parseArgs :: Parser Args
parseArgs =
  Args <$> parseName
       <*> parseLength
  where
    parseName :: Parser Text
    parseName = argument str (metavar "pass-name" <> help "Name of the secret")
    parseLength :: Parser (Maybe Int)
    parseLength = optional (option auto (metavar "pass-size" <> help "Sentence size"))

main :: IO ()
main = do
  args <- execParser opts
  print args
  where
    opts :: ParserInfo Args
    opts = info (parseArgs <**> helper) fullDesc
