{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Command where

import Control.Phrase (Force (..))
import Data.Bool (bool)
import Data.Obfuscate (Obfuscate (..))
import Data.Text (Text)
import Options.Applicative

data Args = Args
  { argsName :: Text,
    argsLength :: Int,
    argsObfuscate :: Obfuscate,
    argsForce :: Force
  }
  deriving (Show)

parseArgs :: Parser Args
parseArgs =
  Args
    <$> parseName
    <*> parseLength
    <*> parseObfuscate
    <*> parseForce
  where
    parseName :: Parser Text
    parseName = argument str (metavar "pass-name" <> help "Name of the secret")
    parseLength :: Parser Int
    parseLength = argument auto (metavar "pass-size" <> help "Sentence size" <> value 5 <> showDefault)
    parseObfuscate :: Parser Obfuscate
    parseObfuscate = bool NoObfuscation Obfuscate <$> switch (long "obfuscate" <> help "Switch some characters with their symbols")
    parseForce :: Parser Force
    parseForce = bool NoForce Force <$> switch (long "force" <> help "Overwrite existing secret")
