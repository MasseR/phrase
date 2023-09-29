{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Command where

import           Data.Text           (Text)

import           Options.Applicative


import           Data.Bool           (bool)
import           Data.Obfuscate      (Obfuscate (..))


data Args
  = Args { argsName      :: Text
         , argsLength    :: Int
         , argsObfuscate :: Obfuscate
         }
  deriving Show

parseArgs :: Parser Args
parseArgs =
  Args <$> parseName
       <*> parseLength
       <*> parseObfuscate
  where
    parseName :: Parser Text
    parseName = argument str (metavar "pass-name" <> help "Name of the secret")
    parseLength :: Parser Int
    parseLength = argument auto (metavar "pass-size" <> help "Sentence size" <> value 5 <> showDefault)
    parseObfuscate :: Parser Obfuscate
    parseObfuscate = bool NoObfuscation Obfuscate <$> switch (long "obfuscate" <> help "Switch some characters with their symbols")
