{-# LANGUAGE OverloadedStrings #-}
module Test.Diceware where

import           Test.Hspec

import           Data.Diceware

import Data.Attoparsec.Text (parseOnly)

spec :: Spec
spec =
  describe "Parsing diceware" $ do
    it "parses 11111 as a 5-tuple of ints" $
      parseOnly parseDice "11111 foo" `shouldBe` Right (Dices 1 1 1 1 1)
    it "parses 12345 as a tuple" $
      parseOnly parseDiceware "12345 foo" `shouldBe` Right (Dices 1 2 3 4 5, "foo")
