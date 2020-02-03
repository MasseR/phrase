module Main where

import Test.Hspec

import qualified Test.Diceware

main :: IO ()
main = hspec
  Test.Diceware.spec
