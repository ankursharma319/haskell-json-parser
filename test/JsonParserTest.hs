module Main (main) where

import JsonParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    defaultMain tests

tests :: TestTree
tests = testGroup "UnitTests and PropertyTests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Should fail" $
      True @?= False
  ]
