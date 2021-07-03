module Main (main) where

import Prelude
import qualified JsonParser ()

main :: IO ()
main = do
  putStrLn "Reading input string from stdin"
  _contents <- getContents
  putStrLn "Pretty printing the parsed json: Not Implemented"

