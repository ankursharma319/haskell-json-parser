module JsonParser (JsonValue(..), prettyPrint) where

import Prelude

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonInteger Int
  | JsonString String


prettyPrint :: String -> String
prettyPrint _inputJsonString = undefined
