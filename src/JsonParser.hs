module JsonParser (JsonValue (..)) where

import Prelude

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonInteger Int
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving stock (Eq, Show)
