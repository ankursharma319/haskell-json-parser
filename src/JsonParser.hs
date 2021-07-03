module JsonParser (JsonValue (..), prettyPrint, PrettyDumpable (..)) where

import Prelude

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonInteger Int
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving stock (Eq, Show)

class PrettyDumpable a where
  prettyDump :: a -> String
  {-# MINIMAL prettyDump #-}

instance PrettyDumpable JsonValue where
  prettyDump = show

prettyPrint :: String -> String
prettyPrint _inputJsonString = undefined
