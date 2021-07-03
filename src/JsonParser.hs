module JsonParser (JsonValue (..), prettyPrint, PrettyDumpable (..)) where

import Data.List
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

rStripNewLine :: String -> String
rStripNewLine x = do
  if "\n" `isSuffixOf` x
    then init x
    else x

rStripLastComma :: String -> String
rStripLastComma x = do
  if "," `isSuffixOf` x
    then init x
    else x

indent :: Int -> String -> String
indent i x = concat (replicate i "\t" ++ [x])

appendComma :: String -> String
appendComma x = x ++ ","

removeCommaFromLastString :: [String] -> [String]
removeCommaFromLastString [] = []
removeCommaFromLastString x = init x ++ [rStripLastComma $ last x]

prettyDumpKeyValueWithIndentation :: Int -> (String, JsonValue) -> String
prettyDumpKeyValueWithIndentation i (key, value) =
  let singleLined j (keyName, jsonValue) =
        indent j $ "\"" ++ keyName ++ "\"" ++ ": " ++ prettyDumpWithIndentation 0 jsonValue
      separateLined j (keyName, jsonValue) =
        indent j $ "\"" ++ keyName ++ "\"" ++ ": \n" ++ prettyDumpWithIndentation (j + 1) jsonValue
      func_to_apply =
        case value of
          JsonArray _ ->
            separateLined
          JsonObject _ ->
            separateLined
          _ ->
            singleLined
   in func_to_apply i (key, value)

prettyDumpWithIndentation :: Int -> JsonValue -> String
prettyDumpWithIndentation i JsonNull = indent i "null"
prettyDumpWithIndentation i (JsonBool x)
  | x = indent i "true"
  | otherwise = indent i "false"
prettyDumpWithIndentation i (JsonInteger x) = indent i (show x)
prettyDumpWithIndentation i (JsonString x) = indent i "\"" ++ x ++ "\""
prettyDumpWithIndentation i (JsonArray xs) =
  rStripNewLine $
    unlines $
      [indent i "["]
        ++ removeCommaFromLastString (fmap (appendComma . prettyDumpWithIndentation (i + 1)) xs)
        ++ [indent i "]"]
prettyDumpWithIndentation i (JsonObject xs) =
  rStripNewLine $
    unlines $
      [indent i "{"]
        ++ removeCommaFromLastString (fmap (appendComma . prettyDumpKeyValueWithIndentation (i + 1)) xs)
        ++ [indent i "}"]

instance PrettyDumpable JsonValue where
  prettyDump = prettyDumpWithIndentation 0

prettyPrint :: String -> String
prettyPrint _inputJsonString = undefined
