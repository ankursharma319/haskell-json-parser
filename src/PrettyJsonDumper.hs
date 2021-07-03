module PrettyJsonDumper (prettyDump) where

import JsonParser ( JsonValue(..) )
import Data.List
import Prelude

prettyDump :: JsonValue -> String
prettyDump = prettyDumpWithIndentation 0

prettyDumpWithIndentation :: Int -> JsonValue -> String
prettyDumpWithIndentation i JsonNull = indent i "null"
prettyDumpWithIndentation i (JsonBool x)
  | x = indent i "true"
  | otherwise = indent i "false"
prettyDumpWithIndentation i (JsonInteger x) = indent i (show x)
prettyDumpWithIndentation i (JsonString x) = indent i "\"" ++ x ++ "\""
prettyDumpWithIndentation i (JsonArray xs) =
  rStripLastNewLine $
    unlines $
      [indent i "["]
        ++ removeCommaFromEndOfLastLine (fmap (appendComma . prettyDumpWithIndentation (i + 1)) xs)
        ++ [indent i "]"]
prettyDumpWithIndentation i (JsonObject xs) =
  rStripLastNewLine $
    unlines $
      [indent i "{"]
        ++ removeCommaFromEndOfLastLine (fmap (appendComma . prettyDumpKeyValueWithIndentation (i + 1)) xs)
        ++ [indent i "}"]


prettyDumpKeyValueWithIndentation :: Int -> (String, JsonValue) -> String
prettyDumpKeyValueWithIndentation i (key, value) =
  let singleLinedDump j (keyName, jsonValue) =
        indent j $ "\"" ++ keyName ++ "\"" ++ ": " ++ prettyDumpWithIndentation 0 jsonValue
      separateLinedDump j (keyName, jsonValue) =
        indent j $ "\"" ++ keyName ++ "\"" ++ ": \n" ++ prettyDumpWithIndentation (j + 1) jsonValue
      prettyDumpFunc =
        case value of
          JsonArray _ ->
            separateLinedDump
          JsonObject _ ->
            separateLinedDump
          _ ->
            singleLinedDump
   in prettyDumpFunc i (key, value)

-- Strip new line if it is the last character in the string
rStripLastNewLine :: String -> String
rStripLastNewLine x = do
  if "\n" `isSuffixOf` x
    then init x
    else x

-- Strip last comma (,) if it is the last character in the string
rStripLastComma :: String -> String
rStripLastComma x = do
  if "," `isSuffixOf` x
    then init x
    else x

indent :: Int -> String -> String
indent i x = concat (replicate i "\t" ++ [x])

appendComma :: String -> String
appendComma x = x ++ ","

removeCommaFromEndOfLastLine :: [String] -> [String]
removeCommaFromEndOfLastLine [] = []
removeCommaFromEndOfLastLine x = init x ++ [rStripLastComma $ last x]
