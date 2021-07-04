module JsonParser where

{-( JsonValue (..),
  Parser (..),
  jsonValueParser,
)-}

import Control.Applicative
import Prelude

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonInteger Int
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving stock (Eq, Show)

{-returns what this parser parsed and also
rest of input that is left to be processed,
for other parsers-}
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  {-
  xx :: Parser Int
  xx = fmap ord $ charP 'a'
  _ = runParser xx "ahello" == Just ("hello",97)
  -}
  fmap :: forall a b. (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser resultantRunParse
    where
      resultantRunParse :: String -> Maybe (String, b)
      resultantRunParse input =
        case p input of
          Nothing -> Nothing
          Just (remaining, parsed) -> Just (remaining, f parsed)

instance Applicative Parser where
  {-
  x = pure 5 :: Parser Int
  _ = runParser x "456" == Just ("456",5)
  -}
  pure :: forall a. a -> Parser a
  pure x = Parser runParseFunc
    where
      runParseFunc :: String -> Maybe (String, a)
      runParseFunc input = Just (input, x)

  (<*>) :: forall a b. Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser f) (Parser p) = Parser runParseFunc
    where
      runParseFunc :: String -> Maybe (String, b)
      runParseFunc input = case f input of
        Nothing -> Nothing
        Just (remaining, parsedF) ->
          case p remaining of
            Nothing -> Nothing
            Just (remaining2, parsed2) -> Just (remaining2, parsedF parsed2)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser runParseFunc
    where
      runParseFunc _ = Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser p1) (Parser p2) = Parser runParseFunc
    where
      runParseFunc input = p1 input <|> p2 input

{-
_ = runParser jsonNullParser "nullsd" == Just ("sd",JsonNull)
_ = runParser jsonNullParser "snullsd" == Nothing
-}
jsonNullParser :: Parser JsonValue
jsonNullParser = fmap convertStringToJsonNull nullStringParser
  where
    convertStringToJsonNull :: String -> JsonValue
    convertStringToJsonNull _ = JsonNull
    nullStringParser :: Parser String
    nullStringParser = stringP "null"

{-
_ = runParser jsonBoolParser "truesd" == Just ("sd",JsonBool True)
_ = runParser jsonBoolParser "snullsd" == Nothing
-}
jsonBoolParser :: Parser JsonValue
jsonBoolParser = fmap convertStringToJsonBool (trueStringParser <|> falseStringParser)
  where
    convertStringToJsonBool :: String -> JsonValue
    convertStringToJsonBool bool = case bool of
      "true" -> JsonBool True
      "false" -> JsonBool False
      _ -> undefined
    trueStringParser :: Parser String
    trueStringParser = stringP "true"
    falseStringParser :: Parser String
    falseStringParser = stringP "false"

{-
z = charP 'x'
_ = runParser z "xhello" == Just ("hello",'x')
_ = runParser z "hellonull" == Nothing
-}
charP :: Char -> Parser Char
charP x = Parser runParserFunc
  where
    runParserFunc :: String -> Maybe (String, Char)
    runParserFunc input =
      case input of
        y : ys | y == x -> Just (ys, x)
        _ -> Nothing

{-
z = stringP "null"
_ = runParser z "nullhello" == Just ("hello","null")
_ = runParser z "hellonull" == Nothing
-}
stringP :: String -> Parser String
stringP = traverse charP

jsonValueParser :: Parser JsonValue
jsonValueParser = jsonNullParser <|> jsonBoolParser
