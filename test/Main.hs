module Main (main) where

import JsonParser (JsonValue (..), Parser (runParser), jsonValueParser)
import PrettyJsonDumper (prettyDump)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "UnitTests and PropertyTests"
    [ prettyDumpUnitTests,
      parsingUnitTests
    ]

parsingUnitTests :: TestTree
parsingUnitTests =
  testGroup
    "Json Parser Unit tests"
    [ mustParseJsonNullCorrectly,
      mustParseJsonBoolCorrectly
    ]

mustParseJsonNullCorrectly :: TestTree
mustParseJsonNullCorrectly =
  testCase "should parse null correctly" $ do
    runParser jsonValueParser "null" @?= Just ("", JsonNull)
    runParser jsonValueParser "notnull" @?= Nothing

mustParseJsonBoolCorrectly :: TestTree
mustParseJsonBoolCorrectly =
  testCase "should parse boolean correctly" $ do
    runParser jsonValueParser "false" @?= Just ("", JsonBool False)
    runParser jsonValueParser "true" @?= Just ("", JsonBool True)
    runParser jsonValueParser "some" @?= Nothing

prettyDumpUnitTests :: TestTree
prettyDumpUnitTests =
  testGroup
    "Pretty Json Dumper Unit tests"
    [ mustDumpJsonIntegerPrettily,
      mustDumpJsonBoolPrettily,
      mustDumpJsonNullPrettily,
      mustDumpJsonStringPrettily,
      mustDumpJsonArrayPrettily,
      mustDumpJsonObjectPrettily,
      mustDumpJsonObjectEmbeddedInListPrettily
    ]

mustDumpJsonIntegerPrettily :: TestTree
mustDumpJsonIntegerPrettily =
  testCase "should prettyDump Json Integer prettily" $
    prettyDump (JsonInteger 4) @?= "4"

mustDumpJsonBoolPrettily :: TestTree
mustDumpJsonBoolPrettily =
  testCase "should prettyDump Json Bool prettily" $
    prettyDump (JsonBool True) @?= "true"

mustDumpJsonNullPrettily :: TestTree
mustDumpJsonNullPrettily =
  testCase "should prettyDump Json Null prettily" $
    prettyDump JsonNull @?= "null"

mustDumpJsonStringPrettily :: TestTree
mustDumpJsonStringPrettily =
  let expected = "\"some random string\""
      got = prettyDump (JsonString "some random string")
   in testCase "should prettyDump Json String prettily" $
        got @?= expected

mustDumpJsonArrayPrettily :: TestTree
mustDumpJsonArrayPrettily =
  let testJsonValue =
        JsonArray
          [ JsonBool False,
            JsonInteger 4,
            JsonString "some random string"
          ]
      expected =
        "[\n\
        \\tfalse,\n\
        \\t4,\n\
        \\t\"some random string\"\n\
        \]"
      got = prettyDump testJsonValue
   in testCase "should prettyDump Json Array prettily" $
        got @?= expected

mustDumpJsonObjectPrettily :: TestTree
mustDumpJsonObjectPrettily =
  let testJsonValue =
        JsonObject
          [ ("key1", JsonBool True),
            ( "key2",
              JsonArray
                [ JsonInteger 4,
                  JsonString "another random string"
                ]
            )
          ]
      expected =
        "{\n\
        \\t\"key1\": true,\n\
        \\t\"key2\": \n\
        \\t\t[\n\
        \\t\t\t4,\n\
        \\t\t\t\"another random string\"\n\
        \\t\t]\n\
        \}"
      got = prettyDump testJsonValue
   in testCase "should prettyDump Json Object prettily" $
        got @?= expected

mustDumpJsonObjectEmbeddedInListPrettily :: TestTree
mustDumpJsonObjectEmbeddedInListPrettily =
  let testJsonValue =
        JsonArray
          [ JsonObject
              [ ( "key1",
                  JsonObject
                    [ ("key2", JsonArray [JsonString "hello"])
                    ]
                )
              ]
          ]
      expected =
        "[\n\
        \\t{\n\
        \\t\t\"key1\": \n\
        \\t\t\t{\n\
        \\t\t\t\t\"key2\": \n\
        \\t\t\t\t\t[\n\
        \\t\t\t\t\t\t\"hello\"\n\
        \\t\t\t\t\t]\n\
        \\t\t\t}\n\
        \\t}\n\
        \]"
      got = prettyDump testJsonValue
   in testCase "should prettyDump embedded Json Object prettily" $
        got @?= expected
