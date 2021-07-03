module Main (main) where

import JsonParser (JsonValue (..), PrettyDumpable (..))
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests = testGroup "UnitTests and PropertyTests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ mustDumpJsonIntegerPrettily,
      mustDumpJsonBoolPrettily,
      mustDumpJsonNullPrettily,
      mustDumpJsonStringPrettily,
      mustDumpJsonArrayPrettily,
      mustDumpJsonObjectPrettily
    ]

mustDumpJsonIntegerPrettily :: TestTree
mustDumpJsonIntegerPrettily =
  let testJsonValue = JsonInteger 4
      expected = "4"
      got = prettyDump testJsonValue
   in testCase "should prettyDump Json Integer prettily" $
        got @?= expected

mustDumpJsonBoolPrettily :: TestTree
mustDumpJsonBoolPrettily =
  let testJsonValue = JsonBool True
      expected = "true"
      got = prettyDump testJsonValue
   in testCase "should prettyDump Json Bool prettily" $
        got @?= expected

mustDumpJsonNullPrettily :: TestTree
mustDumpJsonNullPrettily =
  let testJsonValue = JsonNull
      expected = "null"
      got = prettyDump testJsonValue
   in testCase "should prettyDump Json Null prettily" $
        got @?= expected

mustDumpJsonStringPrettily :: TestTree
mustDumpJsonStringPrettily =
  let testJsonValue = JsonString "some random string"
      expected = "\"some random string\""
      got = prettyDump testJsonValue
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
        \\t\"some random string\",\n\
        \]"
      got = prettyDump testJsonValue
   in testCase "should prettyDump Json Array prettily" $
        got @?= expected

mustDumpJsonObjectPrettily :: TestTree
mustDumpJsonObjectPrettily =
  let testJsonValue =
        JsonObject
          [ ("key1", JsonBool False),
            ( "key2",
              JsonArray
                [ JsonInteger 4,
                  JsonString "another random string"
                ]
            )
          ]
      expected =
        "{\n\
        \\t\"key1\": false,\n\
        \\t\"key2\": [\n\
        \\t\t4,\n\
        \\t\t\"another random string\",\n\
        \\t]\n\
        \}"
      got = prettyDump testJsonValue
   in testCase "should prettyDump Json Array prettily" $
        got @?= expected
