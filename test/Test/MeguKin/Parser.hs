{-# LANGUAGE DeriveDataTypeable #-}

module Test.MeguKin.Parser (tests) where

import MeguKin.Parser.Parsers (
  classExport,
  dataTypeExport,
  longIdentifier,
  moduleDeclaration,
  simpleIdentifier,
 )
import MeguKin.Parser.Types (
  Parser,
  ParserShow (parserShow),
  genClassExport,
  genDataTypeExport,
  genModuleDeclaration,
 )

import Data.Either (Either (Left, Right))
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Prelude (Eq, FilePath, Show, String, filter, not, readFile, show, (.), (==))

import Data.Char (isSpace)
import Data.Data (Typeable)
import Data.Tagged (Tagged (Tagged))
import Test.QuickCheck (Arbitrary (arbitrary), Gen)
import qualified Test.QuickCheck.Property as Property
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Providers (IsTest (run, testOptions), Result, singleTest, testFailed, testPassed)
import Test.Tasty.QuickCheck (testProperty)
import Text.Megaparsec (errorBundlePretty, parse)

tests :: TestTree
tests =
  testGroup
    "Parser"
    [ arbitraryTest
    , fixturesTest
    ]

fixturesTest :: TestTree
fixturesTest =
  testGroup
    "Fixtures RoundTrip"
    [ makeStripedSpaceTest
        "moduleDeclaration"
        (makePath "ModuleDeclaration")
        moduleDeclaration
    ]
 where
  makePath name = "examples/Parser/" <> name

arbitraryTest :: TestTree
arbitraryTest =
  testGroup
    "Arbitrary RoundTrip"
    [ makeRoundTripTest "simpleIdentifier" simpleIdentifier
    , makeRoundTripTest "identifier" longIdentifier
    , makeRoundTripTestWithGen "classExport" classExport genClassExport
    , makeRoundTripTestWithGen
        "dataTypeExport"
        dataTypeExport
        genDataTypeExport
    , makeRoundTripTestWithGen
        "moduleDeclaration"
        moduleDeclaration
        genModuleDeclaration
    ]

makeRoundTripTest ::
  forall a.
  Eq a =>
  ParserShow a =>
  Arbitrary a =>
  Show a =>
  TestName ->
  Parser a ->
  TestTree
makeRoundTripTest name parser = makeRoundTripTestWithGen name parser arbitrary

makeRoundTripTestWithGen ::
  forall a.
  Eq a =>
  ParserShow a =>
  Show a =>
  TestName ->
  Parser a ->
  Gen a ->
  TestTree
makeRoundTripTestWithGen name parser gen =
  testProperty name (testParserWithValue parser <$> gen)

testParserWithValue ::
  forall a.
  Eq a =>
  ParserShow a =>
  Show a =>
  Parser a ->
  a ->
  Property.Result
testParserWithValue parser value =
  let stringValue = parserShow value
   in case parse parser "test" stringValue of
        Right result ->
          if result == value
            then Property.succeeded
            else
              Property.failed
                { Property.reason =
                    "RoundTripFailed parsedValue: " <> show result
                }
        Left parserError ->
          let (showedError :: String) = errorBundlePretty parserError
           in Property.failed{Property.reason = showedError}

stripWitheSpace :: String -> String
stripWitheSpace = filter (not . isSpace)

compareWithoutSpaces :: ParserShow a => Parser a -> String -> Result
compareWithoutSpaces parser content =
  case parse parser "test" content of
    Right result ->
      let stripedResult = stripWitheSpace (parserShow result)
          stripedContent = stripWitheSpace content
       in if stripedResult == stripedContent
            then testPassed ""
            else
              testFailed
                ( "Diferent strings, expected:\n" <> show stripedContent
                    <> "\n\nBut got:\n"
                    <> show stripedResult
                )
    Left parserError -> testFailed (errorBundlePretty parserError)

makeStripedSpaceTest ::
  forall a.
  ParserShow a =>
  TestName ->
  FilePath ->
  Parser a ->
  TestTree
makeStripedSpaceTest name path parser =
  singleTest
    name
    (TestWithExternalFile path (compareWithoutSpaces parser))

data TestWithExternalFile = TestWithExternalFile FilePath (String -> Result)
  deriving (Typeable)

instance IsTest TestWithExternalFile where
  run _ (TestWithExternalFile path test) _ = test <$> readFile path
  testOptions = Tagged []
