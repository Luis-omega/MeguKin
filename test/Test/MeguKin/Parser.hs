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

import Control.Applicative (Applicative (pure), (<*>))
import Data.Char (isSpace)
import Data.Either (Either (Left, Right))
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.Traversable (sequence)
import System.Directory (listDirectory)
import System.IO (IO, print)
import Prelude (
  Eq,
  FilePath,
  Show,
  String,
  filter,
  fst,
  not,
  readFile,
  show,
  snd,
  ($),
  (.),
  (==),
 )

import Data.Tagged (Tagged (Tagged))
import Test.QuickCheck (Arbitrary (arbitrary), Gen)
import qualified Test.QuickCheck.Property as Property
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  Result,
  singleTest,
  testFailed,
  testPassed,
 )
import Test.Tasty.QuickCheck (testProperty)
import Text.Megaparsec (errorBundlePretty, parse)
import Type.Reflection (Typeable)

tests :: IO TestTree
tests =
  testGroup "Parser"
    <$> sequence
      [ arbitraryTest
      , fixturesTest
      ]

fixturesTest :: IO TestTree
fixturesTest =
  testGroup "Fixtures RoundTrip"
    <$> sequence
      [ internalTestWithFolder
          "moduleDeclaration"
          moduleDeclaration
          (makePath "ModuleDeclaration/")
      ]
 where
  makePath name = "examples/Parser/" <> name

arbitraryTest :: IO TestTree
arbitraryTest =
  pure $
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
  Parser a ->
  FilePath ->
  TestTree
makeStripedSpaceTest name parser path =
  singleTest
    name
    (TestWithFile path (compareWithoutSpaces parser))

internalTestWithFolder ::
  forall a.
  ParserShow a =>
  TestName ->
  Parser a ->
  FilePath ->
  IO TestTree
internalTestWithFolder name parser path =
  let allFiles = getFolderTest path
      getPositive = fst <$> allFiles
      getNegative = snd <$> allFiles
      positiveTree = (<$>) (makeTest "Positives/") <$> getPositive
      negativeTree = (<$>) (makeTest "Negatives/") <$> getNegative
      positiveGroup = (: []) . testGroup "positive" <$> positiveTree
      negativeGroup = (: []) . testGroup "negative" <$> negativeTree
   in do
        names <- allFiles
        print names
        testGroup name <$> ((<>) <$> positiveGroup <*> negativeGroup)
 where
  makeTest :: String -> FilePath -> TestTree
  makeTest cas fileName =
    makeStripedSpaceTest
      fileName
      parser
      ( path <> cas
          <> fileName
      )

type PositiveFiles = [FilePath]
type NegativeFiles = [FilePath]

getFolderTest ::
  FilePath ->
  IO (PositiveFiles, NegativeFiles)
getFolderTest path = (,) <$> positiveFiles <*> negativeFiles
 where
  makePath :: FilePath -> FilePath
  makePath relativePath = path <> relativePath

  positiveFiles :: IO [FilePath]
  positiveFiles = listDirectory $ makePath "Positives/"

  negativeFiles :: IO [FilePath]
  negativeFiles = listDirectory $ makePath "Negatives/"

data TestWithExternal
  = TestWithFile FilePath (String -> Result)
  deriving (Typeable)

instance IsTest TestWithExternal where
  run _ (TestWithFile path test) _ = test <$> readFile path
  testOptions = Tagged []
