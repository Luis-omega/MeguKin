{-# LANGUAGE FlexibleInstances #-}

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
import Prelude (Eq, Show, String, show, (==))

import Test.QuickCheck (Arbitrary (arbitrary), Gen)
import Test.QuickCheck.Property (Result (reason), failed, succeeded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Megaparsec (errorBundlePretty, parse)

tests :: TestTree
tests =
  testGroup
    "Parser"
    [ arbitraryTest
    ]

arbitraryTest :: TestTree
arbitraryTest =
  testGroup
    "Arbitrary"
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
  String ->
  Parser a ->
  TestTree
makeRoundTripTest name parser = makeRoundTripTestWithGen name parser arbitrary

makeRoundTripTestWithGen ::
  forall a.
  Eq a =>
  ParserShow a =>
  Show a =>
  String ->
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
  Result
testParserWithValue parser value =
  let stringValue = parserShow value
   in case parse parser "test" stringValue of
        Right x ->
          if x == value
            then succeeded
            else failed{reason = "RoundTripFailed parsedValue: " <> show x}
        Left parserError ->
          let (showedError :: String) = errorBundlePretty parserError
           in failed{reason = showedError}
