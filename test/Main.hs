module Main (main) where

import qualified Test.MeguKin.Parser as Parser

import Control.Monad ((=<<))
import Data.Functor ((<$>))
import Data.Traversable (sequence)
import System.IO (IO ())

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = testGroup "Tests" <$> sequence [Parser.tests]
