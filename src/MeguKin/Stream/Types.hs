module MeguKin.Stream.Types (Position(Position), Range(Range), getPosition, getRange, HasPosition, HasRange) where

import Prelude (Int, Show, id)

data Position = Position {column :: Int, line :: Int, absolutePosition :: Int} deriving (Show)

data Range = Range {startPosition :: Position, endPosition :: Position} deriving (Show)

class HasPosition a where
  getPosition :: a -> Position

class HasRange a where
  getRange :: a -> Range

instance HasPosition Position where
  getPosition = id

instance HasRange Range where
  getRange = id
