{-# LANGUAGE OverloadedStrings #-}

module MeguKin.Parser.Parsers (
  topLevel,
  simpleIdentifier,
  longIdentifier,
  moduleDeclaration,
  dataTypeExport,
  classExport,
) where

import Control.Applicative (Alternative (empty), Applicative (pure), (<|>))
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Bool (Bool)
import Data.Char (Char, isLetter)
import Data.Functor (($>), (<$>))
import Data.Maybe (Maybe (Just))
import Data.String (String)
import Text.Megaparsec (MonadParsec (takeWhile1P), between, sepBy)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Prelude (($), (==), (||))

import MeguKin.Parser.Types (
  ExportDeclaration (ClassExport, DataTypeExport),
  Identifier (Identifier),
  Parser,
  SimpleIdentifier (SimpleIdentifier),
  TopLevelDefinition (ModuleDeclaration),
 )

consumeSpaces :: Parser ()
consumeSpaces = Lexer.space space1 empty empty

consumeNonNewLineSpaces :: Parser ()
consumeNonNewLineSpaces =
  Lexer.space
    (takeWhile1P (Just "non new line space") isSimpleSpace $> ())
    empty
    empty
 where
  isSimpleSpace :: Char -> Bool
  isSimpleSpace x = x == ' ' || x == '\t'

-- lexeme :: Parser a -> Parser a
-- lexeme = Lexer.lexeme consumeNonNewLineSpaces

symbol :: String -> Parser ()
symbol str = Lexer.symbol consumeNonNewLineSpaces str $> ()

module_ :: Parser ()
module_ = symbol "module"
where_ :: Parser ()
where_ = symbol "where"
lparen :: Parser ()
lparen = symbol "("
rparen :: Parser ()
rparen = symbol ")"
comma :: Parser ()
comma = symbol ","
dot :: Parser ()
dot = symbol "."
class_ :: Parser ()
class_ = symbol "class"

-- import_ :: Parser ()
-- import_ = symbol "import"
-- as_ :: Parser ()
-- as_ = symbol "as"
-- equalOperator :: Parser ()
-- equalOperator = symbol "="
-- typeOperator :: Parser ()
-- typeOperator = symbol "::"
-- let_ :: Parser ()
-- let_ = symbol "let"
-- leftArrow :: Parser ()
-- leftArrow = symbol "<-"
-- rightArrow :: Parser ()
-- rightArrow = symbol "->"
-- in_ :: Parser ()
-- in_ = symbol "in"
-- lambdaStart :: Parser ()
-- lambdaStart = symbol "\\"

simpleIdentifier :: Parser SimpleIdentifier
simpleIdentifier = SimpleIdentifier <$> takeWhile1P (Just "IdentifierCharacter") isLetter

longIdentifier :: Parser Identifier
longIdentifier = Identifier <$> sepBy1 simpleIdentifier dot

topLevel :: Parser TopLevelDefinition
topLevel = moduleDeclaration

moduleDeclaration :: Parser TopLevelDefinition
moduleDeclaration = do
  _ <- Lexer.nonIndented consumeSpaces module_
  moduleName <- longIdentifier
  exports <- between lparen rparen (sepBy (classExport <|> dataTypeExport) comma)
  _ <- where_
  pure $ ModuleDeclaration moduleName exports

dataTypeExport :: Parser ExportDeclaration
dataTypeExport = do
  typeName <- simpleIdentifier
  exportedValues <- between lparen rparen (sepBy simpleIdentifier comma)
  pure $ DataTypeExport typeName exportedValues

classExport :: Parser ExportDeclaration
classExport = do
  _ <- class_
  className <- simpleIdentifier
  exportedValues <- between lparen rparen (sepBy simpleIdentifier comma)
  pure $ ClassExport className exportedValues

-- parseLet :: Parser SugaredExpression
-- parseLet = Lexer.indentBlock consumeSpaces p *> in_
--  where
--   p = do
--     _ <- let_
--     pure $ Lexer.IndentSome Nothing joinBindings parsePatternMatch
