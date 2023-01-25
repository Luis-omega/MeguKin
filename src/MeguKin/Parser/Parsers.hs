{-# LANGUAGE OverloadedStrings #-}

module MeguKin.Parser.Parsers (topLevel) where

import Control.Applicative (Applicative (pure), (<|>))
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Char (isLetter)
import Data.Functor (($>), (<$>))
import Data.Maybe (Maybe (Just))
import Data.Monoid (Monoid (mempty))
import Data.String (String)
import Text.Megaparsec (MonadParsec (takeWhile1P), between, satisfy, sepBy)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Prelude (($), (.), (==), (||))

import MeguKin.Parser.Types (
  ExportDeclaration (DataClassExport, DataTypeExport),
  Identifier (Identifier),
  Parser,
  SimpleIdentifier (SimpleIdentifier),
  TopLevelDefinition (ModuleDeclaration),
 )

consumeSpaces :: Parser ()
consumeSpaces = Lexer.space space1 mempty mempty

consumeNonNewLineSpaces :: Parser ()
consumeNonNewLineSpaces =
  Lexer.space
    (satisfy isSimpleSpace $> ())
    mempty
    mempty
 where
  isSimpleSpace x = x == ' ' || x == '\t'

-- lexeme :: Parser a -> Parser a
-- lexeme = Lexer.lexeme consumeNonNewLineSpaces

simpleSymbol :: String -> Parser String
simpleSymbol = Lexer.symbol consumeNonNewLineSpaces

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
-- class_ :: Parser ()
-- class_ = symbol "class"

simpleIdentifier :: Parser SimpleIdentifier
simpleIdentifier = SimpleIdentifier <$> takeWhile1P (Just "IdentifierCharacter") isLetter

longIdentifier :: Parser Identifier
longIdentifier = do
  value <- sepBy1 simpleIdentifier (simpleSymbol ".")
  pure . Identifier $ value

topLevel :: Parser TopLevelDefinition
topLevel = moduleDeclaration

moduleDeclaration :: Parser TopLevelDefinition
moduleDeclaration = do
  _ <- Lexer.nonIndented consumeSpaces module_
  moduleName <- longIdentifier
  exports <- between lparen rparen (sepBy (dataTypeExport <|> classExport) comma)
  _ <- where_
  pure $ ModuleDeclaration moduleName exports

dataTypeExport :: Parser ExportDeclaration
dataTypeExport = do
  typeName <- simpleIdentifier
  exportedValues <- between lparen rparen (sepBy simpleIdentifier comma)
  pure $ DataTypeExport typeName exportedValues

classExport :: Parser ExportDeclaration
classExport = do
  className <- simpleIdentifier
  exportedValues <- between lparen rparen (sepBy simpleIdentifier comma)
  pure $ DataClassExport className exportedValues

-- parseLet :: Parser SugaredExpression
-- parseLet = Lexer.indentBlock consumeSpaces p *> in_
--  where
--   p = do
--     _ <- let_
--     pure $ Lexer.IndentSome Nothing joinBindings parsePatternMatch
