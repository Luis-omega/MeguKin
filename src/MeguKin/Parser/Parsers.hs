{-# LANGUAGE OverloadedStrings #-}

module MeguKin.Parser.Parsers (
  Token (
    ModuleToken,
    ImportToken,
    AsToken,
    EqualToken,
    TypeToken,
    IdentifierToken,
    LetToken,
    LeftParenToken,
    RightParenToken,
    LeftArrowToken,
    RightArrowToken,
    LambdaStartToken,
    InToken
  ),
  lexer,
) where

import Control.Applicative (Applicative (pure), (<|>))
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Char (isLetter)
import Data.Functor (($>), (<$>))
import Data.Maybe (Maybe (Just))
import Data.Monoid (Monoid (mempty))
import Data.String (String)
import Text.Megaparsec (MonadParsec (lookAhead, takeWhile1P), Parsec, satisfy)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer (lexeme, space, symbol)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Prelude (($), (==), (||))

import MeguKin.Parser.Types (
  Parser,
  Token (
    AsToken,
    EqualToken,
    IdentifierToken,
    ImportToken,
    InToken,
    LambdaStartToken,
    LeftArrowToken,
    LeftParenToken,
    LetToken,
    ModuleToken,
    RightArrowToken,
    RightParenToken,
    TypeToken
  ),
 )

consumeSpaces :: Parser ()
consumeSpaces = Text.Megaparsec.Char.Lexer.space space1 mempty mempty

consumeNonNewLineSpaces :: Parser ()
consumeNonNewLineSpaces =
  Text.Megaparsec.Char.Lexer.space
    (satisfy isSimpleSpace $> ())
    mempty
    mempty
 where
  isSimpleSpace x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = Text.Megaparsec.Char.Lexer.lexeme consumeNonNewLineSpaces

simpleSymbol :: String -> Parser String
simpleSymbol = Text.Megaparsec.Char.Lexer.symbol consumeNonNewLineSpaces

symbol :: String -> Token -> Parser Token
symbol str tok = Text.Megaparsec.Char.Lexer.symbol consumeNonNewLineSpaces str $> tok

module_ :: Parser Token
module_ = symbol "module" ModuleToken
import_ :: Parser Token
import_ = symbol "import" ImportToken
as_ :: Parser Token
as_ = symbol "as" AsToken
equalOperator :: Parser Token
equalOperator = symbol "=" EqualToken
typeOperator :: Parser Token
typeOperator = symbol "::" TypeToken
lparen :: Parser Token
lparen = symbol "(" LeftParenToken
rparen :: Parser Token
rparen = symbol ")" RightArrowToken
let_ :: Parser Token
let_ = symbol "let" LetToken
leftArrow :: Parser Token
leftArrow = symbol "<-" LeftArrowToken
rightArrow :: Parser Token
rightArrow = symbol "->" RightArrowToken
in_ :: Parser Token
in_ = symbol "in" InToken
lambdaStart :: Parser Token
lambdaStart = symbol "\\" LambdaStartToken

simpleIdentifier :: Parser String
simpleIdentifier = takeWhile1P (Just "IdentifierCharacter") isLetter

longIdentifier :: Parser Token
longIdentifier = do
  value <- sepBy1 simpleIdentifier (simpleSymbol ".")
  pure $ IdentifierToken $ Identifier $ value

data IndentationState = AtStart | AtParen | AtLet | AtEq

lexNonIndentation :: Parser [Token]
lexNonIndentation =
  pure
    <$> ( module_
            <|> import_
            <|> as_
            <|> typeOperator
            <|> lparen
            <|> rparen
            <|> leftArrow
            <|> rightArrow
            <|> lambdaStart
            <|> longIdentifier
        )

-- parseLet :: Parser SugaredExpression
-- parseLet = Lexer.indentBlock consumeSpaces p *> in_
--  where
--   p = do
--     _ <- let_
--     pure $ Lexer.IndentSome Nothing joinBindings parsePatternMatch

lexIndentation :: Parser Token -> Parser [Token]
lexIndentation p = do
  lookAhead p

lexer :: Parser [Token]
lexer = do
  consumeSpaces
  value <- lexNonIndentation
  pure value
