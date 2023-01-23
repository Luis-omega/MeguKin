{-# LANGUAGE OverloadedStrings #-}

module MeguKin.Lexer.Types (
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

import MeguKin.Stream.Types (HasPosition, HasRange, Position (Position), Range (Range), getPosition)

import Control.Applicative (Applicative (pure), (<|>))
import Data.Char (isLetter)
import Data.Functor ((<$>))
import Data.Maybe (Maybe (Just))
import Data.Monoid (Monoid (mempty))
import Data.String (String)
import Text.Megaparsec (MonadParsec (lookAhead, takeWhile1P), Parsec, SourcePos (SourcePos, sourceColumn, sourceLine), getOffset, getSourcePos, sepBy1, unPos)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer (lexeme, space, symbol)
import Prelude (Int)

type Parser a = Parsec String String a

data LexerState = LexerState
  { lexerPosition :: Position
  , lexerFile :: String
  }

instance HasPosition LexerState where
  getPosition LexerState{..} = lexerPosition

data Token
  = ModuleToken
  | ImportToken
  | AsToken
  | EqualToken
  | TypeToken
  | LetToken
  | LeftParenToken
  | RightParenToken
  | LeftArrowToken
  | RightArrowToken
  | InToken
  | LambdaStartToken
  | IdentifierToken [String]

consumeSpaces :: Parser ()
consumeSpaces = Text.Megaparsec.Char.Lexer.space space1 mempty mempty

lexeme :: Parser a -> Parser a
lexeme = Text.Megaparsec.Char.Lexer.lexeme consumeSpaces

megaParsecPosition2Position :: Int -> SourcePos -> Position
megaParsecPosition2Position abs SourcePos{..} =
  Position (unPos sourceColumn) (unPos sourceLine) abs

simpleSymbol :: String -> Parser String
simpleSymbol = Text.Megaparsec.Char.Lexer.symbol consumeSpaces

symbol :: String -> Token -> Parser (Range, Token)
symbol str tok = do
  offsetStart <- getOffset
  start <- getSourcePos
  _ <- Text.Megaparsec.Char.Lexer.symbol consumeSpaces str
  end <- getSourcePos
  offsetEnd <- getOffset
  pure
    ( Range
        (megaParsecPosition2Position offsetStart start)
        (megaParsecPosition2Position offsetEnd end)
    , tok
    )

module_ :: Parser (Range, Token)
module_ = symbol "module" ModuleToken
import_ :: Parser (Range, Token)
import_ = symbol "import" ImportToken
as :: Parser (Range, Token)
as = symbol "as" AsToken
equalOperator :: Parser (Range, Token)
equalOperator = symbol "=" EqualToken
typeOperator :: Parser (Range, Token)
typeOperator = symbol "::" TypeToken
lparen :: Parser (Range, Token)
lparen = symbol "(" LeftParenToken
rparen :: Parser (Range, Token)
rparen = symbol ")" RightArrowToken
let_ :: Parser (Range, Token)
let_ = symbol "let" LetToken
leftArrow :: Parser (Range, Token)
leftArrow = symbol "<-" LeftArrowToken
rightArrow :: Parser (Range, Token)
rightArrow = symbol "->" RightArrowToken
in_ :: Parser (Range, Token)
in_ = symbol "in" InToken
lambdaStart :: Parser (Range, Token)
lambdaStart = symbol "\\" LambdaStartToken

simpleIdentifier :: Parser String
simpleIdentifier = takeWhile1P (Just "IdentifierCharacter") isLetter

longIdentifier :: Parser (Range, Token)
longIdentifier = do
  offsetStart <- getOffset
  start <- getSourcePos
  value <- sepBy1 simpleIdentifier (simpleSymbol ".")
  end <- getSourcePos
  offsetEnd <- getOffset
  pure
    ( Range
        (megaParsecPosition2Position offsetStart start)
        (megaParsecPosition2Position offsetEnd end)
    , IdentifierToken value
    )

data IndentationState = AtStart | AtParen | AtLet | AtEq

lexNonIndentation :: Parser [(Range, Token)]
lexNonIndentation =
  pure
    <$> ( module_
            <|> import_
            <|> as
            <|> typeOperator
            <|> lparen
            <|> rparen
            <|> leftArrow
            <|> rightArrow
            <|> lambdaStart
            <|> longIdentifier
        )

lexIndentation :: Parser Token -> Parser [(Range, Token)]
lexIndentation p = do
  lookAhead p

lexer :: Parser [(Range, Token)]
lexer = do
  consumeSpaces
  value <- lexNonIndentation
  pure value
