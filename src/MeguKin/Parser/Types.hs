module MeguKin.Parser.Types (
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
  Parser,
  Identifier (Identifier),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.String (String)
import Text.Megaparsec (Parsec)

type Parser a = Parsec String String a

newtype Identifier = Identifier (NonEmpty String)

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
  | IdentifierToken Identifier
