module MeguKin.Parser.Types (
  Parser,
  Identifier (Identifier),
  SimpleIdentifier (SimpleIdentifier),
  TopLevelDefinition (ModuleDeclaration),
  ExportDeclaration (DataTypeExport, DataClassExport),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.String (String)
import Text.Megaparsec (Parsec)

type Parser a = Parsec String String a

-- | Do import the constructor! only allowed in Parsers.hs
newtype SimpleIdentifier = SimpleIdentifier String

newtype Identifier = Identifier (NonEmpty SimpleIdentifier)

data TopLevelDefinition
  = ModuleDeclaration Identifier [ExportDeclaration]

data ExportDeclaration
  = DataTypeExport SimpleIdentifier [SimpleIdentifier]
  | DataClassExport SimpleIdentifier [SimpleIdentifier]
