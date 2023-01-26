module MeguKin.Parser.Types (
  Parser,
  Identifier (Identifier),
  SimpleIdentifier (SimpleIdentifier),
  TopLevelDefinition (ModuleDeclaration),
  ExportDeclaration (DataTypeExport, ClassExport),
  ParserShow (parserShow),
  genDataTypeExport,
  genClassExport,
  genModuleDeclaration,
) where

import Control.Applicative ((<*>))
import Data.Char (isLetter)
import Data.Functor ((<$>))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Void (Void)
import Prelude (Eq, Show, ($), (.))

import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Gen,
  frequency,
  listOf1,
  suchThat,
 )
import Text.Megaparsec (Parsec)

type Parser a = Parsec Void String a

class ParserShow a where
  parserShow :: a -> String

-- | Do not import the constructor! only allowed in Parsers.hs
newtype SimpleIdentifier = SimpleIdentifier String
  deriving (Eq, Show)

instance Arbitrary SimpleIdentifier where
  arbitrary = SimpleIdentifier <$> generator
   where
    generator :: Gen String
    generator = listOf1 $ suchThat arbitrary isLetter

  shrink _ = SimpleIdentifier . (: []) <$> "abcdefghijklmnopqrstuvwxyz"

instance ParserShow SimpleIdentifier where
  parserShow (SimpleIdentifier x) = x

newtype Identifier = Identifier (NonEmpty SimpleIdentifier)
  deriving (Eq, Show)

instance Arbitrary Identifier where
  arbitrary = Identifier <$> ((:|) <$> arbitrary <*> arbitrary)
  shrink (Identifier (head :| _)) =
    [Identifier (head :| [SimpleIdentifier ".arbitraryShrink"])]

instance ParserShow Identifier where
  parserShow (Identifier x) = List.intercalate "." $ parserShow <$> toList x

data TopLevelDefinition
  = ModuleDeclaration Identifier [ExportDeclaration]
  deriving (Eq, Show)

instance Arbitrary TopLevelDefinition where
  arbitrary = genModuleDeclaration

genModuleDeclaration :: Gen TopLevelDefinition
genModuleDeclaration = ModuleDeclaration <$> arbitrary <*> arbitrary

instance ParserShow TopLevelDefinition where
  parserShow (ModuleDeclaration moduleName exports) =
    "module " <> parserShow moduleName <> "("
      <> List.intercalate "," (parserShow <$> exports)
      <> ") where"

data ExportDeclaration
  = DataTypeExport SimpleIdentifier [SimpleIdentifier]
  | ClassExport SimpleIdentifier [SimpleIdentifier]
  deriving (Eq, Show)

instance Arbitrary ExportDeclaration where
  arbitrary =
    frequency
      [ (5, genDataTypeExport)
      , (5, genClassExport)
      ]

genClassExport :: Gen ExportDeclaration
genClassExport = ClassExport <$> arbitrary <*> arbitrary

genDataTypeExport :: Gen ExportDeclaration
genDataTypeExport = DataTypeExport <$> arbitrary <*> arbitrary

instance ParserShow ExportDeclaration where
  parserShow (DataTypeExport typeName exports) =
    parserShow typeName <> "("
      <> List.intercalate "," (parserShow <$> exports)
      <> ")"
  parserShow (ClassExport className exports) =
    "class "
      <> parserShow className
      <> "("
      <> List.intercalate "," (parserShow <$> exports)
      <> ")"
