from dataclasses import dataclass
from typing import Union

from MeguKin.Parser.Token import Token
from MeguKin.Compiler.ConcreteSyntaxTree.CST import CST
from MeguKin.Pretty import Text
from MeguKin.Comparable import compare_list
import MeguKin.Comparable as Comparable


PatternT = Union[
    "Literal", "Variable", "ConstructorName", "Hole", "Constructor"
]


@dataclass
class Literal(CST):
    token: Token

    def compare(self, other: Comparable.T) -> bool:
        return isinstance(other, type(self)) and self.token == other.token

    def to_document(self) -> Text:
        return Text(str(self.token.value))


@dataclass
class Variable(CST):
    value: int

    def compare(self, other: Comparable.T) -> bool:
        return self.value == other

    def to_document(self) -> Text:
        return Text(str(self.value))


@dataclass
class ConstructorName(CST):
    value: int

    def compare(self, other: Comparable.T) -> bool:
        return self.value == other

    def to_document(self) -> Text:
        return Text(str(self.value))


@dataclass
class Hole(CST):
    def compare(self, other: Comparable.T) -> bool:
        return type(other) == type(self)

    def to_document(self) -> Text:
        return Text("_")


@dataclass
class Constructor(CST):
    name: ConstructorName
    patterns: list[PatternT]

    def compare(self, other: Comparable.T) -> bool:
        return (
            isinstance(other, type(self))  # mypy complains otherwise
            and type(self) == type(other)
            and self.name == other.name
            and compare_list(self.patterns, other.patterns)
        )

    def to_document(self) -> Text:
        return Text("_")
