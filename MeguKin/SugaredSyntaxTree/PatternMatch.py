from typing import Union
from lark import Token

from MeguKin.File import token2Range, mergeRanges
from MeguKin.SugaredSyntaxTree.SST import (
    SST,
    compare_list,
    MetaVar,
    MetaLiteral,
)

PatternMatchT = Union[
    "PatternMatchVariable",
    "PatternMatchConstructor",
    "PatternMatchLiteral",
    "PatternMatchConstructorName",
]


class PatternMatch(SST):
    pass


class PatternMatchLiteral(MetaLiteral, PatternMatch):
    pass


class PatternMatchVariable(PatternMatch):
    name: str

    def __init__(self, name: Token):
        self.name = name.value
        self._range = token2Range(name)

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, PatternMatchVariable) and self.name == other.name
        )

    def __repr__(self):
        return f"PatternMatchVariable({repr(self.name)})"


class PatternMatchConstructor(PatternMatch):
    name: "PatternMatchConstructorName"
    patterns: list["PatternMatchT"]

    def __init__(
        self,
        name: "PatternMatchConstructorName",
        patterns: list["PatternMatchT"],
    ):
        self.name = name
        self.patterns = patterns
        self._range = mergeRanges(name._range, patterns[-1]._range)

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, PatternMatchConstructor)
            and self.name == other.name
            and compare_list(self.patterns, other.patterns, compare_patterns)
        )

    def __repr__(self):
        return f"PatternMatchConstructor({self.name},{self.patterns})"


class PatternMatchConstructorName(MetaVar, PatternMatch):
    pass


def compare_patterns(p1: PatternMatchT, p2: PatternMatchT) -> bool:
    return p1.compare(p2)
