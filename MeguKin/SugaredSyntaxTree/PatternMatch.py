from typing import Union
from MeguKin.File import mergeRanges
from MeguKin.SugaredSyntaxTree.SST import (
    SST,
    compare_list,
    MetaVar,
    MetaLiteral,
)

from MeguKin.Pretty import (
    Nil,
    parens,
    DocumentT,
    DocumentSettings,
    Text,
    LineBreak,
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


class PatternMatchVariable(PatternMatch, MetaVar):
    pass


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

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        doc: DocumentT = Nil()
        for pattern in self.patterns[::-1]:
            new_doc = pattern.to_document(settings)
            doc = new_doc + LineBreak() + doc
        return parens(
            settings, self.name.to_document(settings) + Text(" ") + doc
        )

    def __repr__(self):
        return f"PatternMatchConstructor({self.name},{self.patterns})"


class PatternMatchConstructorName(MetaVar, PatternMatch):
    pass


def compare_patterns(p1: PatternMatchT, p2: PatternMatchT) -> bool:
    return p1.compare(p2)
