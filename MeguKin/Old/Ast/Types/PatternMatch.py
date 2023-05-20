from typing import Union
from lark import Token

from MeguKin.Reconstruction import Range, token2Range, mergeRanges

PatternMatchT = Union[
    "PatternMatchVariable",
    "PatternMatchConstructor",
    "PatternMatchLiteral",
    "PatternMatchConstructorName",
]


class PatternMatch:
    pass


class PatternMatchLiteral(PatternMatch):
    value: int
    _range: Range
    bound_variables: set[str] = set()

    def __init__(self, value: Token):
        self.value = int(value.value)
        self._range = token2Range(value)

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return f"PatternMatchLiteral({repr(self.value)},{repr(self._range)})"


class PatternMatchVariable(PatternMatch):
    name: str
    _range: Range
    bound_variables: set[str]

    def __init__(self, name: Token):
        self.name = name.value
        self._range = token2Range(name)
        self.bound_variables = set(name.value)

    def __str__(self):
        return f"{self.name}"

    def __repr__(self):
        return f"PatternMatchVariable({repr(self.name)})"


class PatternMatchConstructor(PatternMatch):
    name: "PatternMatchConstructorName"
    patterns: list["PatternMatchT"]
    _range: Range
    bound_variables: set[str]

    def __init__(
        self,
        name: "PatternMatchConstructorName",
        patterns: list["PatternMatchT"],
    ):
        self.name = name
        self.patterns = patterns
        self._range = mergeRanges(name._range, patterns[-1]._range)
        self.bound_variables = set().union(*(p.bound_variables for p in patterns))

    def __str__(self):
        return f"PatternMatchConstructor({self.name},{self.patterns})"

    def __repr__(self):
        return f"PatternMatchConstructor({self.name},{self.patterns})"


class PatternMatchConstructorName(PatternMatch):
    prefix: list[str]
    name: str
    _range: Range
    bound_variables: set[str] = set()

    def __init__(self, prefix: list[str], name: str, _range: Range) -> None:
        self.prefix = prefix
        # shall we add `assert name[0].isupper()` here?
        self.name = name
        self._range = _range

    @staticmethod
    def from_lark_token(token: Token) -> "PatternMatchConstructorName":
        _range = token2Range(token)
        splited = token.value.split(".")
        name = splited[-1]
        prefix = splited[:-1]
        return PatternMatchConstructorName(prefix, name, _range)

    def __str__(self):
        prefix = ".".join(self.prefix)
        return f"{prefix}.{self.name}"

    def __repr__(self):
        return f"PatternMatchConstructorName({self.prefix},{self.name},{self._range},{self.free_variables})"
