from typing import Union
from lark import Token

from MeguKin.Reconstruction import Range, token2Range

PatternMatchT = Union["PatternMatchVariable", "PatternMatchConstructor"]


class PatternMatch:
    pass


class PatternMatchLiteral(PatternMatch):
    value: int
    _range: Range
    bound_variables: set[str] = set()

    def __init__(self, value: int, _range: Range):
        self.value = value
        self._range = _range

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
    name: str
    patterns: list["PatternMatchT"]
    _range: Range
    bound_variables: set[str]

    def __init__(
        self,
        name: str,
        patterns: list["PatternMatchT"],
        _range: Range,
        bound_variables: set[str],
    ):
        self.name = name
        self.patterns = patterns
        self._range = _range
        self.bound_variables = bound_variables

    def pretty(self):
        def prettify(pattern: PatternMatchT):
            if isinstance(pattern, PatternMatchConstructor):
                if len(pattern.patterns) == 0:
                    return pattern.pretty()
                else:
                    return f"({pattern.pretty()})"
            else:
                return pattern.pretty()

        args = " ".join([prettify(i) for i in self.patterns])
        return f"{self.name} {args}"

    def __str__(self):
        return f"PatternMatchConstructor({self.name},{self.patterns})"

    def __repr__(self):
        return f"PatternMatchConstructor({self.name},{self.patterns})"
