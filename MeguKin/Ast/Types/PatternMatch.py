from typing import List, Union

from MeguKin.Reconstruction import Range

PatternMatchT = Union["PatternMatchVariable", "PatternMatchConstructor"]


class PatternMatch:
    pass


class PatternMatchVariable(PatternMatch):
    name: str
    _range: Range

    def __init__(self, name: str, _range: Range):
        self.name = name
        self._range = _range

    def pretty(self):
        return f"{self.name}"

    def __str__(self):
        return f"PatternMatchVariable({self.name})"

    def __repr__(self):
        return f"PatternMatchVariable({self.name})"


# TODO : Make it a two elements constructor instead of having a list
class PatternMatchConstructor(PatternMatch):
    name: str
    patterns: List["PatternMatchT"]
    _range: Range

    def __init__(self, name: str, patterns: List["PatternMatchT"], _range: Range):
        self.name = name
        self.patterns = patterns
        self._range = _range

    def pretty(self):
        args = " ".join([f"({i.pretty()})" for i in self.patterns])
        return f"{self.name} {args}"

    def __str__(self):
        return f"PatternMatchConstructor({self.name},{self.patterns})"

    def __repr__(self):
        return f"PatternMatchConstructor({self.name},{self.patterns})"
