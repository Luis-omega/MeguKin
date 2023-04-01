from typing import List, Union, Set

from MeguKin.Reconstruction import Range

PatternMatchT = Union["PatternMatchVariable", "PatternMatchConstructor"]


class PatternMatch:
    pass


class PatternMatchVariable(PatternMatch):
    name: str
    _range: Range
    bound_variables: Set[str]

    def __init__(self, name: str, _range: Range, bound_variables: Set[str]):
        self.name = name
        self._range = _range
        self.bound_variables = bound_variables

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
    bound_variables: Set[str]

    def __init__(
        self,
        name: str,
        patterns: List["PatternMatchT"],
        _range: Range,
        bound_variables: Set[str],
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
