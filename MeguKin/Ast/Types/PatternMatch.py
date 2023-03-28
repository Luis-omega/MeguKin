from typing import List


class PatternMatch:
    pass


class PatternMatchVariable(PatternMatch):
    name: str

    def __init__(self, name: str):
        self.name = name

    def pretty(self):
        return f"{self.name}"

    def __str__(self):
        return f"PatternMatchVariable({self.name})"

    def __repr__(self):
        return f"PatternMatchVariable({self.name})"


# TODO : Make it a two elements constructor instead of having a list
class PatternMatchConstructor(PatternMatch):
    name: str
    patterns: List[PatternMatch]

    def __init__(self, name: str, patterns: List[PatternMatch]):
        self.name = name
        self.patterns = patterns

    def pretty(self):
        args = " ".join([f"({i.pretty()})" for i in self.patterns])
        return f"{self.name} {args}"

    def __str__(self):
        return f"PatternMatchConstructor({self.name},{self.patterns})"

    def __repr__(self):
        return f"PatternMatchConstructor({self.name},{self.patterns})"
