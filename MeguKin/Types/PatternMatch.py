from typing import List


class PatternMatch:
    pass


class PatternMatchVariable(PatternMatch):
    name: str

    def __init__(self, name: str):
        self.name = name


class PatternMatchConstructor(PatternMatch):
    name: str
    patterns: List[PatternMatch]

    def __init__(self, name: str, patterns: List[PatternMatch]):
        self.name = name
        self.patterns = patterns
