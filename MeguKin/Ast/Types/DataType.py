from MeguKin.Reconstruction import mergeRanges, token2Range, Range
from MeguKin.Ast.Types.Type import TypeT


class Constructor:
    name: str
    types: list[TypeT]
    _range: Range

    def __init__(self, name: str, types: list[TypeT], _range: Range):
        self.name = name
        self.types = types
        self._range = _range

    def pretty(self):
        args = " , ".join([f"{i.pretty()}" for i in self.types])
        return f"{self.name} {args}"

    def __str__(self):
        return f"Constructor({self.name},{self.types})"

    def __repr__(self):
        return f"Constructor({self.name},{self.types})"


class DataType:
    name: str
    constructors: list[Constructor]
    _range: Range

    def __init__(self, name: str, constructors: list[Constructor], _range: Range):
        self.name = name
        self.constructors = constructors
        self._range = _range

    def pretty(self):
        args = " | ".join([f"{i.pretty()}" for i in self.constructors])
        return f"data {self.name} = ({args})"

    def __str__(self):
        return f"DataType({self.name},{self.constructors})"

    def __repr__(self):
        return f"DataType({self.name},{self.constructors})"
