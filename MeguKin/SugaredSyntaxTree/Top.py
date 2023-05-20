from typing import Union


from MeguKin.SugaredSyntaxTree.Range import Range
from MeguKin.SugaredSyntaxTree.Expression import ExpressionT
from MeguKin.SugaredSyntaxTree.Type import TypeT

TopT = Union["Definition", "Declaration", "DataType", "Import", "Export"]


class Top:
    def pretty(self):
        raise "Not implemented"


class Definition(Top):
    name: str
    expression: ExpressionT
    _range: Range

    def __init__(self, name: str, expression: ExpressionT, _range: Range):
        self.name = name
        self.expression = expression
        self._range = _range

    def __str__(self):
        return f"Definition({self.name},{self.expression})"

    def __repr__(self):
        return f"Definition({self.name},{self.expression})"


class Declaration(Top):
    name: str
    _type: TypeT
    _range: Range

    def __init__(self, name: str, _type: TypeT, _range: Range):
        self.name = name
        self._type = _type
        self._range = _range

    def pretty(self):
        return f"{self.name} : ({self._type.pretty()})"

    def __str__(self):
        return repr(self)

    def __repr__(self):
        return f"Declaration({self.name},{self._type})"


class ConstructorDefinition:
    name: str
    types: list[TypeT]
    _range: Range

    def __init__(self, name: str, types: list[TypeT], _range: Range):
        self.name = name
        self.types = types
        self._range = _range

    def __str__(self):
        return f"ConstructorDefinition({self.name},{self.types})"

    def __repr__(self):
        return f"ConstructorDefinition({self.name},{self.types})"


class DataType:
    name: str
    constructors: list[ConstructorDefinition]
    _range: Range

    def __init__(
        self, name: str, constructors: list[ConstructorDefinition], _range: Range
    ):
        self.name = name
        self.constructors = constructors
        self._range = _range

    def __str__(self):
        return f"DataType({self.name},{self.constructors})"

    def __repr__(self):
        return f"DataType({self.name},{self.constructors})"


class Import:
    pass


class Export:
    pass
