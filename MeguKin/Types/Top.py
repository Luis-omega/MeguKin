from typing import List
from MeguKin.Types.Expression import Expression
from MeguKin.Types.Type import Type


class Top:
    pass


class Definition(Top):
    name: str
    expression: Expression

    def __init__(self, name: str, expression: Expression):
        self.name = name
        self.expression = expression

    def __str__(self):
        return f"Definition({self.name},{self.expression})"

    def __repr__(self):
        return f"Definition({self.name},{self.expression})"


class Declaration(Top):
    name: str
    _type: Type

    def __init__(self, name: str, _type: Type):
        self.name = name
        self._type = _type

    def __str__(self):
        return f"Declaration({self.name},{self._type})"

    def __repr__(self):
        return f"Declaration({self.name},{self._type})"


class Constructor(Type):
    name: str
    types: List[Type]

    def __init__(self, name: str, types: List[Type]):
        self.name = name
        self.types = types

    def __str__(self):
        return f"Constructor({self.name},{self.types})"

    def __repr__(self):
        return f"Constructor({self.name},{self.types})"


class DataType(Top):
    name: str
    constructors: List[Constructor]

    def __init__(self, name: str, constructors: List[Constructor]):
        self.name = name
        self.constructors = constructors

    def __str__(self):
        return f"DataType({self.name},{self.constructors})"

    def __repr__(self):
        return f"DataType({self.name},{self.constructors})"
