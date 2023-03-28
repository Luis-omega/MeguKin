from typing import List, Union
from MeguKin.Ast.Types.Expression import ExpressionT
from MeguKin.Ast.Types.Type import TypeT, Type

TopT = Union["Definition", "Declaration", "DefinitionAndDeclaration", "DataType"]


class Top:
    def pretty(self):
        raise "Not implemented"


class Definition(Top):
    name: str
    expression: ExpressionT

    def __init__(self, name: str, expression: ExpressionT):
        self.name = name
        self.expression = expression

    def pretty(self):
        return f"{self.name} = {{{self.expression.pretty()}}}"

    def __str__(self):
        return f"Definition({self.name},{self.expression})"

    def __repr__(self):
        return f"Definition({self.name},{self.expression})"


class Declaration(Top):
    name: str
    _type: TypeT

    def __init__(self, name: str, _type: TypeT):
        self.name = name
        self._type = _type

    def pretty(self):
        return f"{self.name} : ({self._type.pretty()})"

    def __str__(self):
        return f"Declaration({self.name},{self._type})"

    def __repr__(self):
        return f"Declaration({self.name},{self._type})"


class DefinitionAndDeclaration(Top):
    definition: Definition
    declaration: Declaration

    def __init__(self, definition: Definition, declaration: Declaration):
        self.definition = definition
        self.declaration = declaration

    def pretty(self):
        return f"{self.name} : ({self._type.pretty()})"

    def __str__(self):
        return f"Declaration({self.name},{self._type})"

    def __repr__(self):
        return f"Declaration({self.name},{self._type})"


class Constructor(Type):
    name: str
    types: List[TypeT]

    def __init__(self, name: str, types: List[TypeT]):
        self.name = name
        self.types = types

    def pretty(self):
        args = " ".join([f"({i.pretty()})" for i in self.types])
        return f"{self.name} {args}"

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

    def pretty(self):
        args = " | ".join([f"{i.pretty()}" for i in self.constructors])
        return f"data {self.name} = {args}"

    def __str__(self):
        return f"DataType({self.name},{self.constructors})"

    def __repr__(self):
        return f"DataType({self.name},{self.constructors})"
