from typing import List, Union


from MeguKin.Reconstruction import Range
from MeguKin.Ast.Types.Expression import ExpressionT
from MeguKin.Ast.Types.Type import TypeT, Type

TopT = Union["Definition", "Declaration", "DefinitionAndDeclaration", "DataType"]


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

    def pretty(self):
        return f"{self.name} = ({self.expression.pretty()})"

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


class DefinitionAndDeclaration(Top):
    definition: Definition
    declaration: Declaration
    _range: Range

    def __init__(self, definition: Definition, declaration: Declaration, _range: Range):
        self.definition = definition
        self.declaration = declaration
        self._range = _range

    def pretty(self):
        return f"{self.name} : ({self._type.pretty()})"

    def __str__(self):
        return f"Declaration({self.name},{self._type})"

    def __repr__(self):
        return f"Declaration({self.name},{self._type})"
