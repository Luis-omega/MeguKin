from typing import Union
from lark import Token

from MeguKin.File import Range, token2Range
from MeguKin.SugaredSyntaxTree.SST import (
    SST,
    compare_list,
    MetaVar,
    MetaLiteral,
)

TypeT = Union[
    "TypeArrow", "TypeApplication", "TypeVariable", "TypeConcreteName"
]


class Type(SST):
    pass


class TypeVariable(MetaVar, Type):
    pass


class TypeConcreteName(MetaVar, Type):
    pass


class TypeApplication(Type):
    function: Type
    argument: Type

    def __init__(self, function: Type, argument: Type, _range: Range):
        self.function = function
        self.argument = argument
        self._range = _range

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, TypeApplication)
            and self.function.compare(other.function)
            and self.argument.compare(other.argument)
        )

    def __repr__(self):
        return f"TypeApplication({self.function},{self.argument})"


class TypeArrow(Type):
    domain: Type
    codomain: Type

    def __init__(self, domain: Type, codomain: Type, _range: Range):
        self.domain = domain
        self.codomain = codomain
        self._range = _range

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, TypeArrow)
            and self.domain.compare(other.domain)
            and self.codomain.compare(other.codomain)
        )

    def __repr__(self):
        return f"TypeArrow({self.domain}, {self.codomain})"
