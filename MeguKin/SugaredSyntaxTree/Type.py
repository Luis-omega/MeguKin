from typing import Union, TypeVar
from lark import Token

from MeguKin.File import Range, token2Range
from MeguKin.SugaredSyntaxTree.SST import (
    SST,
    compare_list,
    MetaRecord,
    MetaVar,
    MetaLiteral,
    MetaMeaninglessOperatorApplications,
)

TypeT = Union[
    "TypeVariable",
    "TypeConcreteName",
    "TypeOperator",
    "TypeRecord",
    "TypeMeaninglessOperatorApplications",
    "TypeApplication",
    "TypeArrow",
    "TypeForall",
]

T = TypeVar("T")


class Type(SST):
    pass


class TypeVariable(MetaVar, Type):
    pass


class TypeConcreteName(MetaVar, Type):
    pass


class TypeOperator(MetaVar, Type):
    pass


class TypeRecord(MetaRecord[TypeT], Type):
    @staticmethod
    def compare_items(item1: TypeT, item2: TypeT):
        return item1.compare(item2)


class TypeMeaninglessOperatorApplications(
    MetaMeaninglessOperatorApplications[TypeT, TypeOperator], Type
):
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


class TypeForall(Type):
    args: list[TypeVariable]
    expression: TypeT

    def __init__(
        self, args: list[TypeVariable], expression: TypeT, _range: Range
    ):
        self.args = args
        self.expression = expression
        self._range = _range

    def compare(self, other: SST):
        return (
            isinstance(other, TypeForall)
            and self.expression.compare(other.expression)
            and compare_list(self.args, other.args, TypeVariable.compare)
        )

    def __repr__(self):
        return f"TypeForall({self.args},{self.expression})"
