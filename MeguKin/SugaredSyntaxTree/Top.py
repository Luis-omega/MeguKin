from typing import Union

from MeguKin.File import Range
from MeguKin.SugaredSyntaxTree.Expression import Function, ExpressionT
from MeguKin.SugaredSyntaxTree.Type import TypeT
from MeguKin.SugaredSyntaxTree.SST import SST, MetaTop, compare_list

TopT = Union["Definition", "Declaration", "DataType", "Import", "Export"]


class Top(SST):
    pass


class Definition(MetaTop[ExpressionT], Top):
    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, Definition)
            and self.name == other.name
            and self.value.compare(other.value)
        )


class Declaration(MetaTop[TypeT], Top):
    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, Declaration)
            and self.name == other.name
            and self.value.compare(other.value)
        )


class ConstructorDefinition(MetaTop[list[TypeT]]):
    def compare(self, other: SST) -> bool:
        def compare_types(t1: TypeT, t2: TypeT):
            return t1.compare(t2)

        return (
            isinstance(other, ConstructorDefinition)
            and self.name == other.name
            and compare_list(self.value, other.value, compare_types)
        )


class DataType(MetaTop[list[ConstructorDefinition]]):
    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, DataType)
            and self.name == other.name
            and compare_list(
                self.value, other.value, ConstructorDefinition.compare
            )
        )


class Import:
    pass


class Export:
    pass
