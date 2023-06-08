from typing import Union, Optional, TypeVar
from dataclasses import dataclass

from MeguKin.SugaredSyntaxTree.Expression import ExpressionT, Function, Operator
from MeguKin.SugaredSyntaxTree.Type import TypeT
from MeguKin.SugaredSyntaxTree.SST import SST, MetaTop, compare_list
from MeguKin.SugaredSyntaxTree.Module import (
    ExportNameT,
    ExportModuleName,
    ImportModule,
)
from MeguKin.Pretty import (
    Text,
    DocumentT,
    Group,
    LineBreak,
    NoSpaceLineBreak,
    parens,
    Nil,
    DocumentSettings,
    maybe_indent,
    indent,
    AlwaysLineBreak,
)


TopT = Union["Definition", "Declaration", "DataType", "Module", "Exports"]

T = TypeVar("T")


class Top(SST):
    pass


class Definition(MetaTop[ExpressionT], Top):
    def compare_value(self, value2: ExpressionT) -> bool:
        return self.value.compare(value2)

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        name = Text(self.name)
        # print("PRETTIE : ", self)
        match self.value:
            case Function(patterns=patterns, expression=expression):
                doc: DocumentT = Nil()
                for pattern in patterns[::-1]:
                    new_doc = pattern.to_document(settings)
                    doc = new_doc + LineBreak() + doc
                end_expression: DocumentT
                if isinstance(expression, Operator):
                    end_expression = parens(
                        settings, expression.to_document(settings)
                    )
                else:
                    end_expression = expression.to_document(settings)
                return name + maybe_indent(
                    Group(doc)
                    + maybe_indent(
                        Text("=") + LineBreak() + end_expression,
                    ),
                )
            case Operator():
                # print("PRETTYE es operador")
                return name + maybe_indent(
                    Text("=")
                    + LineBreak()
                    + parens(settings, self.value.to_document(settings))
                )

            case _:
                return name + maybe_indent(
                    Text("=") + LineBreak() + self.value.to_document(settings)
                )


class Declaration(MetaTop[TypeT], Top):
    def compare_value(self, value2: TypeT) -> bool:
        return self.value.compare(value2)

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        return Text(self.name) + maybe_indent(
            Text(":") + LineBreak() + self.value.to_document(settings)
        )


class ConstructorDefinition(MetaTop[list[TypeT]]):
    def compare_value(self, value2: list[TypeT]) -> bool:
        def compare_types(t1: TypeT, t2: TypeT):
            return t1.compare(t2)

        return compare_list(self.value, value2, compare_types)


class DataType(MetaTop[list[ConstructorDefinition]]):
    def compare_value(self, value2: list[ConstructorDefinition]) -> bool:
        return compare_list(self.value, value2, ConstructorDefinition.compare)


@dataclass
class Exports(Top):
    module_name: ExportModuleName
    exports: list[ExportNameT]

    def compare(self, other: SST) -> bool:
        def compare_items(e1: ExportNameT, e2: ExportNameT) -> bool:
            return e1.compare(e2)

        return (
            isinstance(other, Exports)
            and self.module_name.compare(other.module_name)
            and compare_list(self.exports, other.exports, compare_items)
        )


# @dataclass
# class Imports(Top):
#    imports: list[ImportModule]
#
#    def compare(self, other: SST) -> bool:
#        def compare_items(i1: ImportModule, i2: ImportModule) -> bool:
#            return i1.compare(i2)
#
#        return isinstance(other, Imports) and compare_list(
#            self.imports, other.imports, compare_items
#        )


@dataclass
class Module(Top):
    exports: Exports
    imports: list[ImportModule]
    data_types: list[DataType]
    declarations: list[Declaration]
    definitions: list[Definition]

    def compare(self, other: SST) -> bool:
        def compare_imports(i1: ImportModule, i2: ImportModule) -> bool:
            return i1.compare(i2)

        return (
            isinstance(other, Module)
            and self.exports.compare(other.exports)
            and compare_list(self.imports, other.imports, compare_imports)
            and compare_list(
                self.data_types, other.data_types, DataType.compare
            )
            and compare_list(
                self.declarations, other.declarations, Declaration.compare
            )
            and compare_list(
                self.definitions, other.definitions, Definition.compare
            )
        )
