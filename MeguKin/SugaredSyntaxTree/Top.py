from typing import Union, Optional, TypeVar
from dataclasses import dataclass

from MeguKin.SugaredSyntaxTree.Expression import (
    ExpressionT,
    Function,
    Operator,
    Let,
)
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
    Indent,
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
                elif isinstance(expression, Let):
                    end_expression = AlwaysLineBreak() + indent(
                        expression.to_document(settings)
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
                return name + maybe_indent(
                    Text("=")
                    + LineBreak()
                    + parens(settings, self.value.to_document(settings))
                )
            case Let():
                return (
                    name
                    + indent(Text("="))
                    + Indent(
                        1, AlwaysLineBreak() + self.value.to_document(settings)
                    )
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
            Text(":")
            + Group(Indent(1, LineBreak() + self.value.to_document(settings)))
        )


class ConstructorDefinition(MetaTop[list[TypeT]]):
    def compare_value(self, value2: list[TypeT]) -> bool:
        def compare_types(t1: TypeT, t2: TypeT):
            return t1.compare(t2)

        return compare_list(self.value, value2, compare_types)

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        doc: DocumentT = Nil()
        for type_ in self.value:
            new_doc = type_.to_document(settings)
            doc = doc + LineBreak() + new_doc
        return (
            Text(self.name) + LineBreak() + parens(settings, maybe_indent(doc))
        )


class DataType(MetaTop[list[ConstructorDefinition]]):
    def compare_value(self, value2: list[ConstructorDefinition]) -> bool:
        return compare_list(self.value, value2, ConstructorDefinition.compare)

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        doc: DocumentT = Nil()
        for constructor in self.value:
            new_doc = constructor.to_document(settings)
            doc = doc + LineBreak() + Text("| ") + new_doc
        return (
            Text("data ")
            + Text(self.name)
            + LineBreak()
            + Text("=")
            + LineBreak()
            + parens(settings, maybe_indent(doc))
        )


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

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        doc: DocumentT = (
            Text("module ")
            + self.module_name.to_document(settings)
            + Text("( ")
        )
        new_doc: DocumentT = Nil()
        for export in self.exports:
            new_doc = new_doc + LineBreak() + export.to_document(settings)
        doc = doc + Indent(1, new_doc) + LineBreak() + Text(")")
        return doc

    def __repr__(self):
        return f"Exports{self.module_name},{self.exports})"


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
    # The name is inside Exports
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

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        doc = (
            self.exports.to_document(settings)
            + AlwaysLineBreak()
            # + self.imports: list[ImportModule]
            + list_to_document(self.data_types, settings)
            + list_to_document(self.declarations, settings)
            + list_to_document(self.definitions, settings)
        )
        return doc

    # FIXME:
    def __rep__(self):
        return "Module"


def list_to_document(lst: list[SST], settings: DocumentSettings) -> DocumentT:
    doc: DocumentT = Nil()
    for item in lst:
        print(item)
        doc = doc + AlwaysLineBreak() + item.to_document(settings)
    return doc

    # exports: Exports
    # imports: list[ImportModule]
    # data_types: list[DataType]
    # declarations: list[Declaration]
    # definitions: list[Definition]
