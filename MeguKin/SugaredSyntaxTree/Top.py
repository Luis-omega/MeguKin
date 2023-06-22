from typing import Union, Optional, TypeVar
from dataclasses import dataclass

from MeguKin.File import Range
from MeguKin.SugaredSyntaxTree.Expression import (
    ExpressionT,
    Function,
    Operator,
    Let,
    ConstructorName,
)
from MeguKin.SugaredSyntaxTree.Type import TypeT, TypeVariable, TypeApplication
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
    indent,
    AlwaysLineBreak,
    Indent,
    list_to_document_with,
)


TopT = Union["Definition", "Declaration", "DataType", "ParsedModule", "Exports"]

T = TypeVar("T")

T_SST = TypeVar("T_SST", bound=SST)


class Top(SST):
    pass


class Definition(MetaTop[ExpressionT], Top):
    def compare_value(self, value2: ExpressionT) -> bool:
        return self.value.compare(value2)

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        name = Text(self.name)
        match self.value:
            case Function(patterns=patterns, expression=expression):
                doc = list_to_document_with(patterns, LineBreak(), settings)
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
                return name + Group(
                    Indent(
                        1,
                        Group(LineBreak() + doc)
                        + Group(
                            Indent(
                                1,
                                Text("=") + LineBreak() + end_expression,
                            )
                        ),
                    ),
                )
            case Operator():
                return name + Group(
                    Indent(
                        1,
                        Text("=")
                        + LineBreak()
                        + parens(settings, self.value.to_document(settings)),
                    )
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
                return name + Group(
                    Indent(
                        1,
                        Text("=")
                        + LineBreak()
                        + self.value.to_document(settings),
                    )
                )


class Declaration(MetaTop[TypeT], Top):
    def compare_value(self, value2: TypeT) -> bool:
        return self.value.compare(value2)

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        return Text(self.name) + Group(
            Indent(
                1,
                Text(":")
                + Group(
                    Indent(1, LineBreak() + self.value.to_document(settings))
                ),
            )
        )


class ConstructorDefinition(MetaTop[list[TypeT]]):
    def compare_value(self, value2: list[TypeT]) -> bool:
        def compare_types(t1: TypeT, t2: TypeT):
            return t1.compare(t2)

        return compare_list(self.value, value2, compare_types)

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        if len(self.value) == 0:
            return Text(self.name)

        doc: DocumentT = self.value[0].to_document(settings)
        for type_ in self.value[1:]:
            new_doc: DocumentT
            if isinstance(type_, TypeApplication):
                new_doc = parens(settings, type_.to_document(settings))
            else:
                new_doc = type_.to_document(settings)
            doc = doc + LineBreak() + new_doc
        return Text(self.name) + Text(" ") + doc


class DataType(SST):
    name: ConstructorName | Operator
    arguments: list[TypeVariable]
    constructors: list[ConstructorDefinition]

    def __init__(
        self,
        name: ConstructorName | Operator,
        arguments: list[TypeVariable],
        constructors: list[ConstructorDefinition],
        _range: Range,
    ):
        self.name = name
        self.arguments = arguments
        self.constructors = constructors
        self._range = _range

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, type(self))
            and self.name == other.name
            and compare_list(
                self.constructors, other.constructors, TypeVariable.compare
            )
            and compare_list(
                self.constructors,
                other.constructors,
                ConstructorDefinition.compare,
            )
        )

    def __repr__(self):
        return f"{type(self).__name__}({self.name},{self.arguments},{self.constructors},{self._range})"

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        doc_types = list_to_document_with(self.arguments, LineBreak(), settings)

        doc: DocumentT
        if len(self.constructors) == 1:
            doc = self.constructors[0].to_document(settings)
        else:
            doc = list_to_document_with(
                self.constructors, LineBreak() + Text("| "), settings
            )
        return Text("data ") + Group(
            Indent(
                1,
                self.name.to_document(settings)
                + Group(
                    (LineBreak() + doc_types) if doc_types != Nil() else Nil()
                )
                + LineBreak()
                + Text("=")
                + Group(Indent(1, LineBreak() + doc)),
            )
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
class ParsedModule(Top):
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
            isinstance(other, ParsedModule)
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
            + list_to_document(self.imports, settings)
            + list_to_document(self.data_types, settings)
            + list_to_document(self.declarations, settings)
            + list_to_document(self.definitions, settings)
        )
        return doc

    # FIXME:
    def __repr_(self):
        return "ParsedModule"


def list_to_document(lst: list[T_SST], settings: DocumentSettings) -> DocumentT:
    doc: DocumentT = Nil()
    for item in lst:
        doc = doc + AlwaysLineBreak() + item.to_document(settings)
    return doc

    # exports: Exports
    # imports: list[ImportModule]
    # data_types: list[DataType]
    # declarations: list[Declaration]
    # definitions: list[Definition]
