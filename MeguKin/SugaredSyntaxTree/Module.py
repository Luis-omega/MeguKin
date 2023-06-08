from typing import Union, Optional
from dataclasses import dataclass

from MeguKin.SugaredSyntaxTree.SST import MetaVar, SST, compare_list

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

ImportNameT = Union["ImportType", "ImportFunction", "ImportOperator"]

ExportNameT = Union["ExportType", "ExportFunction", "ExportOperator"]


class ImportConstructorName(MetaVar):
    pass


class ImportTypeName(MetaVar):
    pass


@dataclass
class ImportType(SST):
    type_name: ImportTypeName
    constructors: list[ImportConstructorName]

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, ImportType)
            and self.type_name.compare(other.type_name)
            and compare_list(
                self.constructors,
                other.constructors,
                ImportConstructorName.compare,
            )
        )

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        doc: DocumentT = Nil()
        for constructor in self.constructors:
            new_doc = constructor.to_document(settings)
            doc = doc + LineBreak() + Text(",") + new_doc
        return self.type_name.to_document(settings) + parens(
            settings, indent(parens(settings, maybe_indent(doc)))
        )

    def __repr__(self):
        return f"ImportType({self.type_name},{self.constructors})"


class ImportFunction(MetaVar):
    pass


class ImportOperator(MetaVar):
    pass


class ImportModuleName(MetaVar):
    pass


@dataclass
class ImportModule(SST):
    # The module being imported
    module_name: ImportModuleName
    imports: list[ImportNameT]
    # None if is not qualified, otherwise it has the qualified name
    qualified_name: Optional[str]

    def compare(self, other: SST) -> bool:
        def compare_items(i1: ImportNameT, i2: ImportNameT) -> bool:
            return i1.compare(i2)

        return (
            isinstance(other, ImportModule)
            and self.module_name.compare(other.module_name)
            and self.qualified_name == self.qualified_name
            and compare_list(self.imports, other.imports, compare_items)
        )

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        doc: DocumentT = Nil()
        for import_ in self.imports:
            new_doc = import_.to_document(settings)
            doc = doc + LineBreak() + Text(",") + new_doc
        return (
            Text("module ")
            + self.module_name.to_document(settings)
            + parens(settings, indent(parens(settings, maybe_indent(doc))))
        )


# -----------------------------------Exports-------------------------------


class ExportConstructorName(MetaVar):
    pass


class ExportTypeName(MetaVar):
    pass


@dataclass
class ExportType(SST):
    type_name: ExportTypeName
    constructors: list[ExportConstructorName]

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, ExportType)
            and self.type_name.compare(other.type_name)
            and compare_list(
                self.constructors,
                other.constructors,
                ExportConstructorName.compare,
            )
        )

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        doc: DocumentT = Nil()
        for constructor in self.constructors:
            new_doc = constructor.to_document(settings)
            doc = doc + LineBreak() + Text(",") + new_doc
        return self.type_name.to_document(settings) + parens(
            settings, indent(parens(settings, maybe_indent(doc)))
        )


class ExportFunction(MetaVar):
    pass


class ExportOperator(MetaVar):
    pass


class ExportModuleName(MetaVar):
    pass
