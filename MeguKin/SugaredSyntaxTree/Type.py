from typing import Union, TypeVar
from MeguKin.File import Range
from MeguKin.SugaredSyntaxTree.SST import (
    SST,
    compare_list,
    MetaRecord,
    MetaVar,
    MetaMeaninglessOperatorApplications,
)

from MeguKin.Pretty import (
    Text,
    DocumentT,
    Group,
    LineBreak,
    parens,
    Nil,
    DocumentSettings,
    Indent,
    AlwaysLineBreak,
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


class TypeRecordField(MetaVar, Type):
    pass


class TypeRecord(MetaRecord[TypeT, TypeRecordField], Type):
    @staticmethod
    def compare_items(item1: TypeT, item2: TypeT):
        return item1.compare(item2)

    def item_to_document(self, settings: DocumentSettings, item: TypeT):
        return Text(":") + Group(Indent(1, item.to_document(settings)))


class TypeMeaninglessOperatorApplications(
    MetaMeaninglessOperatorApplications[TypeT, TypeOperator], Type
):
    def to_document(self, settings: DocumentSettings):
        doc: DocumentT = Nil()
        new_doc: DocumentT
        for item in self.applications[::-1]:
            if isinstance(item, TypeOperator):
                new_doc = (
                    LineBreak() + item.to_document(settings) + Text(" ") + doc
                )
                doc = Group(new_doc)
            else:
                new_doc = item.to_document(settings)
                doc = new_doc + doc
        return doc


class TypeApplication(Type):
    function: TypeT
    argument: TypeT

    def __init__(self, function: TypeT, argument: TypeT, _range: Range):
        self.function = function
        self.argument = argument
        self._range = _range

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, TypeApplication)
            and self.function.compare(other.function)
            and self.argument.compare(other.argument)
        )

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        # We need to take care of the case that we have an application
        # to other application like `f (g x)` we don't want to
        # print `f g x`
        match self.argument:
            case TypeApplication():
                arg = parens(settings, self.argument.to_document(settings))
                return self.function.to_document(settings) + Text(" ") + arg
            case _:
                return (
                    self.function.to_document(settings)
                    + Text(" ")
                    + self.argument.to_document(settings)
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

    def to_document(self, settings: DocumentSettings):
        match self.domain:
            case TypeArrow():
                return (
                    parens(settings, self.domain.to_document(settings))
                    + LineBreak()
                    + Text("-> ")
                    + self.codomain.to_document(settings)
                )
            case _:
                return (
                    self.domain.to_document(settings)
                    + LineBreak()
                    + Text("-> ")
                    + self.codomain.to_document(settings)
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

    def to_document(self, settings: DocumentSettings):
        doc: DocumentT = self.args[0].to_document(settings)
        for arg in self.args[1:]:
            new_doc = arg.to_document(settings)
            doc = doc + LineBreak() + new_doc
        return (
            Text("forall")
            + Indent(1, LineBreak() + doc)
            + LineBreak()
            + Text(".")
            + Indent(1, LineBreak() + self.expression.to_document(settings))
        )

    def __repr__(self):
        return f"TypeForall({self.args},{self.expression})"
