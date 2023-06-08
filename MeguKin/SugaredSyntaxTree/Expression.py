from typing import Optional, Union, TypeVar

from MeguKin.File import Range, mergeRanges
from MeguKin.SugaredSyntaxTree.Type import TypeT
from MeguKin.SugaredSyntaxTree.PatternMatch import (
    PatternMatchT,
    compare_patterns,
)
from MeguKin.SugaredSyntaxTree.SST import (
    SST,
    compare_list,
    MetaVar,
    MetaLiteral,
    MetaRecord,
    MetaMeaninglessOperatorApplications,
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
    AlwaysLineBreak,
    maybe_indent,
    indent,
    Indent,
)


T = TypeVar("T")
T2 = TypeVar("T2")

ExpressionT = Union[
    "Literal",
    "Variable",
    "Operator",
    "ConstructorName",
    "RecordUpdate",
    "Record",
    "Selector",
    "ExpressionTypeArgument",
    "Application",
    "Function",
    "AnnotatedExpression",
    "ExpressionMeaninglessOperatorApplications",
    "Let",
]


class Expression(SST):
    pass


class Literal(MetaLiteral, Expression):
    pass


class Variable(MetaVar, Expression):
    pass


class Operator(MetaVar, Expression):
    pass


class PrefixOperator(MetaVar, Expression):
    def to_document(self, settings: DocumentSettings) -> Text:
        all_ = self.prefix.copy()
        all_.append(self.name)
        return parens(settings, Text(".".join(all_)))


class ConstructorName(MetaVar, Expression):
    pass


class RecordUpdate(MetaRecord[ExpressionT], Expression):
    @staticmethod
    def compare_items(
        item1: ExpressionT,
        item2: ExpressionT,
    ) -> bool:
        return item1.compare(item2)

    def item_to_document(self, settings: DocumentSettings, item: ExpressionT):
        return Text("=") + Group(expression_parens(settings, item))


class Record(MetaRecord[Optional[ExpressionT]], Expression):
    # The Optional represent a expression like {val} instead of {val=val}
    @staticmethod
    def compare_items(
        item1: Optional[ExpressionT],
        item2: Optional[ExpressionT],
    ) -> bool:
        return (item1 is None and item2 is None) or (
            item1 is not None and item2 is not None and item1.compare(item2)
        )

    def item_to_document(
        self, settings: DocumentSettings, item: Optional[ExpressionT]
    ):
        if item is None:
            return Nil()
        else:
            return Text(":") + Group(expression_parens(settings, item))


class Selector(Expression):
    expression: ExpressionT
    fields: list[str]

    def __init__(
        self, expression: ExpressionT, fields: list[str], _range: Range
    ) -> None:
        self.expression = expression
        self.fields = fields
        self._range = _range

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, Selector)
            and self.expression.compare(other.expression)
            and self.fields == other.fields
        )

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        return (
            Group(
                parens(settings, expression_parens(settings, self.expression))
            )
            + Text(".")
            + Text(".".join(self.fields))
        )

    def __repr__(self):
        return f"RecordUpdate({self.expression},{self.fields})"


class AnnotatedExpression(Expression):
    expression: ExpressionT
    annotation: TypeT

    def __init__(
        self,
        expression: ExpressionT,
        annotation: TypeT,
    ):
        self.expression = expression
        self.annotation = annotation
        self._range = mergeRanges(expression._range, annotation._range)

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, AnnotatedExpression)
            and self.expression.compare(other.expression)
            and self.annotation == other.annotation
        )

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        return (
            parens(settings, self.expression.to_document(settings))
            + Text(":")
            + self.annotation.to_document(settings)
        )

    def __repr__(self):
        return f"AnnotatedExpression({self.expression},{self.annotation})"


class ExpressionTypeArgument(Expression):
    _type: TypeT

    def __init__(self, _type: TypeT, _range: Range):
        self._type = _type
        self._range = _range

    def compare(self, other: SST) -> bool:
        return isinstance(other, ExpressionTypeArgument) and self._type.compare(
            other
        )

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        return Text("@") + parens(settings, self._type.to_document(settings))

    def __repr__(self):
        return f"ExpressionTypeArgument({self._type})"


class Application(Expression):
    function: ExpressionT
    argument: ExpressionT

    def __init__(
        self,
        function: ExpressionT,
        argument: ExpressionT,
        _range: Range,
    ):
        self.function = function
        self.argument = argument
        self._range = _range

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, Application)
            and self.function.compare(other.function)
            and self.argument.compare(other.argument)
        )

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        # We need to take care of the case that we have an application
        # to other application like `f (g x)` we don't want to
        # print `f g x`
        if need_parentheses(self.argument):
            return expression_parens(
                settings, self.function
            ) + expression_parens(settings, self.argument)
        else:
            match self.argument:
                case Application():
                    arg = parens(settings, self.argument.to_document(settings))
                    return (
                        expression_parens(settings, self.function)
                        + Text(" ")
                        + arg
                    )
                case _:
                    return (
                        expression_parens(settings, self.function)
                        + Text(" ")
                        + self.argument.to_document(settings)
                    )

    def __repr__(self):
        return f"Application({self.function},{self.argument})"


class ExpressionMeaninglessOperatorApplications(
    MetaMeaninglessOperatorApplications[ExpressionT, Operator], Expression
):
    def to_document(self, settings: DocumentSettings):
        doc: DocumentT = Nil()
        new_doc: DocumentT
        for item in self.applications[::-1]:
            if isinstance(item, Operator):
                new_doc = (
                    LineBreak() + item.to_document(settings) + Text(" ") + doc
                )
                doc = Group(new_doc)
            else:
                new_doc = expression_parens(settings, item)
                doc = new_doc + doc
        return doc


class CaseCase(Expression):
    pattern: PatternMatchT
    expression: ExpressionT

    def __init__(self, pattern: PatternMatchT, expression: ExpressionT):
        self.pattern = pattern
        self.expression = expression
        self._range = mergeRanges(pattern._range, expression._range)

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, CaseCase)
            and self.pattern.compare(other.pattern)
            and self.expression.compare(other.expression)
        )

    def to_document(self, settings: DocumentSettings):
        return (
            self.pattern.to_document(settings)
            + Text(" -> ")
            + maybe_indent(self.expression.to_document(settings))
        )

    def __repr__(self):
        return f"CaseCase({repr(self.pattern)},{repr(self.expression)})"


class Case(Expression):
    expression: ExpressionT
    cases: list[CaseCase]

    def __init__(
        self, expression: ExpressionT, cases: list[CaseCase], _range: Range
    ):
        self.expression = expression
        self.cases = cases
        self._range = _range

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, Case)
            and self.expression.compare(other.expression)
            and compare_list(self.cases, other.cases, CaseCase.compare)
        )

    def to_document(self, settings: DocumentSettings):
        doc: DocumentT = Nil()
        for case in self.cases[::-1]:
            new_doc = case.to_document(settings)
            doc = new_doc + LineBreak() + doc
        return (
            Text("case")
            + indent(self.expression.to_document(settings))
            + LineBreak()
            + Text("of")
            + indent(doc)
        )

    def __repr__(self):
        return f"Case({repr(self.expression)},{repr(self.cases)})"


class Function(Expression):
    patterns: list[PatternMatchT]
    expression: ExpressionT

    def __init__(
        self,
        patterns: list[PatternMatchT],
        expression: ExpressionT,
        _range: Range,
    ):
        self.patterns = patterns
        self.expression = expression
        self._range = _range

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, Function)
            and self.expression.compare(other.expression)
            and compare_list(self.patterns, other.patterns, compare_patterns)
        )

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        doc: DocumentT = Nil()
        for pattern in self.patterns[::-1]:
            new_doc = pattern.to_document(settings)
            doc = new_doc + LineBreak() + doc
        return (
            Text("\\ ")
            + NoSpaceLineBreak()
            + maybe_indent(doc)
            + LineBreak()
            + Text("->")
            + maybe_indent(self.expression.to_document(settings))
        )

    def __repr__(self):
        return f"Function({repr(self.patterns)},{repr(self.expression)})"


class LetBinding(Expression):
    pattern: PatternMatchT
    expression: ExpressionT

    def __init__(
        self,
        pattern: PatternMatchT,
        expression: ExpressionT,
    ):
        self.pattern = pattern
        self.expression = expression
        self._range = mergeRanges(pattern._range, expression._range)

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, LetBinding)
            and self.pattern.compare(other.pattern)
            and self.expression.compare(other.expression)
        )

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        return (
            self.pattern.to_document(settings)
            + Text(" =")
            + LineBreak()
            + self.expression.to_document(settings)
        )

    def __repr__(self):
        return f"LetBinding({repr(self.pattern)},{repr(self.expression)})"


class Let(Expression):
    bindings: list[LetBinding]
    expression: ExpressionT
    _range: Range

    def __init__(
        self,
        bindings: list[LetBinding],
        expression: ExpressionT,
    ):
        self.bindings = bindings
        self.expression = expression
        self._range = mergeRanges(bindings[0]._range, expression._range)

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, Let)
            and self.expression.compare(other.expression)
            and compare_list(self.bindings, other.bindings, LetBinding.compare)
        )

    def to_document(self, settings: DocumentSettings) -> DocumentT:
        doc: DocumentT = Nil()
        for binding in self.bindings[::-1]:
            new_doc = binding.to_document(settings)
            doc = new_doc + AlwaysLineBreak() + doc
        out = (
            Text("let")
            + AlwaysLineBreak()
            + doc
            + Text("in")
            + AlwaysLineBreak()
            + indent(self.expression.to_document(settings))
        )
        return out

    def __repr__(self):
        return f"Let({self.bindings},{self.expression})"


def expression_parens(
    settings: DocumentSettings, exp: ExpressionT
) -> DocumentT:
    if need_parentheses(exp):
        return parens(settings, exp.to_document(settings))
    else:
        return exp.to_document(settings)


def need_parentheses(exp: ExpressionT) -> bool:
    match exp:
        # TODO: TypeArgument can wo without parents
        case (
            Function()
            | Case()
            | AnnotatedExpression()
            | ExpressionMeaninglessOperatorApplications()
            | Let()
        ):
            return True
        case _:
            return False
