from typing import Optional, Union, TypeVar, Generic
from abc import abstractmethod

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
    "OperatorsWithoutMeaning",
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


class ConstructorName(MetaVar, Expression):
    pass


class RecordUpdate(Expression):
    _map: list[tuple[str, Range, ExpressionT]]
    _range: Range

    def __init__(self, _map: list[tuple[str, Range, ExpressionT]]) -> None:
        self._map = _map
        self._range = mergeRanges(_map[-1][1], _map[0][1])

    def compare(self, other: SST) -> bool:
        def compare_map_items(
            item1: tuple[str, Range, ExpressionT],
            item2: tuple[str, Range, ExpressionT],
        ) -> bool:
            return item1[0] == item2[0] and item1[2].compare(item2[2])

        return isinstance(other, RecordUpdate) and compare_list(
            self._map, other._map, compare_map_items
        )

    def __repr__(self):
        return f"RecordUpdate({self._map})"


class Record(Expression):
    # The Optional represent a expression like {val} instead of {val=val}
    _map: list[tuple[str, Range, Optional[ExpressionT]]]

    def __init__(
        self, _map: list[tuple[str, Range, Optional[ExpressionT]]]
    ) -> None:
        self._map = _map
        self._range = mergeRanges(_map[-1][1], _map[0][1])

    def compare(self, other: SST) -> bool:
        def compare_map_items(
            item1: tuple[str, Range, Optional[ExpressionT]],
            item2: tuple[str, Range, Optional[ExpressionT]],
        ) -> bool:
            if item1[2] is None and item2[2] is None:
                return item1[0] == item2[0]
            elif item1[2] is not None and item2[2] is not None:
                return item1[0] == item2[0] and item1[2].compare(item2[2])
            return False

        return isinstance(other, Record) and compare_list(
            self._map, other._map, compare_map_items
        )

    def __repr__(self):
        return f"RecordUpdate({self._map})"


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

    def __repr__(self):
        return f"Application({self.function},{self.argument})"


T1_Intercalated = TypeVar("T1_Intercalated", bound=Expression)
T2_Intercalated = TypeVar("T2_Intercalated", bound=Expression)


class IntercalatedList(Generic[T1_Intercalated, T2_Intercalated]):
    @abstractmethod
    def compare(self, other: SST) -> bool:
        """
        As in SST, compare two methods
        """


class IntercalatedListFist(IntercalatedList[T1_Intercalated, T2_Intercalated]):
    value: T1_Intercalated

    def __init__(self, value: T1_Intercalated):
        self.value = value

    def compare(self, other):
        return isinstance(other, IntercalatedListFist) and self.value.compare(
            other.value
        )

    def __repr__(self):
        return f"IntercalatedListFist({self.value})"


class IntercalatedListSecond(
    IntercalatedList[T1_Intercalated, T2_Intercalated]
):
    value: T1_Intercalated
    tail: IntercalatedList[T2_Intercalated, T1_Intercalated]

    def __init__(
        self,
        value: T1_Intercalated,
        tail: IntercalatedList[T2_Intercalated, T1_Intercalated],
    ):
        self.value = value
        self.tail = tail

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, IntercalatedListSecond)
            and self.value == other.value
            and self.tail.compare(other)
        )

    def __repr__(self):
        return f"IntercalatedListSecond({self.value},{self.tail})"


class OperatorsWithoutMeaning(Expression):
    # In fact we know that the list has this form:
    # [Expression, str,Expression,str,Expression,str,...]
    # But to encode that with types we would need to create the following
    # GAT:
    # {-# LANGUAGE GADTs,DataKinds #-}
    #
    # data AlternatingList a b where
    #   InitialItem :: a -> AlternatingList a b
    #   ConsItem :: b -> AlternatingList a b -> AlternatingList b a
    #
    # value :: AlternatingList Int String
    # value = ConsItem 1 (ConsItem "hi" (InitialItem 0))    #
    #
    # And in fact we can translate that with a huge cost at runtime to us...
    listOfOperatorExpression: IntercalatedList[ExpressionT, Operator]
    _range: Range

    def __init__(
        self,
        listOfOperatorExpression: IntercalatedList[ExpressionT, Operator],
        _range: Range,
    ):
        self.listOfOperatorExpression = listOfOperatorExpression
        self._range = _range

    def compare(self, other: SST) -> bool:
        return isinstance(
            other, OperatorsWithoutMeaning
        ) and self.listOfOperatorExpression.compare(
            # python alone have erased the types of the list
            # so, we can't ensure statically that both are
            # of the same type, raisin a type error here.
            other.listOfOperatorExpression  # type:ignore
        )

    def __repr__(self):
        return f"OperatorWithoutMeaning({repr(self.listOfOperatorExpression)})"


class CaseCase(Expression):
    pattern: PatternMatchT
    expression: ExpressionT

    def __init__(self, pattern: PatternMatchT, expression: ExpressionT):
        self.pattern = pattern
        self.expression = expression
        self.range = mergeRanges(pattern._range, expression._range)

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, CaseCase)
            and self.pattern.compare(other.pattern)
            and self.expression.compare(other.expression)
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

    def __repr__(self):
        return f"Function({repr(self.patterns)},{repr(self.value)})"


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

    def __repr__(self):
        return f"Let({self.bindings},{self.expression})"
