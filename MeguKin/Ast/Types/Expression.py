from typing import Optional, Set, Union, TypeVar, Generic

from lark import Token

from MeguKin.Reconstruction import Range, token2Range, mergeRanges
from MeguKin.Ast.Types.Type import TypeT
from MeguKin.Ast.Types.PatternMatch import PatternMatchT

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


class Expression:
    pass


class Literal(Expression):
    value: Token
    _range: Range
    free_variables: Set[str] = set()

    def __init__(self, value: Token, _range: Range):
        self.value = value
        self._range = _range

    def __str__(self):
        return f"{self.value}"

    def __repr__(self):
        return f"Literal({self.value})"


class Variable(Expression):
    prefix: list[str]
    name: str
    _range: Range
    free_variables: Set[str]

    def __init__(self, prefix: list[str], name: str, _range: Range) -> None:
        self.prefix = prefix
        self.name = name
        self._range = _range
        self.free_variables = set(str(self))

    @staticmethod
    def from_lark_token(token: Token) -> "Variable":
        _range = token2Range(token)
        splited = token.value.split(".")
        name = splited[-1]
        prefix = splited[:-1]
        return Variable(prefix, name, _range)

    def __str__(self):
        prefix = ".".join(self.prefix)
        return f"{prefix}.{self.name}"

    def __repr__(self):
        return (
            f"Variable({self.prefix},{self.name},{self._range},{self.free_variables})"
        )


class Operator(Expression):
    prefix: list[str]
    name: str
    _range: Range
    free_variables: Set[str]

    def __init__(self, prefix: list[str], name: str, _range: Range) -> None:
        self.prefix = prefix
        self.name = name
        self._range = _range
        self.free_variables = set(str(self))

    @staticmethod
    def from_lark_token(token: Token) -> "Operator":
        _range = token2Range(token)
        splited = token.value.split(".")
        name = splited[-1]
        prefix = splited[:-1]
        return Operator(prefix, name, _range)

    def __str__(self):
        prefix = ".".join(self.prefix)
        return f"{prefix}.{self.name}"

    def __repr__(self):
        return (
            f"Operator({self.prefix},{self.name},{self._range},{self.free_variables})"
        )


class ConstructorName(Expression):
    prefix: list[str]
    name: str
    _range: Range
    free_variables: Set[str]

    def __init__(self, prefix: list[str], name: str, _range: Range) -> None:
        self.prefix = prefix
        # shall we add `assert name[0].isupper()` here?
        self.name = name
        self._range = _range
        self.free_variables = set(str(self))

    @staticmethod
    def from_lark_token(token: Token) -> "ConstructorName":
        _range = token2Range(token)
        splited = token.value.split(".")
        name = splited[-1]
        prefix = splited[:-1]
        return ConstructorName(prefix, name, _range)

    def __str__(self):
        prefix = ".".join(self.prefix)
        return f"{prefix}.{self.name}"

    def __repr__(self):
        return f"ConstructorName({self.prefix},{self.name},{self._range},{self.free_variables})"


class RecordUpdate(Expression):
    _map: list[tuple[str, Range, ExpressionT]]
    _range: Range
    free_variables: set[str]

    def __init__(self, _map: list[tuple[str, Range, ExpressionT]]) -> None:
        self._map = _map
        self._range = mergeRanges(_map[-1][1], _map[0][1])
        self.free_variables = set(*(i[2] for i in _map))

    def __str__(self):
        return repr(self)

    def __repr__(self):
        return f"RecordUpdate({self._map})"


class Record(Expression):
    _map: list[tuple[str, Range, Optional[ExpressionT]]]
    _range: Range
    free_variables: set[str]

    def __init__(self, _map: list[tuple[str, Range, Optional[ExpressionT]]]) -> None:
        self._map = _map
        self._range = mergeRanges(_map[-1][1], _map[0][1])
        self.free_variables = set(*(i[2] for i in _map))

    def __str__(self):
        return repr(self)

    def __repr__(self):
        return f"RecordUpdate({self._map})"


class Selector(Expression):
    expression: ExpressionT
    fields: list[str]
    _range: Range
    free_variables: set[str]

    def __init__(
        self, expression: ExpressionT, fields: list[str], _range: Range
    ) -> None:
        self.expression = expression
        self.fields = fields
        self.free_variables = expression.free_variables

    def __str__(self):
        return repr(self)

    def __repr__(self):
        return f"RecordUpdate({self.expression},{self.fields})"


class AnnotatedExpression(Expression):
    expression: ExpressionT
    annotation: Optional[TypeT]
    _range: Range
    free_variables: Set[str]

    def __init__(
        self,
        expression: ExpressionT,
        annotation: Optional[TypeT],
        _range: Range,
        free_variables: Set[str],
    ):
        self.expression = expression
        self.annotation = annotation
        self._range = _range
        self.free_variables = free_variables

    def pretty(self):
        return f"({self.expression.pretty()}:{self.annotation.pretty()})"

    def __str__(self):
        return f"AnnotatedExpression({self.expression},{self.annotation})"

    def __repr__(self):
        return f"AnnotatedExpression({self.expression},{self.annotation})"


class ExpressionTypeArgument(Expression):
    _type: TypeT
    _range: Range
    free_variables: set[str] = set()

    def __init__(self, _type: TypeT, _range: Range):
        self._type = _type
        self._range = _range


class Application(Expression):
    function: ExpressionT
    argument: ExpressionT
    _range: Range
    free_variables: Set[str]

    def __init__(
        self,
        function: ExpressionT,
        argument: ExpressionT,
        _range: Range,
        free_variables: Set[str],
    ):
        self.function = function
        self.argument = argument
        self._range = _range
        self.free_variables = free_variables

    def pretty(self):
        if isinstance(self.function, Literal) or isinstance(self.function, Variable):
            return f"{self.function.pretty()} {self.argument.pretty()}"
        else:
            return f"({self.function.pretty()}) ({self.argument.pretty()})"

    def __str__(self):
        return self.pretty()

    def __repr__(self):
        return f"Application({self.function},{self.argument})"


class IntercalatedList(Generic[T, T2]):
    pass


class IntercalatedListFist(IntercalatedList[T, T2]):
    value: T

    def __init__(self, value: T):
        self.value = value

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return f"IntercalatedListFist({self.value})"


class IntercalatedListSecond(IntercalatedList[T, T2]):
    value: T
    tail: IntercalatedList[T2, T]

    def __init__(self, value: T, tail: IntercalatedList[T2, T]):
        self.value = value
        self.tail = tail

    def __str__(self):
        return f"({self.value},{self.tail})"

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
    free_variables: Set[str]

    def __init__(
        self,
        listOfOperatorExpression: IntercalatedList[ExpressionT, Operator],
        _range: Range,
        free_variables: Set[str],
    ):
        self.listOfOperatorExpression = listOfOperatorExpression
        self._range = _range
        self.free_variables = free_variables

    def __str__(self):
        def prettify(exp: ExpressionT):
            if (
                isinstance(exp, Function)
                or isinstance(exp, OperatorsWithoutMeaning)
                or isinstance(exp, Let)
            ):
                return f"({exp})"
            else:
                return str(exp)

        return f"{self.listOfOperatorExpression}"

    def __repr__(self):
        return f"OperatorWithoutMeaning({repr(self.listOfOperatorExpression)})"


class CaseCase:
    pattern: PatternMatchT
    expression: ExpressionT
    _range: Range
    free_variables: set[str]

    def __init__(self, pattern: PatternMatchT, expression: ExpressionT):
        self.pattern = pattern
        self.expression = expression
        self.range = mergeRanges(pattern._range, expression._range)
        self.free_variables = expression.free_variables - pattern.bound_variables

    def __str__(self):
        return f"({str(self.pattern)} -> {str(self.expression)})"

    def __repr__(self):
        return f"CaseCase({repr(self.pattern)},{repr(self.expression)})"


class Case(Expression):
    expression: ExpressionT
    cases: list[CaseCase]
    _range: Range
    free_variables: set[str]

    def __init__(self, expression: ExpressionT, cases: list[CaseCase], _range: Range):
        self.expression = expression
        self.cases = cases
        self._range = _range
        self.free_variables = set.union(*map(lambda x: x.free_variables, cases))

    def __str__(self):
        return f"case {str(self.expression)} of ({str(self.cases)})"

    def __repr__(self):
        return f"Case({repr(self.expression)},{repr(self.cases)})"


class Function(Expression):
    pattern: PatternMatchT
    expression: ExpressionT
    _range: Range
    free_variables: Set[str]

    def __init__(
        self,
        pattern: PatternMatchT,
        expression: ExpressionT,
        _range: Range,
    ):
        self.pattern = pattern
        self.expression = expression
        self._range = _range
        self.free_variables = expression.free_variables - pattern.bound_variables

    def __str__(self):
        return f"\\ {str(self.pattern)} -> ({str(self.value)})"

    def __repr__(self):
        return f"Function({repr(self.pattern)},{repr(self.value)})"


class LetBinding(Expression):
    name: Token
    expression: ExpressionT
    _range: Range
    free_variables: set[str]
    bound_variables: set[str]

    def __init__(
        self,
        name: Token,
        expression: ExpressionT,
    ):
        self.name = name
        self.expression = expression
        self._range = mergeRanges(token2Range(name), expression._range)
        self.free_variables = expression.free_variables
        self.bound_variables = set(name.value)

    def __str__(self):
        return f"{str(self.name)} = {str(self.expression)}"

    def __repr__(self):
        return f"Let({repr(self.name)},{repr(self.expression)})"


class Let(Expression):
    bindings: list[LetBinding]
    expression: ExpressionT
    _range: Range
    free_variables: set[str]
    bound_variables: set[str]

    def __init__(
        self,
        bindings: list[LetBinding],
        expression: ExpressionT,
    ):
        self.bindings = bindings
        self.expression = expression
        self._range = mergeRanges(bindings[0]._range, expression._range)
        self.bound_variables = set(*map(lambda x: x.bound_variables, bindings))
        self.free_variables = (
            set.union(
                set.union(*(i.free_variables for i in bindings)),
                expression.free_variables,
            )
            - self.bound_variables
        )

    def __str__(self):
        bindings = "".join(f"({str(i)})" for i in self.bindings)
        return f"let {bindings} in ({str(self.expression)})"

    def __repr__(self):
        return f"Let({self.bindings},{self.expression})"
