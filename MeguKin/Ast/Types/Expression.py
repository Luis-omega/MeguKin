from typing import Optional, List, Union, Set

from lark import Token

from MeguKin.Reconstruction import Range
from MeguKin.Ast.Types.Type import TypeT
from MeguKin.Ast.Types.PatternMatch import PatternMatchT

ExpressionT = Union[
    "Literal",
    "Variable",
    "Application",
    "Function",
    "AnnotatedExpression",
    "OperatorsWithoutMeaning",
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

    def pretty(self):
        return f"{self.value}"

    def __str__(self):
        return f"Int({self.value})"

    def __repr__(self):
        return f"Int({self.value})"


class Variable(Expression):
    name: Token
    constructor: bool
    _range: Range
    free_variables: Set[str]

    def __init__(
        self, name: Token, constructor: bool, _range: Range, free_variables: Set[str]
    ):
        self.name = name
        self.constructor = constructor
        self._range = _range
        self.free_variables = free_variables

    def pretty(self):
        return f"{self.name}"

    def __str__(self):
        return f"Variable({self.name})"

    def __repr__(self):
        return f"Variable({self.name})"


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
        return f"{self.function.pretty()} ({self.argument}.pretty())"

    def __str__(self):
        return repr(self)

    def __repr__(self):
        return f"Application({self.function},{self.argument})"


class Function(Expression):
    pattern: PatternMatchT
    value: ExpressionT
    _range: Range
    free_variables: Set[str]

    def __init__(
        self,
        pattern: PatternMatchT,
        value: ExpressionT,
        _range: Range,
        free_variables: Set[str],
    ):
        self.value = value
        self.pattern = pattern
        self._range = _range
        self.free_variables = free_variables

    def pretty(self):
        return f"\ {self.pattern.pretty()} -> ({self.value.pretty()})"

    def __str__(self):
        return f"Function({self.pattern},{self.value})"

    def __repr__(self):
        return f"Function({self.pattern},{self.value})"


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


class OperatorsWithoutMeaning(Expression):
    firstExpression: Expression
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
    listOfOperatorExpression: List[Union[Token, ExpressionT]]
    _range: Range
    free_variables: Set[str]

    def __init__(
        self,
        listOfOperatorExpression: List[Union[Token, ExpressionT]],
        _range: Range,
        free_variables: Set[str],
    ):
        self.listOfOperatorExpression = listOfOperatorExpression
        self._range = _range
        self.free_variables = free_variables

    def pretty(self):
        def prettify(exp: ExpressionT):
            if isinstance(exp, Function) or isinstance(exp, OperatorsWithoutMeaning):
                return f"({exp.pretty()})"
            else:
                return exp.pretty()

        args = " ".join(
            [
                prettify(i) if isinstance(i, Expression) else i
                for i in self.listOfOperatorExpression
            ]
        )
        return f"{args}"

    def __str__(self):
        return f"OperatorWithoutMeaning({self.listOfOperatorExpression})"

    def __repr__(self):
        return f"OperatorWithoutMeaning({self.listOfOperatorExpression})"
