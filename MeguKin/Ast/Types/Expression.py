from typing import Optional, List, Union

from MeguKin.Ast.Types.Type import Type
from MeguKin.Ast.Types.PatternMatch import PatternMatch


class Expression:
    pass


class Int(Expression):
    value: int

    def __init__(self, value: int):
        self.value = value

    def pretty(self):
        return f"{self.value}"

    def __str__(self):
        return f"Int({self.value})"

    def __repr__(self):
        return f"Int({self.value})"


class Variable(Expression):
    name: str

    def __init__(self, name: str):
        self.name = name

    def pretty(self):
        return f"{self.name}"

    def __str__(self):
        return f"Variable({self.name})"

    def __repr__(self):
        return f"Variable({self.name})"


class Application(Expression):
    function: Expression
    argument: Expression

    def __init__(self, function: Expression, argument: Expression):
        self.function = function
        self.argument = argument

    def pretty(self):
        return f"{self.function.pretty()} ({self.argument}.pretty())"

    def __str__(self):
        return repr(self)

    def __repr__(self):
        return f"Application({self.function},{self.argument})"


class Function(Expression):
    pattern: PatternMatch
    value: Expression

    def __init__(self, pattern: PatternMatch, value: Expression):
        self.value = value
        self.pattern = pattern

    def pretty(self):
        return f"\ {self.pattern.pretty()} -> {{{self.value.pretty()}}}"

    def __str__(self):
        return f"Function({self.pattern},{self.value})"

    def __repr__(self):
        return f"Function({self.pattern},{self.value})"


class AnnotatedExpression(Expression):
    expression: Expression
    annotation: Optional[Type]

    def __init__(self, expression: Expression, annotation: Optional[Type]):
        self.expression = expression
        self.annotation = annotation

    def pretty(self):
        return f"({self.expression}:{self.annotation})"

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
    listOfOperatorExpression: List[Union[str, Expression]]

    def __init__(self, listOfOperatorExpression: List[Union[str, Expression]]):
        self.listOfOperatorExpression = listOfOperatorExpression

    def pretty(self):
        args = " ".join(
            [
                f"({i.pretty()})" if isinstance(i, Expression) else i
                for i in self.listOfOperatorExpression
            ]
        )
        return f"{args}"

    def __str__(self):
        return f"OperatorWithoutMeaning({self.listOfOperatorExpression})"

    def __repr__(self):
        return f"OperatorWithoutMeaning({self.listOfOperatorExpression})"
