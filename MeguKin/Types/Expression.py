from typing import Optional

from MeguKin.Types.Type import Type
from MeguKin.Types.PatternMatch import PatternMatch


class Expression:
    pass


class Int(Expression):
    value: int

    def __init__(self, value: int):
        self.value = value


class Variable(Expression):
    name: str

    def __init__(self, name: str):
        self.name = name

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

    def __str__(self):
        return f"Application({self.function},{self.argument})"

    def __repr__(self):
        return f"Application({self.function},{self.argument})"


class Function(Expression):
    pattern: PatternMatch
    value: Expression

    def __init__(self, pattern: PatternMatch, value: Expression):
        self.value = value
        self.pattern = pattern

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

    def __str__(self):
        return f"({self.expression}:{self.annotation})"

    def __repr__(self):
        return f"({self.expression}:{self.annotation})"
