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


class Application(Expression):
    function: Expression
    argument: Expression

    def __init__(self, function: Expression, argument: Expression):
        self.function = function
        self.argument = argument


class Function(Expression):
    variable: Variable
    value: Expression

    def __init__(self, variable: Variable, value: Expression):
        self.value = value
        self.variable = variable
