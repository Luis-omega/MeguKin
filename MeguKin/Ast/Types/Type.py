class Type:
    pass


class TypeInt(Type):
    pass


class TypeName(Type):
    name: str

    def __init__(self, name: str):
        self.name = name

    def pretty(self):
        return f"{self.name}"

    def __str__(self):
        return f"TypeName({self.name})"

    def __repr__(self):
        return f"TypeName({self.name})"


class TypeApplication(Type):
    function: Type
    argument: Type

    def __init__(self, function: Type, argument: Type):
        self.function = function
        self.argument = argument

    def pretty(self):
        return f"TypeApplication({self.function},{self.argument})"

    def __str__(self):
        return f"TypeApplication({self.function},{self.argument})"

    def __repr__(self):
        return f"TypeApplication({self.function},{self.argument})"


class TypeArrow(Type):
    domain: Type
    codomain: Type

    def __init__(self, domain: Type, codomain: Type):
        self.domain = domain
        self.codomain = codomain

    def pretty(self):
        if isinstance(self.domain, TypeArrow):
            return f"({self.domain.pretty()})-> {self.codomain.pretty()}"
        return f"{self.domain.pretty()} -> {self.codomain.pretty()}"

    def __str__(self):
        return repr(self)

    def __repr__(self):
        return f"TypeArrow({self.domain}, {self.codomain})"
