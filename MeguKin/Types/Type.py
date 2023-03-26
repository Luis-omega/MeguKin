class Type:
    pass


class TypeInt(Type):
    pass


class TypeName(Type):
    name: str

    def __init__(self, name: str):
        self.name = name

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

    def __str__(self):
        if isinstance(self.domain, TypeArrow):
            return f"({self.domain})-> {self.codomain}"
        return f"{self.domain} -> {self.codomain}"

    def __repr__(self):
        if isinstance(self.domain, TypeArrow):
            return f"({self.domain})-> {self.codomain}"
        return f"{self.domain} -> {self.codomain}"
