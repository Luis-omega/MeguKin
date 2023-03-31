from typing import Union

from MeguKin.Reconstruction import Range

TypeT = Union["TypeArrow", "TypeApplication", "TypeName"]


class Type:
    pass


class TypeName(Type):
    name: str
    _range: Range

    def __init__(self, name: str, _range: Range):
        self.name = name
        self._range = _range

    def pretty(self):
        return f"{self.name}"

    def __str__(self):
        return f"TypeName({self.name})"

    def __repr__(self):
        return f"TypeName({self.name})"


class TypeApplication(Type):
    function: Type
    argument: Type
    _range: Range

    def __init__(self, function: Type, argument: Type, _range: Range):
        self.function = function
        self.argument = argument
        self._range = _range

    def pretty(self):
        return f"TypeApplication({self.function},{self.argument})"

    def __str__(self):
        return f"TypeApplication({self.function},{self.argument})"

    def __repr__(self):
        return f"TypeApplication({self.function},{self.argument})"


class TypeArrow(Type):
    domain: Type
    codomain: Type
    _range: Range

    def __init__(self, domain: Type, codomain: Type, _range: Range):
        self.domain = domain
        self.codomain = codomain
        self._range = _range

    def pretty(self):
        if isinstance(self.domain, TypeArrow):
            return f"({self.domain.pretty()})-> {self.codomain.pretty()}"
        return f"{self.domain.pretty()} -> {self.codomain.pretty()}"

    def __str__(self):
        return repr(self)

    def __repr__(self):
        return f"TypeArrow({self.domain}, {self.codomain})"
