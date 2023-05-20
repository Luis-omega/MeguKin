from typing import Union
from lark import Token

from MeguKin.SugaredSyntaxTree.Range import Range, token2Range

TypeT = Union["TypeArrow", "TypeApplication", "TypeVariable", "TypeConcreteName"]


class Type:
    pass


class TypeVariable(Type):
    prefix: list[str]
    name: str
    _range: Range

    def __init__(self, prefix: list[str], name: str, _range: Range) -> None:
        self.prefix = prefix
        self.name = name
        self._range = _range

    @staticmethod
    def from_lark_token(token: Token) -> "TypeVariable":
        _range = token2Range(token)
        splited = token.value.split(".")
        name = splited[-1]
        prefix = splited[:-1]
        return TypeVariable(prefix, name, _range)

    def __str__(self):
        prefix = ".".join(self.prefix)
        return f"{prefix}.{self.name}"

    def __repr__(self):
        return f"TypeVariable({self.prefix},{self.name},{self._range})"


class TypeConcreteName(Type):
    prefix: list[str]
    name: str
    _range: Range

    def __init__(self, prefix: list[str], name: str, _range: Range) -> None:
        self.prefix = prefix
        self.name = name
        self._range = _range

    @staticmethod
    def from_lark_token(token: Token) -> "TypeConcreteName":
        _range = token2Range(token)
        splited = token.value.split(".")
        name = splited[-1]
        prefix = splited[:-1]
        return TypeConcreteName(prefix, name, _range)

    def __str__(self):
        prefix = ".".join(self.prefix)
        return f"{prefix}.{self.name}"

    def __repr__(self):
        return f"TypeConcreteName({self.prefix},{self.name},{self._range})"


class TypeApplication(Type):
    function: Type
    argument: Type
    _range: Range

    def __init__(self, function: Type, argument: Type, _range: Range):
        self.function = function
        self.argument = argument
        self._range = _range

    def pretty(self):
        raise Exception("not implemented")

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
