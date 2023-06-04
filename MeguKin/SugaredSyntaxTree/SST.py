from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import TypeVar, Callable

from MeguKin.File import Range, token2Range
from MeguKin.SugaredSyntaxTree.Pretty import Document, DocumentData
from MeguKin.Parser.Token import Token

T = TypeVar("T")


@dataclass
class SSTDataClass:
    _range: Range


class SST(ABC, SSTDataClass):
    """
    Abstract base class for Sugared Syntax Tree
    """

    @abstractmethod
    def compare(self, other: "SST") -> bool:
        """
        Compare for equality two SST without ranges
        """

    # @abstractmethod
    # def reconstruct(self) -> Document:
    #    """
    #    Attempt to reconstruct the original code
    #    """

    # @abstractmethod
    # def format(self, configuration: DocumentData) -> Document:
    #    """
    #    Attempt to format the code
    #    """

    def range2str(self) -> str:
        return repr(self._range)

    # @abstractmethod
    # def log_format(self) -> str:
    #    """
    #    Representation for logs
    #    """

    def __str__(self):
        return repr(self)

    @abstractmethod
    def __repr__(self):
        """
        Usual python repr
        """


T_MetaVar = TypeVar("T_MetaVar", bound="MetaVar")


class MetaVar(SST):
    prefix: list[str]
    name: str

    @classmethod
    def __init__(cls, prefix: list[str], name: str, _range: Range) -> None:
        cls.prefix = prefix
        cls.name = name
        cls._range = _range

    @classmethod
    def from_lark_token(cls: type[T_MetaVar], token: Token) -> T_MetaVar:
        _range = token2Range(token)
        splited = token.value.split(".")
        name = splited[-1]
        prefix = splited[:-1]
        return cls(prefix, name, _range)

    @classmethod
    def compare(cls, other: SST) -> bool:
        return (
            isinstance(other, cls)
            and cls.prefix == other.prefix
            and cls.name == other.name
        )

    @classmethod
    def __repr__(cls) -> str:
        return f"{cls.__name__}({cls.prefix},{cls.name},{cls._range})"


class LiteralDataClass:
    token: Token


class MetaLiteral(SST, LiteralDataClass):
    @classmethod
    def __init__(cls, token: Token):
        cls.token = token
        cls._range = token2Range(token)

    @classmethod
    def compare(cls, other: SST) -> bool:
        if isinstance(other, cls):
            return cls.token == other.token
        return False

    @classmethod
    def __repr__(cls):
        return f"{cls.__name__}({cls.token})"


def compare_list(
    l1: list[T], l2: list[T], compare: Callable[[T, T], bool]
) -> bool:
    return len(l1) == len(l2) and all(compare(i, j) for i, j in zip(l1, l2))
