from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import TypeVar, Callable, Generic

from MeguKin.File import Range, token2Range, mergeRanges
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

    def __init__(self, prefix: list[str], name: str, _range: Range) -> None:
        self.prefix = prefix
        self.name = name
        self._range = _range

    @classmethod
    def from_lark_token(cls: type[T_MetaVar], token: Token) -> T_MetaVar:
        _range = token2Range(token)
        splited = token.value.split(".")
        name = splited[-1]
        prefix = splited[:-1]
        return cls(prefix, name, _range)

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, type(self))
            and self.prefix == other.prefix
            and self.name == other.name
        )

    def __repr__(self) -> str:
        return f"{type(self).__name__}({self.prefix},{self.name},{self._range})"


class LiteralDataClass:
    token: Token


class MetaLiteral(SST, LiteralDataClass):
    def __init__(self, token: Token):
        self.token = token
        self._range = token2Range(token)

    def compare(self, other: SST) -> bool:
        return isinstance(other, type(self)) and self.token == other.token

    def __repr__(self):
        return f"{type(self).__name__}({self.token})"


class MetaTop(SST, Generic[T]):
    name: str
    value: T

    def __init__(
        cls,
        name: str,
        value: T,
        _range: Range,
    ):
        cls.name = name
        cls.value = value
        cls._range = _range

    def __repr__(self):
        return f"{type(self).__name__}({self.name},{self.value})"


class MetaRecord(SST, Generic[T]):
    _map: list[tuple[str, Range, T]]

    def __init__(self, _map: list[tuple[str, Range, T]]) -> None:
        self._map = _map
        self._range = mergeRanges(_map[-1][1], _map[0][1])

    def compare(self, other: SST) -> bool:
        def compare_map_items(
            item1: tuple[str, Range, T],
            item2: tuple[str, Range, T],
        ) -> bool:
            return item1[0] == item2[0] and self.compare_items(
                item1[2], item2[2]
            )

        return isinstance(other, type(self)) and compare_list(
            self._map, other._map, compare_map_items
        )

    @staticmethod
    @abstractmethod
    def compare_items(item1: T, item2: T):
        """
        Compare for the third component of the tuples in _map
        """

    def __repr__(self):
        return f"{type(self).__name__}({self._map})"


def compare_list(
    l1: list[T], l2: list[T], compare: Callable[[T, T], bool]
) -> bool:
    return len(l1) == len(l2) and all(compare(i, j) for i, j in zip(l1, l2))


T1_Intercalated = TypeVar("T1_Intercalated", bound=SST)
T2_Intercalated = TypeVar("T2_Intercalated", bound=SST)


class IntercalatedList(SST, Generic[T1_Intercalated, T2_Intercalated]):
    @abstractmethod
    def compare(self, other: SST) -> bool:
        """
        As in SST, compare two methods
        """


class IntercalatedListFist(IntercalatedList[T1_Intercalated, T2_Intercalated]):
    value: T1_Intercalated

    def __init__(self, value: T1_Intercalated):
        self.value = value
        self._range = value._range

    def compare(self, other):
        return isinstance(other, IntercalatedListFist) and self.value.compare(
            other.value
        )

    def __repr__(self):
        return f"IntercalatedListFist({self.value})"


class IntercalatedListSecond(
    IntercalatedList[T1_Intercalated, T2_Intercalated]
):
    value: T1_Intercalated
    tail: IntercalatedList[T2_Intercalated, T1_Intercalated]

    def __init__(
        self,
        value: T1_Intercalated,
        tail: IntercalatedList[T2_Intercalated, T1_Intercalated],
    ):
        self.value = value
        self.tail = tail
        self._range = mergeRanges(value._range, tail._range)

    def compare(self, other: SST) -> bool:
        return (
            isinstance(other, IntercalatedListSecond)
            and self.value == other.value
            and self.tail.compare(other)
        )

    def __repr__(self):
        return f"IntercalatedListSecond({self.value},{self.tail})"


T_Operator = TypeVar("T_Operator", bound=MetaVar)


class MetaMeaninglessOperatorApplications(
    SST, Generic[T1_Intercalated, T_Operator]
):
    applications: IntercalatedList[T1_Intercalated, T_Operator]

    def __init__(
        self,
        applications: IntercalatedList[T1_Intercalated, T_Operator],
        _range: Range,
    ):
        self.applications = applications
        self._range = _range

    @classmethod
    def compare(cls, other: SST) -> bool:
        return isinstance(other, cls) and cls.applications.compare(
            other.applications
        )

    def __repr__(self):
        return f"{type(self).__name__}({self.applications})"
