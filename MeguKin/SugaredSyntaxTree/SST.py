from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import TypeVar, Callable

from MeguKin.File import Range
from MeguKin.SugaredSyntaxTree.Pretty import Document, DocumentData

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


def compare_list(
    l1: list[T], l2: list[T], compare: Callable[[T, T], bool]
) -> bool:
    return len(l1) == len(l2) and all(compare(i, j) for i, j in zip(l1, l2))
