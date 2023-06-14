from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import TypeVar, Generic

from MeguKin.Pretty import Text
from MeguKin.File import HasRange
from MeguKin.Comparable import Comparable

T = TypeVar("T")


class CSTMeta(ABC):
    pass


class ToDocument:
    @abstractmethod
    def to_document(self) -> Text:
        """To document"""


class CST(CSTMeta, HasRange, Comparable, ToDocument):
    def __str__(self):
        return repr(self)

    @abstractmethod
    def __repr__(self):
        """
        Usual python repr
        """
