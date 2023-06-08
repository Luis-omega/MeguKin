from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import TypeVar, Generic

from MeguKin.Pretty import ToDocument
from MeguKin.File import Range
from MeguKin.Comparable import Comparable

T = TypeVar("T")


class CSTMeta(ABC):
    pass


@dataclass
class CSTMetaClass:
    _range: Range


class CST(CSTMeta, Comparable, ToDocument, CSTMetaClass):
    def __str__(self):
        return repr(self)

    @abstractmethod
    def __repr__(self):
        """
        Usual python repr
        """
