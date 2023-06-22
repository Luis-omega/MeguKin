from abc import ABC, abstractmethod
from inspect import signature
from typing import TypeVar, Generic, Union

from MeguKin.File import HasRange, Range


T = TypeVar("T", bound="HasRange")
ComparableT = Union[int, str, list["ComparableT"], T]


def compare(self, other: ComparableT) -> bool:
    if isinstance(self, int) or isinstance(self, str):
        return self == other
    elif isinstance(self, list) and isinstance(other, list):
        if len(self) != len(other):
            return False
        for i, j in zip(self, other):
            if not compare(i, j):
                return False
        return True
    else:
        if type(self) != type(other):
            return False
        for arg in signature(self.__init__).parameters:
            if arg == "_range":
                pass
            self_attrib = getattr(self, arg)
            other_attrib = getattr(other, arg)
            if not compare(self_attrib, other_attrib):
                return False
        return True
