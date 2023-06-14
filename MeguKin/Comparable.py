from abc import ABC, abstractmethod
from typing import TypeVar, Generic

T = TypeVar("T", bound="Comparable")


class Comparable(ABC):
    @abstractmethod
    def compare(self, other: T) -> bool:
        pass


def compare_list(l1: list[T], l2: list[T]) -> bool:
    return len(l1) == len(l2) and all(i.compare(j) for i, j in zip(l1, l2))
