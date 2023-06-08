from abc import ABC, abstractmethod
from typing import TypeVar, Generic

T = TypeVar("T")


class Comparable(ABC, Generic[T]):
    @abstractmethod
    def compare(self, other: T) -> bool:
        pass
