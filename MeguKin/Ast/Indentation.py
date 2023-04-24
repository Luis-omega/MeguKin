from Typing import Optional, Union
from enum import Enum, auto

from lark import Token


class Context(Enum):
    ROOT = auto()
    let = auto()
    CASE = auto()
    OF = auto()
    FORALL = auto()
    DATA = auto()
    LAMBDA = auto()
    RECORD = auto()
    PAREN = auto()


class Indenter:
    indentation_stack: list[int]

    def __init__(self) -> None:
        self.indentation_stack: int = [0]

    def _process(self, stream: list[Token]):
        for token in stream[1:]:
            pass
            # yield from self.handle_NL(token)
