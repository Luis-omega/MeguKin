from typing import Union
from dataclasses import dataclass
from abc import ABC, abstractmethod

from MeguKin.SugaredSyntaxTree.Module import ImportModuleName
from MeguKin.SugaredSyntaxTree.Top import ParsedModule
from MeguKin.ModuleLoad.SymbolResolution import SymbolPath


TaskT = Union[
    "PendingToSolveSymbol",
    "PendingToParse",
    "PendingToGetModules",
    "SolvedSymbol",
]


class Task:
    """This class represent a task to be performed async"""


@dataclass
class PendingToSolveSymbol(Task):
    pass


@dataclass
class PendingToParse(Task):
    symbol: SymbolPath


@dataclass
class PendingToGetModules(Task):
    symbol: SymbolPath
    parsed: ParsedModule


@dataclass
class SolvedSymbol(Task):
    symbol: SymbolPath
    ast: ParsedModule


@dataclass
class TaskManager(ABC):
    @abstractmethod
    def add_task(self, task: Task):
        pass
