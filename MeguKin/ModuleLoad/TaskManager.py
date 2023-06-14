from typing import Union, Callable, Optional
from dataclasses import dataclass
from pathlib import Path
from abc import ABC, abstractmethod
from collections import deque

from MeguKin.SugaredSyntaxTree.Module import ImportModuleName, ExportModuleName
from MeguKin.Parser.Parser import FileLoadError, ParserError
from MeguKin.SugaredSyntaxTree.Top import ParsedModule
from MeguKin.ModuleLoad.SymbolResolution import (
    SymbolPath,
    SymbolResolution,
    CantResolveSymbol,
)


TaskT = Union[
    "PendingToSolveSymbol",
    "PendingToParse",
    "PendingToGetModules",
    "SolvedSymbol",
]


class Status:
    """This class represent a task to be performed async"""


@dataclass
class PendingToSolveSymbol(Status):
    pass


@dataclass
class PendingToParse(Status):
    symbol_path: SymbolPath


@dataclass
class PendingToGetModules(Status):
    symbol_path: SymbolPath
    parsed: ParsedModule


@dataclass
class SolvedSymbol(Status):
    symbol_path: SymbolPath
    ast: ParsedModule


@dataclass
class NumberAndMark:
    number: int
    mark: bool


def init_depth_first_search_dic(
    to_find: dict[ExportModuleName, PendingToGetModules]
) -> dict[ExportModuleName, NumberAndMark]:
    return dict((name, NumberAndMark(0, False)) for name in to_find)


def import2Export(i: ImportModuleName) -> ExportModuleName:
    return ExportModuleName(i.prefix, i.name, i._range)


def find_back_edge(
    initial_edge: ExportModuleName,
    arg_dict: dict[ExportModuleName, PendingToGetModules],
) -> Optional[tuple[ExportModuleName, ExportModuleName]]:
    visit_dict = init_depth_first_search_dic(arg_dict)
    visit_number: int = 0

    def DFS(
        vertex: tuple[ExportModuleName, PendingToGetModules], visit_number: int
    ) -> tuple[int, Optional[tuple[ExportModuleName, ExportModuleName]]]:
        visit_name, parsed_modules = vertex
        visit_number = visit_number + 1
        visit_dict[visit_name] = NumberAndMark(visit_number, True)
        for import_ in parsed_modules.parsed.imports:
            name = import2Export(import_.module_name)
            if visit_dict[name] == 0:
                visit_number, maybe_back_edge = DFS(
                    (name, arg_dict[name]), visit_number
                )
                if maybe_back_edge is not None:
                    return (visit_number, maybe_back_edge)
            elif (
                visit_dict[name].number > visit_dict[visit_name].number
                or not visit_dict[name].mark
            ):
                continue
            else:
                return (visit_number, (visit_name, name))
        visit_dict[visit_name].mark = False
        return visit_number, None

    _, maybe_back_edge = DFS(
        (initial_edge, arg_dict[initial_edge]), visit_number
    )
    return maybe_back_edge


@dataclass
class TaskManager:
    symbol_resolution: SymbolResolution
    parser: Callable[[Path], ParsedModule | FileLoadError | ParserError]
    pending_to_solve: dict[ImportModuleName, PendingToSolveSymbol]
    pending_to_parse: dict[ImportModuleName, PendingToParse]
    pending_to_get_modules: dict[ExportModuleName, PendingToGetModules]
    solved: dict[ImportModuleName, SolvedSymbol]

    def __init__(self, paths: set[Path]):
        self.SymbolResolution = SymbolResolution(paths, dict())
        self.pending_to_parse = dict()
        self.pending_to_get_modules = dict()
        self.solved = dict()

    def solve_imports_to_parsed(
        self, to_solve: set[ImportModuleName]
    ) -> CantResolveSymbol | FileLoadError | ParserError | None:
        self.pending_to_solve = dict(
            (name, PendingToSolveSymbol()) for name in to_solve
        )
        while self.pending_to_solve:
            maybe_solved = self.symbol_resolution.query_symbols(
                {name for name in self.pending_to_solve}
            )
            if isinstance(maybe_solved, CantResolveSymbol):
                return maybe_solved
            self.pending_to_solve = dict()
            for name, symbol_solved in maybe_solved.items():
                maybe_parsed = self.parser(symbol_solved.path)
                if isinstance(maybe_parsed, FileLoadError) or isinstance(
                    maybe_parsed, ParserError
                ):
                    return maybe_parsed
                for import_ in maybe_parsed.imports:
                    if import_.module_name not in self.pending_to_parse:
                        self.pending_to_solve[
                            import_.module_name
                        ] = PendingToSolveSymbol()
                self.pending_to_get_modules[
                    maybe_parsed.exports.module_name
                ] = PendingToGetModules(
                    SymbolPath(symbol_solved.path), maybe_parsed
                )
        return None
