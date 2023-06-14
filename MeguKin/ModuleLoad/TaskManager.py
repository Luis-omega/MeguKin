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
    initial_edges: list[ExportModuleName],
    arg_dict: dict[ExportModuleName, PendingToGetModules],
) -> Optional[tuple[ExportModuleName, ExportModuleName]]:
    visit_dict = init_depth_first_search_dic(arg_dict)
    visit_number: int = 0

    normal_stack = initial_edges.copy()
    while normal_stack:
        visit_number = visit_number + 1
        current_vertex = normal_stack.pop()
        visit_item = visit_dict[current_vertex]
        visit_item.number = visit_number
        visit_item.mark = True

        pending_module = arg_dict[current_vertex]

        for import_ in pending_module.parsed.imports:
            name = import2Export(import_.module_name)
            child = visit_dict[name]
            if child.number == 0:
                normal_stack.append(name)
            elif child.number > visit_item.number or not child.mark:
                continue
            else:
                return (current_vertex, name)

    return None


# class ModuleTree:
#    def find_symbol(MetaVar)->list[]:
#        pass


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
