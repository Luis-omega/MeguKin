from dataclasses import dataclass
from typing import Optional, Callable
from abc import ABC, abstractmethod
from pathlib import Path

from MeguKin.SugaredSyntaxTree.Module import ImportModuleName
from MeguKin.SugaredSyntaxTree.Top import Exports, Module
from MeguKin.Error import MeguKinError


@dataclass
class SymbolPath:
    """Represent the path of a file in the system"""

    path: Path


class SymbolResolutionError(MeguKinError):
    pass


@dataclass
class CantResolveSymbol(SymbolResolutionError):
    symbol: ImportModuleName


@dataclass
class SymbolResolution:
    """This class find in the operative system for the
    module files"""

    root_paths: set[Path]
    solved_symbols: dict[ImportModuleName, SymbolPath]

    def find_symbol_path_in_path(
        self, path: Path, splited: list[str]
    ) -> Optional[Path]:
        """Try to find the symbol in the path, internal"""
        if len(splited) == 0:
            return None
        candidate_path = build_path(splited)
        if candidate_path and candidate_path.exists():
            return candidate_path
        return None

    def solve_symbol(self, to_solve: ImportModuleName) -> Optional[SymbolPath]:
        """for internal use only"""
        splited = to_solve.prefix + [to_solve.name]
        if len(splited) == 0:
            return None
        for path in self.root_paths:
            result = self.find_symbol_path_in_path(path, splited)
            if result is not None:
                return SymbolPath(result)
        return None

    def solve_symbols(
        self, symbols_to_solve: set[ImportModuleName]
    ) -> CantResolveSymbol | None:
        for symbol in symbols_to_solve:
            if symbol in self.solved_symbols:
                continue
            search_result = self.solve_symbol(symbol)
            if search_result is not None:
                self.solved_symbols[symbol] = search_result
            return CantResolveSymbol(symbol)
        return None

    def query_symbol(
        self, symbol: ImportModuleName
    ) -> SymbolPath | CantResolveSymbol:
        result = self.solve_symbols({symbol})
        if result is None:
            return self.solved_symbols[symbol]
        else:
            return result

    def query_symbols(
        self, symbols: set[ImportModuleName]
    ) -> dict[ImportModuleName, SymbolPath] | CantResolveSymbol:
        result = self.solve_symbols(symbols)
        if result is None:
            return dict(
                (symbol, self.solved_symbols[symbol]) for symbol in symbols
            )
        else:
            return result


def build_path(splited: list[str]) -> Optional[Path]:
    if splited:
        prefix = splited[0]
        return Path(prefix, *splited)
    return None
