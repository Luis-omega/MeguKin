from pathlib import Path

import pytest

from MeguKin.SugaredSyntaxTree.Top import ParsedModule, Exports
from MeguKin.SugaredSyntaxTree.Module import (
    ExportModuleName,
    ImportModuleName,
    ImportModule,
)
from MeguKin.File import Range

from MeguKin.ModuleLoad.TaskManager import (
    PendingToGetModules,
    find_back_edge,
)
from MeguKin.ModuleLoad.SymbolResolution import SymbolPath


empty_range = Range(0, 0, 0, 0, 0, 0)
empty_symbol = SymbolPath(Path(""))


def make_module_export_name(name: str) -> ExportModuleName:
    rsplited_name = name.rsplit(".", 1)
    if len(rsplited_name) == 1:
        return ExportModuleName([], name, empty_range)
    else:
        export_prefix, export_name = rsplited_name
        return ExportModuleName(
            export_prefix.split("."), export_name, empty_range
        )


def make_module_impor_name(name: str) -> ImportModuleName:
    rsplited_name = name.rsplit(".", 1)
    if len(rsplited_name) == 1:
        return ImportModuleName([], name, empty_range)
    else:
        import_prefix, import_name = rsplited_name
        return ImportModuleName(
            import_prefix.split("."), import_name, empty_range
        )


def make_module_export(name: ExportModuleName) -> Exports:
    return Exports(empty_range, name, [])


def make_module_import(
    name: ImportModuleName,
) -> ImportModule:
    return ImportModule(empty_range, name, [], None)


def make_parsed_module(
    name: str, import_names: list[str]
) -> tuple[ExportModuleName, ParsedModule]:
    export_name = make_module_export_name(name)
    export = make_module_export(export_name)

    imports_name = [make_module_impor_name(name) for name in import_names]
    imports = [make_module_import(name) for name in imports_name]

    return export_name, ParsedModule(empty_range, export, imports, [], [], [])


name1, module1 = make_parsed_module("Maybe", ["Functor"])
name2, module2 = make_parsed_module("Either", ["Functor"])
name3, module3 = make_parsed_module("Functor", [])

name4, module4 = make_parsed_module("User1", ["User2"])
name5, module5 = make_parsed_module("User2", ["User1"])
name6, module6 = make_parsed_module("User3", ["User1", "User2"])

name7, module7 = make_parsed_module("Union", ["Maybe", "Either", "User3"])


def test_single_module():
    initia_edge = name3
    arg_dict = dict([(name3, PendingToGetModules(empty_symbol, module3))])
    result = find_back_edge([initia_edge], arg_dict)
    assert result is None


def test_three_modules():
    initia_edge = name1
    arg_dict = dict(
        [
            (name1, PendingToGetModules(empty_symbol, module1)),
            (name2, PendingToGetModules(empty_symbol, module2)),
            (name3, PendingToGetModules(empty_symbol, module3)),
        ]
    )
    result = find_back_edge([initia_edge], arg_dict)
    assert result is None


def test_tree_modules_dependency():
    initia_edge = name6
    arg_dict = dict(
        [
            (name4, PendingToGetModules(empty_symbol, module4)),
            (name5, PendingToGetModules(empty_symbol, module5)),
            (name6, PendingToGetModules(empty_symbol, module6)),
        ]
    )
    result = find_back_edge([initia_edge], arg_dict)
    assert (name5, name6) == result


def test_seven_modules_dependency():
    initia_edge = name7
    arg_dict = dict(
        [
            (name1, PendingToGetModules(empty_symbol, module1)),
            (name2, PendingToGetModules(empty_symbol, module2)),
            (name3, PendingToGetModules(empty_symbol, module3)),
            (name4, PendingToGetModules(empty_symbol, module4)),
            (name5, PendingToGetModules(empty_symbol, module5)),
            (name6, PendingToGetModules(empty_symbol, module6)),
            (name7, PendingToGetModules(empty_symbol, module7)),
        ]
    )
    result = find_back_edge([initia_edge], arg_dict)
    assert (name5, name6) == result
