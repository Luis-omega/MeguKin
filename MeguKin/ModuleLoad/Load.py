from pathlib import Path

from lark import Lark

from MeguKin.ModuleLoad.Module import (
    ModuleRelations,
    ModuleLoadError,
    ModuleImports,
    LoadedModules,
    Module,
    ModulesPreviewInfo,
)


def find_module_imports(
    info: ModulesPreviewInfo, relations: ModuleRelations, lark: Lark
) -> ModuleLoadError | ModuleImports:
    pass


def find_modules_relations(
    roots: ModulesPreviewInfo, source_paths: list[Path], lark: Lark
) -> ModuleRelations | ModuleLoadError:
    pass


def solve_modules_path(
    modules_name: list[str], source_paths: list[Path]
) -> ModuleLoadError | ModulesPreviewInfo:
    pass


def load_module_imports(
    relations: ModuleRelations, imports: ModuleImports, lark: Lark
) -> ModuleLoadError | LoadedModules:
    pass


def load_modules(
    roots: list[str], source_paths: list[Path], lark: Lark
) -> ModuleLoadError | tuple[LoadedModules, dict[str, Module | ModuleLoadError]]:
    """
    @roots are the module names
    @source_paths are paths to look for sources, it must include target path
    """
    roots_paths_result = solve_modules_path(roots, source_paths)
    if isinstance(roots_paths_result, ModuleLoadError):
        return roots_paths_result

    relations_result = find_modules_relations(roots_paths_result, source_paths, lark)
    if isinstance(relations_result, ModuleLoadError):
        return relations_result

    imports_result = find_module_imports(roots_paths_result, relations_result, lark)
    if isinstance(imports_result, ModuleLoadError):
        return imports_result

    load_results = load_module_imports(relations_result, imports_result, lark)
    if isinstance(load_results, ModuleLoadError):
        return load_results

    return (load_results, load_modules_with(roots, load_results, lark))


def load_modules_with(
    root: list[str], loaded: LoadedModules, lark: Lark
) -> dict[str, Module | ModuleLoadError]:
    return dict(map(lambda name: (name, load_module_with(name, loaded, lark)), root))


def load_module_with(
    root: str, loaded: LoadedModules, lark: Lark
) -> ModuleLoadError | Module:
    pass
    # parse_result = parse(path, lark)
    # if isinstance(parse_result, FileLoadError):
    #    print(parse_result)
    #    return -1
    # file_info, trees = parse_result
    # sugared_trees = tree2sugared(trees, debug)
    # desugared_trees = desugar(trees, debug)
    # semantic_analysis_result = semantic_analysis(desugared_trees, debug)
