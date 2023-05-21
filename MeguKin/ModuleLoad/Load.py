from pathlib import Path

from MeguKin.ModuleLoad.Module import (
    ModuleRelations,
    ModuleLoadError,
    ModuleImports,
    LoadedModules,
    Module,
)


def find_modules_relations(
    root: list[str], source_paths: list[Path]
) -> ModuleRelations | ModuleLoadError:
    pass


def find_module_imports(path: Path) -> ModuleLoadError | ModuleImports:
    pass


def load_module_imports(
    relations: ModuleRelations, imports: ModuleImports
) -> ModuleLoadError | LoadedModules:
    pass


def compile_modules(
    root: list[str], load: LoadedModules
) -> ModuleLoadError | dict[str, Module]:
    pass
