from pathlib import Path
from typing import NamedTuple, NewType

from MeguKin.Error import MeguKinError


class ModuleLoadError(MeguKinError):
    pass


class Module:
    pass


class ModuleRelations:
    pass


class ModuleImports:
    pass


class LoadedModules:
    pass


ModulePreviewInfo = NamedTuple("ModulePreviewInfo", [("name", str), ("path", Path)])

ModulesPreviewInfo = NewType("ModulesPreviewInfo", dict[str, Path])
