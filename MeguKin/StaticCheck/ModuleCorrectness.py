from typing import List, Optional, Union, Tuple, Set, Any
from dataclasses import dataclass
from collections import Counter

from MeguKin.SugaredSyntaxTree.Top import (
    Top,
    TopT,
    Declaration,
    Definition,
    DataType,
    Module,
)
from MeguKin.SugaredSyntaxTree.PatternMatch import (
    PatternMatchT,
    PatternMatchVariable,
    PatternMatchConstructor,
)
from MeguKin.SugaredSyntaxTree.Expression import (
    ExpressionT,
    Expression,
    Variable,
    Function,
    AnnotatedExpression,
    Application,
)

from MeguKin.ModuleLoad.Module import LoadedModules


class AST:
    pass


class ModuleCorrectnessError:
    pass


@dataclass
class DefinitionWithoutDeclarationFound(ModuleCorrectnessError):
    definition: Definition


@dataclass
class DeclarationWithoutFollowingDefinition(ModuleCorrectnessError):
    declaration: Declaration
    nextFound: Top


@dataclass
class DuplicatedNames(ModuleCorrectnessError):
    names: List[str]

    def __init__(self, names: List[str]):
        self.names = names


@dataclass
class DuplicatedDeclarations(ModuleCorrectnessError):
    declarations: List[Declaration]


# TODO: This is to find definitions not being contiguos
# at top level, so we find in the module if there is
# something between two defintions.
def check_definitios_are_contiguos():
    pass


def find_duplicated_declarations(
    _map: dict[str, Declaration | DuplicatedDeclarations]
) -> tuple[dict[str, Declaration], list[DuplicatedDeclarations]]:
    regular: list[tuple[str, Declaration]] = []
    bad = []
    # we can just validate data by checking that
    # we don't have a DuplicatedDeclarations
    # but then mypy would comply and I prefer
    # types over performance for this compiler v1.
    # KISS.
    for k, item in _map.items():
        if isinstance(item, Declaration):
            regular.append((k, item))
        else:
            bad.append(item)

    return (dict(regular), bad)


def group_module_declarations(
    module: Module,
) -> dict[str, Declaration | DuplicatedDeclarations]:
    out_dict: dict[str, Declaration | DuplicatedDeclarations] = dict()
    for declaration in module.declarations:
        if declaration.name in out_dict:
            old_item = out_dict[declaration.name]
            if isinstance(old_item, Declaration):
                out_dict[declaration.name] = DuplicatedDeclarations(
                    [old_item, declaration]
                )
            else:
                old_item.declarations.append(declaration)
        else:
            out_dict[declaration.name] = declaration
    return out_dict


def semantic_analysis(
    desugared_trees: Module, modules: LoadedModules
) -> list[AST]:
    pass
