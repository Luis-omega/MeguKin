from typing import List, Optional, Union, Tuple, Set, Any
from collections import Counter

from MeguKin.SugaredSyntaxTree.Top import (
    Top,
    TopT,
    Declaration,
    DefinitionAndDeclaration,
    Definition,
    Constructor,
    DataType,
)
from MeguKin.SugaredSyntaxTree.PatternMatch import (
    PatternMatchT,
    PatternMatchVariable,
    PatternMatchConstructor,
)
from MeguKin.SugaredSyntaxTree.Expression import (
    ExpressionT,
    Expression,
    Int,
    Variable,
    Function,
    AnnotatedExpression,
    OperatorsWithoutMeaning,
    Application,
)

from MeguKin.ModuleLoad.Module import LoadedModules


class AST:
    pass


class ModuleCorrectnessError:
    pass


class DefinitionWithoutDeclarationFound(ModuleCorrectnessError):
    definition: Definition

    def __init__(self, definition: Definition):
        self.definition = definition


class DeclarationWithoutFollowingDefinition(ModuleCorrectnessError):
    declaration: Declaration
    nextFound: Top

    def __init__(self, declaration: Declaration, nextFound: Top):
        self.declaration = declaration
        self.nextFound = nextFound


class DuplicatedNames(ModuleCorrectnessError):
    names: List[str]

    def __init__(self, names: List[str]):
        self.names = names


def check_top_variables_have_defintions_and_types_together_and_join_them(
    moduleContent: List[TopT],
) -> Tuple[
    List[ModuleCorrectnessError], List[Union[DefinitionAndDeclaration, DataType]]
]:
    out: List[Union[DefinitionAndDeclaration, DataType]] = []
    errorOut: List[ModuleCorrectnessError] = []
    previousDeclaration: Optional[Declaration] = None
    for top in moduleContent:
        if previousDeclaration is None:
            match top:
                case Definition(name=_, expression=_):
                    errorOut.append(DefinitionWithoutDeclarationFound(top))
                case Declaration(name=_, _type=_):
                    previousDeclaration = top
                case _:
                    out.append(top)
        else:
            if isinstance(top, Definition):
                out.append(DefinitionAndDeclaration(top, previousDeclaration))
            else:
                errorOut.append(
                    DeclarationWithoutFollowingDefinition(previousDeclaration, top)
                )
                previousDeclaration = top if isinstance(top, Declaration) else None
    return (errorOut, out)


def get_definitionAndDeclarations(
    moduleContent: List[Union[DefinitionAndDeclaration, DataType]]
) -> List[DefinitionAndDeclaration]:
    return [i for i in moduleContent if isinstance(i, DefinitionAndDeclaration)]


def check_top_vaiables_have_only_one_definition(
    moduleContent: List[Union[DefinitionAndDeclaration, DataType]],
) -> Optional[ModuleCorrectnessError]:
    variableNames = [
        i.declaration.name for i in get_definitionAndDeclarations(moduleContent)
    ]
    variableNamesSet = set(variableNames)
    if len(variableNames) == variableNamesSet:
        return None
    else:
        return DuplicatedNames(
            [item for item, count in Counter(variableNames).items() if count > 1]
        )


# We include all the operators inside the body a expression as operators
# are defined at the top of the module or in imports
# so, operators are always free variables
def find_free_variables(expression: ExpressionT) -> Set[str]:
    match expression:
        case Int(value=_):
            return set([])
        case Variable(name=x):
            return set([x])
        case Application(function=f, argument=arg):
            return find_free_variables(f).union(find_free_variables(arg))
        case Function(pattern=p, value=val):
            return find_free_variables(val) - find_pattern_match_binded_variables(p)
        case AnnotatedExpression(expression=e, annotation=_):
            return find_free_variables(e)
        case OperatorsWithoutMeaning(listOfOperatorExpression=op_or_exp):
            free_vars = [
                find_free_variables(i) if isinstance(i, Expression) else i
                for i in op_or_exp
            ]
            return set().union(*free_vars)


def find_pattern_match_binded_variables(pattern: PatternMatchT) -> Set[str]:
    match pattern:
        case PatternMatchVariable(name=n):
            return set([n])
        case PatternMatchConstructor(name=n, patterns=p):
            return set().union(*[find_pattern_match_binded_variables(i) for i in p])


def semantic_analysis(desugared_trees: list[Top], modules: LoadedModules) -> list[AST]:
    pass
