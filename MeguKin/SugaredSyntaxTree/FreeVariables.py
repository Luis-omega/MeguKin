from typing import Union, TypeVar, Optional
from MeguKin.SugaredSyntaxTree.Top import TopT, Module
from MeguKin.SugaredSyntaxTree.SST import MetaVar
from MeguKin.Error import MeguKinError
from MeguKin.File import Range
from typing import NewType
from MeguKin.SugaredSyntaxTree.Expression import (
    Variable,
    Literal,
    Operator,
    PrefixOperator,
    ConstructorName,
    RecordUpdate,
    Record,
    Selector,
    Application,
    AnnotatedExpression,
    ExpressionTypeArgument,
    ExpressionMeaninglessOperatorApplications,
    CaseCase,
    Case,
    Function,
    LetBinding,
    Let,
    ExpressionT,
)

T = TypeVar("T")


class MeguKinFreeVariablesError(MeguKinError):
    pass


# TODO: Add a function to check the free variables in the types inside expression
# like ` a = let (x:External.Type) = 2 in ...`


# FIXME:
def get_expression_free_variables(
    expression: ExpressionT,
) -> set[MetaVar]:
    if isinstance(expression, MetaVar):
        return set([expression])
    print("entering ", type(expression))
    match expression:
        case Literal():
            return set()
        case RecordUpdate(_map=_map):
            item: tuple[Variable, Range, ExpressionT]
            free_vars: set[MetaVar] = set()
            for item in _map:
                free_vars = free_vars.union(
                    get_expression_free_variables(item[2])
                )
            return free_vars
        case Record(_map=_map):
            item2: tuple[Variable, Range, Optional[ExpressionT]]
            free_vars2: set[MetaVar] = set()
            for item2 in _map:
                if item2[2] is None:
                    free_vars2 = free_vars2.union(set([item2[0]]))
                else:
                    free_vars2 = free_vars2.union(
                        get_expression_free_variables(item2[2])
                    )
            return free_vars2
        case Selector(expression=exp):
            return get_expression_free_variables(exp)
    return set()


def get_module_expression_free_variables(module: Module) -> list[MetaVar]:
    pass
