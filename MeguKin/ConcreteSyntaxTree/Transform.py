from MeguKin.SugaredSyntaxTree.Top import TopT, Module
from MeguKin.SugaredSyntaxTree.SST import MetaVar
import MeguKin.SugaredSyntaxTree as SST
from typing import NewType


class Top:
    pass


def desugar(elems: list[TopT]) -> Top:
    pass


# TODO: Add a function to check the free variables in the types inside expression
# like ` a = let (x:External.Type) = 2 in ...`


# FIXME:
def get_expression_free_variables(
    expression: SST.Expression.ExpressionT,
) -> list[MetaVar]:
    if isinstance(expression, MetaVar):
        return [expression]
    return []


def get_module_expression_free_variables(module: Module) -> list[MetaVar]:
    pass
