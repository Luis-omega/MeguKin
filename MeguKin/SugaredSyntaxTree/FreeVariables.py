from typing import Union, TypeVar, Optional
from dataclasses import dataclass


from MeguKin.SugaredSyntaxTree.Top import TopT, Module
from MeguKin.SugaredSyntaxTree.SST import MetaVar
from MeguKin.Error import MeguKinError
from MeguKin.File import Range
from MeguKin.SugaredSyntaxTree.PatternMatch import (
    PatternMatchT,
    PatternMatchLiteral,
    PatternMatchVariable,
    PatternMatchConstructor,
    PatternMatchConstructorName,
    PatternMatchHole,
)
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
T_ExpressionVariables = Variable | Operator | PrefixOperator | ConstructorName


class MeguKinFreeVariablesError(MeguKinError):
    pass


class MeguKinBoundVariablesError(MeguKinError):
    pass


@dataclass
class MeguKinShadowVariables(MeguKinBoundVariablesError):
    variables: set[Variable]


# FIXME
def get_bound_variables(
    pattern: PatternMatchT,
) -> MeguKinShadowVariables | set[Variable]:
    match pattern:
        case PatternMatchLiteral():
            return set()
        case PatternMatchHole():
            return set()
        case PatternMatchVariable(prefix=prefix, name=name, _range=_range):
            return {Variable(prefix, name, _range)}
        case PatternMatchConstructor(name=name, patterns=patterns):
            out: set[Variable] = set()
            for pattern in patterns:
                new_bounds = get_bound_variables(pattern)
                if isinstance(new_bounds, MeguKinShadowVariables):
                    return new_bounds
                intersection = out.intersection(out, new_bounds)
                if intersection:
                    error = MeguKinShadowVariables(intersection)
                    return error
                out = out.union(new_bounds)
            return out
        case _:
            return set()


# FIXME:
def get_expression_free_variables(
    expression: ExpressionT,
) -> MeguKinShadowVariables | set[T_ExpressionVariables]:
    if isinstance(expression, MetaVar):
        return set([expression])
    print("entering ", type(expression))
    match expression:
        case Literal():
            return set()
        case RecordUpdate(_map=_map):
            item: tuple[Variable, Range, ExpressionT]
            free_vars: set[T_ExpressionVariables] = set()
            for item in _map:
                inner_free_vars = get_expression_free_variables(item[2])
                if isinstance(inner_free_vars, MeguKinShadowVariables):
                    return inner_free_vars
                free_vars = free_vars.union(inner_free_vars)
            return free_vars
        case Record(_map=_map):
            item2: tuple[Variable, Range, Optional[ExpressionT]]
            free_vars2: set[T_ExpressionVariables] = set()
            for item2 in _map:
                if item2[2] is None:
                    free_vars2 = free_vars2.union(set([item2[0]]))
                else:
                    inner_free_vars = get_expression_free_variables(item2[2])
                    if isinstance(inner_free_vars, MeguKinShadowVariables):
                        return inner_free_vars
                    free_vars2 = free_vars2.union(inner_free_vars)
            return free_vars2
        case Selector(expression=exp):
            return get_expression_free_variables(exp)
        case AnnotatedExpression(expression=expression):
            return get_expression_free_variables(expression)
        case ExpressionTypeArgument():
            return set()
        case Application(function=function, argument=argument):
            first = get_expression_free_variables(function)
            if isinstance(first, MeguKinShadowVariables):
                return first
            second = get_expression_free_variables(argument)
            if isinstance(second, MeguKinShadowVariables):
                return second
            return first.union(second)
        case ExpressionMeaninglessOperatorApplications(
            applications=applications
        ):
            out: set[T_ExpressionVariables] = set()
            for application in applications:
                inner_free = get_expression_free_variables(application)
                if isinstance(inner_free, MeguKinShadowVariables):
                    return inner_free
                out = out.union(inner_free)
            return out
        case CaseCase(
            pattern=pattern,
            expression=expression,
        ):
            maybe_bound = get_bound_variables(pattern)
            if isinstance(maybe_bound, MeguKinShadowVariables):
                return maybe_bound
            maybe_free = get_expression_free_variables(expression)
            if isinstance(maybe_free, MeguKinShadowVariables):
                return maybe_free
            return maybe_free - maybe_bound
    return set()


def get_module_expression_free_variables(module: Module) -> list[MetaVar]:
    pass
