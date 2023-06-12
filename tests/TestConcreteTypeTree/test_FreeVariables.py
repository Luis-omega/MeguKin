import pytest

from MeguKin.ConcreteSyntaxTree.Transform import get_expression_free_variables
import MeguKin.SugaredSyntaxTree as SST
from MeguKin.SugaredSyntaxTree.SST import MetaRecord, MetaVar
from MeguKin.Parser.Token import Token
from MeguKin.SugaredSyntaxTree.Type import TypeVariable
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
)
from MeguKin.File import Range

empty_range = Range(0, 0, 0, 0, 0, 0)


def make_test(expression: SST.Expression.ExpressionT, expected: set[MetaVar]):
    result = get_expression_free_variables(expression)
    assert result == expected


def test_single_literal():
    lit = Literal(Token("INT", "1", 0, 0, 0, 0, 0, 0))
    make_test(lit, set())


def make_var(name: str) -> Variable:
    return Variable([], name, empty_range)


var_x = make_var("x")
var_y = make_var("y")
var_z = make_var("z")
var_w = make_var("w")
var_t = make_var("t")


@pytest.mark.parametrize(
    "cls,x",
    [
        (Variable, "x"),
        (Operator, "+"),
        (PrefixOperator, "*"),
        (ConstructorName, "C"),
    ],
)
def test_single_meta_var(cls, x: str):
    expression = cls([], x, empty_range)
    make_test(expression, set([expression]))


@pytest.mark.parametrize(
    "expression,expected",
    [
        (RecordUpdate([(var_y, empty_range, var_x)]), set([var_x])),
        (
            RecordUpdate(
                [(var_z, empty_range, var_x), (var_z, empty_range, var_y)]
            ),
            set([var_x, var_y]),
        ),
        (Record(([(var_z, empty_range, var_x)])), set([var_x])),
        (
            Record([(var_z, empty_range, var_x), (var_z, empty_range, var_y)]),
            set([var_x, var_y]),
        ),
    ],
)
def test_record_update_and_regular(expression, expected):
    make_test(expression, expected)


@pytest.mark.parametrize(
    "expression,expected",
    [
        (Selector(var_x, "y", empty_range), set([var_x])),
    ],
)
def test_record_selector(expression, expected):
    make_test(expression, expected)


@pytest.mark.parametrize(
    "expression,expected",
    [
        (
            AnnotatedExpression(var_x, TypeVariable([], "t_var", empty_range)),
            set([var_x]),
        )
    ],
)
def test_annotated_expression(expression, expected):
    pytest.skip()
    make_test(expression, expected)


@pytest.mark.parametrize(
    "expression,expected",
    [
        (
            ExpressionTypeArgument(
                TypeVariable([], "t_var", empty_range), empty_range
            ),
            set([TypeVariable([], "t_var", empty_range)]),
        )
    ],
)
def test_expression_type_argument(expression, expected):
    pytest.skip()
    make_test(expression, expected)


@pytest.mark.parametrize(
    "expression,expected",
    [
        (
            Application(
                var_x,
                var_y,
                empty_range,
            ),
            set([var_x, var_y]),
        ),
    ],
)
def test_application(expression, expected):
    pytest.skip()
    make_test(expression, expected)


@pytest.mark.parametrize(
    "expression,expected",
    [
        (ExpressionMeaninglessOperatorApplications([], empty_range), []),
        (
            ExpressionMeaninglessOperatorApplications([var_x], empty_range),
            set([var_x]),
        ),
        (
            ExpressionMeaninglessOperatorApplications(
                [var_x, var_y, var_z], empty_range
            ),
            set([var_x, var_y, var_z]),
        ),
    ],
)
def test_expression_operators(expression, expected):
    pytest.skip()
    make_test(expression, expected)


# @pytest.mark.parametrize(
#    "expression,expected",
#    [
#    (
#        CaseCase(pattern,expression)
#        , [var_x])
#    ],
# )
# def test_expression_case_case(expression, expected):
#    make_test(expression, expected)
