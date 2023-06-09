import pytest

from MeguKin.ConcreteSyntaxTree.Transform import get_expression_free_variables
import MeguKin.SugaredSyntaxTree as SST
from MeguKin.SugaredSyntaxTree.SST import MetaRecord
from MeguKin.Parser.Token import Token
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
)
from MeguKin.File import Range

empty_range = Range(0, 0, 0, 0, 0, 0)


def make_test(
    expression: SST.Expression.ExpressionT, expected: list[SST.SST.MetaVar]
):
    result = get_expression_free_variables(expression)
    assert SST.SST.compare_list(result, expected, SST.SST.MetaVar.compare)


def test_single_literal():
    lit = Literal(Token("INT", "1", 0, 0, 0, 0, 0, 0))
    make_test(lit, [lit])


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
    make_test(expression, [expression])


@pytest.mark.parametrize(
    "expression",
    [
        (RecordUpdate([("hi", empty_range, Variable([], "x", empty_range))])),
        (Record(([("hi", empty_range, Variable([], "x", empty_range))]))),
    ],
)
def test_record_update(expression):
    make_test(expression, [])


@pytest.mark.parametrize(
    "expression,expected",
    [
        (Variable([], "x", empty_range), [(Variable([], "x", empty_range))]),
        (
            Application(
                Variable([], "x", empty_range),
                Variable([], "y", empty_range),
                empty_range,
            ),
            [
                (Variable([], "x", empty_range)),
                (Variable([], "y", empty_range)),
            ],
        ),
    ],
)
def test_selector_no_empty(expression, expected):
    make_test(Selector(expression, ["a", "b"], empty_range), expected)
