from typing import Iterator, Callable, Optional, TypeVar
import pytest
from pathlib import Path
from hypothesis import given, settings, HealthCheck
import logging

from lark import Lark, Tree
import lark

from MeguKin.Parser.Parser import load_grammar
from MeguKin.File import token2Range
from MeguKin.Parser.Token import Token
from MeguKin.SugaredSyntaxTree.Transform import ToSST
from MeguKin.SugaredSyntaxTree.SST import SST
from MeguKin.SugaredSyntaxTree.Top import Definition
from MeguKin.SugaredSyntaxTree.Expression import (
    Record,
    Literal,
    ExpressionT,
    Expression,
    Variable,
)
from MeguKin.Pretty import defaultSettings, pretty
from MeguKin.File import Range


from .SST_generators import gen_expression_variable
from .lark_generator import gen_MeguKin


def get_parser(symbol: str) -> Callable[[str], lark.ParseTree]:
    lark_parser = load_grammar(True, [symbol])
    if not isinstance(lark_parser, Lark):
        raise Exception(f"Can't get lexer for tests {lark_parser}")
    return lark_parser.parse


def make_test(
    example: str,
    symbol: str,
):
    parser = get_parser(symbol)
    parser_result = parser(example)
    result: SST = ToSST().transform(parser_result)
    doc1 = result.to_document(defaultSettings)
    p = pretty(defaultSettings, doc1)
    print(p)
    parse_result2 = parser(p)
    result2: SST = ToSST().transform(parse_result2)
    doc2 = result2.to_document(defaultSettings)
    assert result.compare(result2)
    assert doc1 == doc2


def make_test2(example: SST, symbol: str):
    parser = get_parser(symbol)
    doc1 = example.to_document(defaultSettings)
    p1 = pretty(defaultSettings, doc1)
    parse_result = parser(p1)
    result: SST = ToSST().transform(parse_result)
    assert example.compare(result)
    doc2 = result.to_document(defaultSettings)
    assert doc1 == doc2
    p2 = pretty(defaultSettings, doc2)
    assert p1 == p2


symbol_to_use = "top_variable_definition"
lark_p = load_grammar(True, [symbol_to_use])


def make_test3(p1: str, symbol: str):
    print("argument passed: ", p1)
    parse_result = lark_p.parse(p1, symbol)
    print("first parser succes")
    result: SST = ToSST().transform(parse_result)
    doc2 = result.to_document(defaultSettings)
    p2 = pretty(defaultSettings, doc2)
    print("prettyfied is: ", p2)
    parse_result2 = lark_p.parse(p2, symbol)
    result2: SST = ToSST().transform(parse_result2)
    doc3 = result2.to_document(defaultSettings)
    p3 = pretty(defaultSettings, doc3)
    assert p2 == p3


@given(gen_MeguKin(lark_p, symbol_to_use))
@settings(suppress_health_check=list(HealthCheck))
def test_expression_variable_megu(
    caplog,
    var: str,
):
    pytest.skip()
    caplog.set_level(logging.INFO)
    symbol = symbol_to_use
    make_test3(var, symbol)


# @given(gen_expression_variable)
# def test_expression_variable(var: Variable):
#    symbol = "expression_variable"
#    make_test2(var, symbol)
#
#
# def test_simple_constant_asignation():
#    symbol = "top_variable_definition"
#    example = "a=1"
#    make_test(example, symbol)
#
#
# def test_simple_variable_asignation():
#    symbol = "top_variable_definition"
#    example = "a=asdfasdf"
#    make_test(example, symbol)
#
#
# def test_simple_constant_asignation_indent():
#    symbol = "top_variable_definition"
#    example = "a=\n asdfasdf"
#    make_test(example, symbol)
#
#
# def test_asignation_record():
#    symbol = "top_variable_definition"
#    example = "a={b:c}"
#    make_test(example, symbol)
#
#
# def test_asignation_let():
#    symbol = "top_variable_definition"
#    example = "a=let b = c in d"
#    make_test(example, symbol)
