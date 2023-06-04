from typing import Iterator, Callable, Optional, TypeVar

import pytest
from lark import Lark, Tree
import lark
from pathlib import Path

from MeguKin.Parser.Parser import load_grammar
from MeguKin.File import token2Range
from MeguKin.Parser.Token import Token
from MeguKin.SugaredSyntaxTree.Transform import ToSST
from MeguKin.SugaredSyntaxTree.SST import SST
from MeguKin.SugaredSyntaxTree.Expression import Record, Literal


def get_parser(symbol: str) -> Callable[[str], lark.ParseTree]:
    lark_parser = load_grammar(True, [symbol])
    if not isinstance(lark_parser, Lark):
        raise Exception(f"Can't get lexer for tests {lark_parser}")
    return lark_parser.parse


def make_test(
    example: str,
    symbol: str,
    expected: SST,
):
    parser = get_parser(symbol)
    parser_result = parser(example)
    print(parser_result.pretty())
    result: SST = ToSST().transform(parser_result)
    assert expected.compare(result)


class TestLet:
    @staticmethod
    def test_record_item_single_same_line():
        symbol = "expression_record"
        token = Token("", "", 0, 0, 0, 0, 0, 0)
        example = "{a:1}"
        expected = Record(
            [
                (
                    "a",
                    token2Range(token),
                    Literal(token.new_borrow_pos("INT", "1", token)),
                )
            ]
        )
        make_test(example, symbol, expected)

    @staticmethod
    def test_record_item_single_two_lines():
        symbol = "expression_record"
        token = Token("", "", 0, 0, 0, 0, 0, 0)
        example = """
    {a:
        1
       }"""
        expected = Record(
            [
                (
                    "a",
                    token2Range(token),
                    Literal(token.new_borrow_pos("INT", "1", token)),
                )
            ]
        )
        make_test(example, symbol, expected)
