from typing import Iterator, Callable, Optional, TypeVar

import pytest
from lark import Lark, Tree
import lark
from pathlib import Path

from MeguKin.Parser.Parser import load_grammar
from MeguKin.Parser.Token import Token
from MeguKin.SugaredSyntaxTree.Transform import ToSST


def get_parser(symbol: str) -> Callable[[str], lark.ParseTree]:
    lark_parser = load_grammar(True, [symbol])
    if not isinstance(lark_parser, Lark):
        raise Exception(f"Can't get lexer for tests {lark_parser}")
    return lark_parser.parse


T = TypeVar("T")


def make_test(
    example: str,
    symbol: str,
    expected: T,
):
    parser = get_parser(symbol)
    parser_result = parser(example)
    print(parser_result.pretty())
    result: T = ToSST().transform(parser_result)
    assert expected == result


class TestLet:
    @staticmethod
    def test_record_item_single_same_line():
        symbol = "expression_record"
        example = "{a:1}"
        expected = Tree("expression_let", [])
        make_test(example, symbol, expected)

    # @staticmethod
    # def test_let_sigle_line():
    #     symbol = "expression_let"
    #     example = "let b = c in d"
    #     expected = Tree("expression_let", [])
    #     make_test(example, symbol, expected)
