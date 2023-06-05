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


def make_layout_test(example_plain: str, example_layout: str, symbol: str):
    parser = get_parser(symbol)
    plain_result = parser(example_plain)
    plain_ToSST = ToSST().transform(plain_result)
    layout_result = parser(example_layout)
    layout_ToSST = ToSST().transform(layout_result)
    print(plain_ToSST)
    print(layout_ToSST)
    assert plain_ToSST.compare(layout_ToSST)


class TestRecordLayout:
    @staticmethod
    def test_top_level():
        symbol = "top_variable_definition"
        plain = "a = {b:1}"
        # This first { can be missaligned since we don't have a
        # previous context
        layout = """
        a = 
            {   
             b 
              : 
               1
            }
        """
        make_layout_test(plain, layout, symbol)

    @staticmethod
    def test_record_single_item():
        symbol = "expression_record"
        plain = "{a:1}"
        # This first { can be missaligned since we don't have a
        # previous context
        layout = """
        {   
      a 
        : 
         1
        }
        """
        make_layout_test(plain, layout, symbol)

    @staticmethod
    def test_record_two_items():
        symbol = "expression_record"
        plain = "{a:1,b:2}"
        # This first { can be missaligned since we don't have a
        # previous context
        layout = """
        {   
      a 
        : 
         1
        ,b 
      :
        2
     }
        """
        make_layout_test(plain, layout, symbol)

    @staticmethod
    def test_record_tree_items():
        symbol = "expression_record"
        plain = "{a:1,b:2,c:3}"
        # This first { can be missaligned since we don't have a
        # previous context
        layout = """
        {   
            a 
                : 
                    1
           ,b 
              :
                    2
            ,c :
                    3
        }
        """
        make_layout_test(plain, layout, symbol)


class TestRecord:
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
    def test_record_item_single_multiple_lines():
        symbol = "expression_record"
        token = Token("", "", 0, 0, 0, 0, 0, 0)
        example = """
    {
      a :
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

    @staticmethod
    def test_record_multiple_items_same_line():
        symbol = "expression_record"
        token = Token("", "", 0, 0, 0, 0, 0, 0)
        example = """{a:1,b:2,c:3,d:4}"""
        expected = Record(
            [
                (
                    "a",
                    token2Range(token),
                    Literal(token.new_borrow_pos("INT", "1", token)),
                ),
                (
                    "b",
                    token2Range(token),
                    Literal(token.new_borrow_pos("INT", "2", token)),
                ),
                (
                    "c",
                    token2Range(token),
                    Literal(token.new_borrow_pos("INT", "3", token)),
                ),
                (
                    "d",
                    token2Range(token),
                    Literal(token.new_borrow_pos("INT", "4", token)),
                ),
            ]
        )
        make_test(example, symbol, expected)
