from lark import Lark
import lark
from typing import Callable

from MeguKin.Parser.Parser import parse_string
from MeguKin.Parser import Parser
from MeguKin.Parser.Parser import load_grammar


def get_parser(symbol: str) -> Callable[[str], lark.ParseTree]:
    lark_parser = load_grammar(True, [symbol])
    if not isinstance(lark_parser, Lark):
        raise Exception(f"Can't get lexer for tests {lark_parser}")
    return lark_parser.parse


def initialize():
    with open("grammar_test", "r") as file:
        to_parse = file.read()
    return to_parse


def test_some():
    to_parser = initialize()
    print(to_parser)
    parser = get_parser("top")
    result = parser(to_parser)
    print(result)
    assert 1 == 2
