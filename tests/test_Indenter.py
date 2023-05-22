from typing import Iterator, Callable

import pytest
from lark import Lark, Token
from pathlib import Path

from MeguKin.Parser.Parser import load_grammar
from MeguKin.File import FileInfo
from MeguKin.Parser.Indentation import handle_indentation, IndenterState, Context, Item

lark_parser = None


# this must memoize the lexer generated to avoid
# multiple initialization and load of the same grammar
def get_lexer() -> Callable[[str], Iterator[Token]]:
    global lark_parser
    if lark_parser is None:
        lark_parser = load_grammar(True)
        if not isinstance(lark_parser, Lark):
            raise Exception("Can't get lexer for tests")
    return lark_parser.lex


def make_test(example: str, state: IndenterState, expected: str):
    lexer = get_lexer()
    info = FileInfo("test", Path("test"))
    regular_tokens = list(lexer(example))
    new_tokens = list(handle_indentation(info, state, regular_tokens))
    print(new_tokens)
    tokens_type = [token.type for token in new_tokens]
    assert expected == tokens_type


class TestEqualAtRoot:
    @staticmethod
    def test_same_line_single():
        example = "a = b"
        state = IndenterState()
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "EQUAL_TOP_END",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_same_line():
        example = "a = b c d"
        state = IndenterState()
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "EQUAL_TOP_END",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_next_line_single():
        example = "a = \n b"
        state = IndenterState()
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "EQUAL_TOP_END",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_next_line():
        example = "a = \n b c \n  d"
        state = IndenterState()
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "EQUAL_TOP_END",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_equal_indentation_wont_close_context():
        example = "a = \n b \n c"
        state = IndenterState()
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "EQUAL_TOP_END",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_lower_indentation_close_context():
        example = "a = \n  b \n c"
        state = IndenterState()
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "EQUAL_TOP_END",
            "VARIABLE_IDENTIFIER",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_fail_incomplete():
        example = "a = \n"
        state = IndenterState()
        expected = ["VARIABLE_IDENTIFIER", "EQUAL", "IndentationError"]
        make_test(example, state, expected)


class TestEqualAtLet:
    # keep a position in sync with the item in state in every example
    # the IndentationError happens because "let" context wasn't close by "in"
    @staticmethod
    def test_same_line_single():
        example = " a = b"
        state = IndenterState()
        state.append(Item(Context.LET, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "EQUAL_END",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_same_line():
        example = " a = b c d"
        state = IndenterState()
        state.append(Item(Context.LET, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "EQUAL_END",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_next_line_single():
        example = " a = \n    b"
        state = IndenterState()
        state.append(Item(Context.LET, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "EQUAL_END",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_next_line_single_fail():
        example = " a = \n b"
        state = IndenterState()
        state.append(Item(Context.LET, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_next_line():
        example = " a = \n    b c \n    d"
        state = IndenterState()
        state.append(Item(Context.LET, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "EQUAL_END",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_equal_indentation_wont_close_context():
        example = " a = \n    b \n    c"
        state = IndenterState()
        state.append(Item(Context.LET, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "EQUAL_END",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_lower_indentation_close_context():
        example = " a = \n     b \n    c"
        state = IndenterState()
        state.append(Item(Context.LET, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "EQUAL_END",
            "VARIABLE_IDENTIFIER",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_fail_incomplete():
        example = " a = \n"
        state = IndenterState()
        state.append(Item(Context.LET, 1, 0))
        expected = ["VARIABLE_IDENTIFIER", "EQUAL", "IndentationError"]
        make_test(example, state, expected)


class TestEqualAtRecord:
    # keep a position in sync with the item in state in every example
    # the IndentationError happens because "let" context wasn't close by "in"
    @staticmethod
    def test_same_line_single():
        example = " a = b"
        state = IndenterState()
        state.append(Item(Context.RECORD, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_same_line():
        example = " a = b c d"
        state = IndenterState()
        state.append(Item(Context.RECORD, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_next_line_single():
        example = " a = \n    b"
        state = IndenterState()
        state.append(Item(Context.RECORD, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_next_line_single_fail():
        example = " a = \n b"
        state = IndenterState()
        state.append(Item(Context.RECORD, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_next_line():
        example = " a = \n    b c \n    d"
        state = IndenterState()
        state.append(Item(Context.RECORD, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_equal_indentation_wont_close_context():
        example = " a = \n    b \n    c"
        state = IndenterState()
        state.append(Item(Context.RECORD, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_lower_indentation_close_context():
        example = " a = \n     b \n    c"
        state = IndenterState()
        state.append(Item(Context.RECORD, 1, 0))
        expected = [
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "VARIABLE_IDENTIFIER",
            "IndentationError",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_fail_incomplete():
        example = " a = \n"
        state = IndenterState()
        state.append(Item(Context.RECORD, 1, 0))
        expected = ["VARIABLE_IDENTIFIER", "EQUAL", "IndentationError"]
        make_test(example, state, expected)


class TestLet:
    @staticmethod
    def test_single_line():
        example = "let a = b in c"
        state = IndenterState()
        expected = [
            "LET",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "EQUAL_END",
            "IN",
            "VARIABLE_IDENTIFIER",
            "IN_END",
        ]
        make_test(example, state, expected)
