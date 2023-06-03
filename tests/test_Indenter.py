from typing import Iterator, Callable, Optional

import pytest
from lark import Lark
import lark
from pathlib import Path

from MeguKin.Parser.Parser import load_grammar
from MeguKin.Parser.Token import Token
from MeguKin.File import FileInfo
from MeguKin.Parser.Indentation import handle_indentation, Context, Stream, ContextStack

lark_parser = None


# this must memoize the lexer generated to avoid
# multiple initialization and load of the same grammar
def get_lexer() -> Callable[[str], Iterator[lark.Token]]:
    global lark_parser
    if lark_parser is None:
        lark_parser = load_grammar(True)
        if not isinstance(lark_parser, Lark):
            raise Exception("Can't get lexer for tests")
    return lark_parser.lex


def make_test(example: str, state: Optional[ContextStack], expected: list[str]):
    lexer = get_lexer()
    info = FileInfo("test", Path("test"))
    regular_tokens = Stream(lexer(example))
    # our_tokens = Stream(map(lambda x: Token.from_lark(x),regular_tokens))
    new_tokens = list(handle_indentation(info, state, regular_tokens))
    print(new_tokens)
    tokens_type = [token.type for token in new_tokens]
    print(new_tokens[-1].column, new_tokens[-1].line)
    assert expected == tokens_type


class TestLet:
    @staticmethod
    def test_same_line_single():
        example = "let a = b in w\nc"
        state = None
        expected = [
            "LET",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "VARIABLE_IDENTIFIER",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_same_line_nested_two_in_let():
        example = "let a = let b = c in d in e \n1"
        state = None
        expected = [
            # let a =
            "LET",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "LAYOUT_START",
            # let b =
            "LET",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "LAYOUT_START",
            # c in d
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            # in e
            "LAYOUT_END",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "INT",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_same_line_nested_two_in_in():
        example = "let a = b in let c = d in e\n1"
        state = None
        expected = [
            # let a = b in
            "LET",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            # let c = d in e
            "LET",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "INT",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_same_line_nested_tree_in_in():
        example = "let a = b in let c = d in let e = f in g\n1"
        state = None
        expected = [
            # let a = b in
            "LET",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            # let c = d in
            "LET",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            # let e = f in g
            "LET",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "LAYOUT_END",
            "INT",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_different_line_single():
        example = """
    let 
        a = b
    in
        c
1
        """
        state = None
        expected = [
            "LET",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "INT",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_different_line_two():
        example = """
    let 
        a = b
    in
        let c = d
        in
            e
         f
1
        """
        state = None
        expected = [
            # let a = b in
            "LET",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            # let c = d in e
            "LET",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            # f
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "INT",
        ]
        make_test(example, state, expected)


# class TestEqualAtRoot:
#    @staticmethod
#    def test_same_line_single():
#        example = "a = b"
#        state = IndenterState()
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_TOP_END",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_same_line():
#        example = "a = b c d"
#        state = IndenterState()
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_TOP_END",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_next_line_single():
#        example = "a = \n b"
#        state = IndenterState()
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_TOP_END",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_next_line():
#        example = "a = \n b c \n  d"
#        state = IndenterState()
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_TOP_END",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_equal_indentation_wont_close_context():
#        example = "a = \n b \n c"
#        state = IndenterState()
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_TOP_END",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_lower_indentation_close_context():
#        example = "a = \n  b \n c"
#        state = IndenterState()
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_TOP_END",
#            "VARIABLE_IDENTIFIER",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_fail_incomplete():
#        example = "a = \n"
#        state = IndenterState()
#        expected = ["VARIABLE_IDENTIFIER", "EQUAL", "IndentationError"]
#        make_test(example, state, expected)
#
#
# class TestEqualAtLet:
#    # keep a position in sync with the item in state in every example
#    # the IndentationError happens because "let" context wasn't close by "in"
#    @staticmethod
#    def test_same_line_single():
#        example = " a = b"
#        state = IndenterState()
#        state.append(Item(Context.LET, 1, 0, Token("LET", "let")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_same_line():
#        example = " a = b c d"
#        state = IndenterState()
#        state.append(Item(Context.LET, 1, 0, Token("LET", "let")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_next_line_single():
#        example = " a = \n    b"
#        state = IndenterState()
#        state.append(Item(Context.LET, 1, 0, Token("LET", "let")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_next_line_single_fail():
#        example = " a = \n b"
#        state = IndenterState()
#        state.append(Item(Context.LET, 1, 0, Token("LET", "let")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_next_line():
#        example = " a = \n    b c \n    d"
#        state = IndenterState()
#        state.append(Item(Context.LET, 1, 0, Token("LET", "let")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_equal_indentation_wont_close_context():
#        example = " a = \n    b \n    c"
#        state = IndenterState()
#        state.append(Item(Context.LET, 1, 0, Token("LET", "let")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_lower_indentation_close_context():
#        example = " a = \n     b \n    c"
#        state = IndenterState()
#        state.append(Item(Context.LET, 1, 0, Token("LET", "let")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "VARIABLE_IDENTIFIER",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_fail_incomplete():
#        example = " a = \n"
#        state = IndenterState()
#        state.append(Item(Context.LET, 1, 0, Token("LET", "let")))
#        expected = ["VARIABLE_IDENTIFIER", "EQUAL", "IndentationError"]
#        make_test(example, state, expected)
#
#
# class TestEqualAtRecord:
#    # keep a position in sync with the item in state in every example
#    # the IndentationError happens because "let" context wasn't close by "in"
#    @staticmethod
#    def test_same_line_single():
#        example = " a = b"
#        state = IndenterState()
#        state.append(Item(Context.RECORD, 1, 0, Token("LBRACE", "}")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_same_line():
#        example = " a = b c d"
#        state = IndenterState()
#        state.append(Item(Context.RECORD, 1, 0, Token("LBRACE", "}")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_next_line_single():
#        example = " a = \n    b"
#        state = IndenterState()
#        state.append(Item(Context.RECORD, 1, 0, Token("LBRACE", "}")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_next_line_single_fail():
#        example = " a = \n b"
#        state = IndenterState()
#        state.append(Item(Context.RECORD, 1, 0, Token("LBRACE", "}")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_next_line():
#        example = " a = \n    b c \n    d"
#        state = IndenterState()
#        state.append(Item(Context.RECORD, 1, 0, Token("LBRACE", "}")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_equal_indentation_wont_close_context():
#        example = " a = \n    b \n    c"
#        state = IndenterState()
#        state.append(Item(Context.RECORD, 1, 0, Token("LBRACE", "}")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_lower_indentation_close_context():
#        example = " a = \n     b \n    c"
#        state = IndenterState()
#        state.append(Item(Context.RECORD, 1, 0, Token("LBRACE", "}")))
#        expected = [
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "VARIABLE_IDENTIFIER",
#            "IndentationError",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_fail_incomplete():
#        example = " a = \n"
#        state = IndenterState()
#        state.append(Item(Context.RECORD, 1, 0, Token("LBRACE", "}")))
#        expected = ["VARIABLE_IDENTIFIER", "EQUAL", "IndentationError"]
#        make_test(example, state, expected)
#

# class TestLet:
#    @staticmethod
#    def test_single_line():
#        example = "let a = b in c"
#        state = IndenterState()
#        expected = [
#            "LET",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "IN",
#            "VARIABLE_IDENTIFIER",
#            "IN_END",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_multiple_line():
#        example = """
#    let
#     a = b
#      c = d
#       e = f
#    in
#     z
#        """
#        state = IndenterState()
#        expected = [
#            "LET",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "IN",
#            "VARIABLE_IDENTIFIER",
#            "IN_END",
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_multiple_line_in_at_same_line_as_last():
#        example = """
#    let
#       a = b
#    in z"""
#        state = IndenterState()
#        expected = [
#            "LET",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "IN",
#            "VARIABLE_IDENTIFIER",
#            "IN_END"
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_multiple_line_in_at_same_indentation_as_last():
#        example = """
#    let
#       a = b
#    in
#    z"""
#        state = IndenterState()
#        expected = [
#            "LET",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "IN",
#            "VARIABLE_IDENTIFIER",
#            "IN_END"
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_fail_if_next_not_indented():
#        example = """
#    let
#    a """
#        state = IndenterState()
#        expected = [
#            "LET",
#            "IndentationError"
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_fail_if_in_not_in_the_same_column():
#        example = """
#    let a = b
#     in z"""
#        state = IndenterState()
#        expected = [
#            "LET",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "IndentationError"
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_fail_if_in_not_in_the_same_line_and_last_is():
#        example = """
#    let a = b
#     in
#    z"""
#        state = IndenterState()
#        expected = [
#            "LET",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "IndentationError"
#        ]
#        make_test(example, state, expected)
#
#    @staticmethod
#    def test_nested_let():
#        example = """
#    let a =
#           let
#            w =
#               z
#           in
#           c
#    in
#        let
#           h = m
#        in
#         r"""
#        state = IndenterState()
#        expected = [
#            "LET",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "LET",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL_END",
#            "IN",
#            "VARIABLE_IDENTIFIER",
#            "IN_END",
#            "EQUAL_END",
#            "IN",
#            "LET",
#            "VARIABLE_IDENTIFIER",
#            "EQUAL",
#            "VARIABLE_IDENTIFIER"
#            "EQUAL_END",
#            "IN",
#            "VARIABLE_IDENTIFIER",
#            "IN_END",
#            "IN_END"
#        ]
#        make_test(example, state, expected)
