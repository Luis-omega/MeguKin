from typing import Iterator, Callable, Optional

import pytest
from lark import Lark
import lark
from pathlib import Path

from MeguKin.Parser.Parser import load_grammar
from MeguKin.Parser.Token import Token
from MeguKin.File import FileInfo
from MeguKin.Parser.Indentation import (
    handle_indentation,
    Stream,
    ContextStack,
)

lark_parser = None


def get_lexer() -> Callable[[str], Iterator[lark.Token]]:
    global lark_parser
    if lark_parser is None:
        lark_parser = load_grammar(True)
        if not isinstance(lark_parser, Lark):
            raise Exception(f"Can't get lexer for tests {lark_parser}")
    return lark_parser.lex


def make_test(example: str, state: Optional[ContextStack], expected: list[str]):
    lexer = get_lexer()
    regular_tokens = list(lexer(example))
    # before we had a explicit call to inject tokens
    # but now the load of grammar inmediatly calls it, so
    # doing it again, would crash the algorithm
    tokens_type = [token.type for token in regular_tokens]
    assert expected == tokens_type


class TestStream:
    @staticmethod
    def test_stream():
        def gen_stream_raw():
            for i in range(0, 10):
                yield i

        stream_raw = gen_stream_raw()
        stream = Stream(stream_raw)
        first = stream.peek()
        first_consumed = stream.get_next()
        assert first == first_consumed
        second = stream.peek()
        second2 = stream.peek()
        assert second == second2
        second_consumed = stream.get_next()
        assert second_consumed == second
        assert second == 1


class TestLet:
    @staticmethod
    def test_same_line_single():
        example = "let a = b in w\n1"
        state = None
        expected = [
            "LET",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "IN",
            "VARIABLE_IDENTIFIER",
            "INT",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_same_line_nested_two_in_let():
        example = "let a = let b = c in d in e \n1"
        state = None
        expected = [
            # let a =
            "LET",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            # let b =
            "LET",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            # c in d
            "VARIABLE_IDENTIFIER",
            "IN",
            "VARIABLE_IDENTIFIER",
            # in e
            "IN",
            "VARIABLE_IDENTIFIER",
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
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "IN",
            # let c = d in e
            "LET",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "IN",
            "VARIABLE_IDENTIFIER",
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
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "IN",
            # let c = d in
            "LET",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "IN",
            # let e = f in g
            "LET",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "IN",
            "VARIABLE_IDENTIFIER",
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
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "INT",
        ]
        make_test(example, state, expected)

    @staticmethod
    def test_different_line_two_second_same_line():
        example = """
    let 
        a = b
    in
        let c = d in
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
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            # let c = d in e
            "LET",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
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

    @staticmethod
    def test_different_line_two_second_other_line():
        example = """
    let 
        a = b
    in
        let 
            c = d 
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
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            "IN",
            "LAYOUT_START",
            # let c = d in e
            "LET",
            "LAYOUT_START",
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
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

    @staticmethod
    def test_tree_definition_single_let():
        example = """
    let 
        a = b
        c = 
         d
        e = f
    in
     g
1
        """
        state = None
        expected = [
            # let
            "LET",
            "LAYOUT_START",
            # a = b
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_SEPARATOR",
            # c = d
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_SEPARATOR",
            # e = f
            "VARIABLE_IDENTIFIER",
            "EQUAL",
            "VARIABLE_IDENTIFIER",
            "LAYOUT_END",
            # in g
            "IN",
            "LAYOUT_START",
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
