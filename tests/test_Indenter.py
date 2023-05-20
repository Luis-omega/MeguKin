from typing import Iterator, Callable
from lark import Lark, Token


# this must memoize the lexer generated to avoid
# multiple initialization
def get_lexer() -> Callable[[str], Iterator[Token]]:
    pass


def test_expression_record_update():
    lexer = get_lexer()
    text_to_test = [
        """
a = { b = c, d=e,f=g}
""",
        """
a = { b 
      = c,
      d=e,
      f=g
    }
""",
        """
a = { b 
      = 
      c,
      d = 
        e,
      f = 
        g
    }
""",
    ]
    lexed_text = [token.value for token in lexer(text_to_test)]
    assert expected_result == lexed_text


def test_expression_record():
    lexer = get_lexer()
    text_to_test = """
a = {
 b: w,
 c: z,
     d : y1 y2,
         e:x1 
            x2
           x3,
      f : v
 }"""
    expected_result = [
        "a",
        "=",
        "(",
        "{",
        "b",
        ":",
        "(",
        "w",
        ")",
        ",",
        "c",
        ":",
        "(",
        "z",
        ")",
        "d",
        ":",
        "(",
        "y1",
        "y2",
        ")",
        ",",
        "e",
        ":",
        "(",
        "x1",
        "x2",
        "x3" ")",
        ",",
        "f",
        ":",
        "(",
        "v",
        ")",
        "}",
        ")",
        # TODO: I don't know if this is the right final token
        "EOF",
    ]
    lexed_text = [token.value for token in lexer(text_to_test)]
    assert expected_result == lexed_text


def test_expression_annotation():
    lexer = get_lexer()
    text_to_test = """
a = ( 
 b : 
    C d e
        f
      g)"""
    expected_result = ["a", "=", "(", "(" "b", ":", "C", "d", "e", "f", "g", ")", ")"]
