from typing import Optional

import lark


class Token(lark.Token):
    start_pos: int
    line: int
    column: int
    end_line: int
    end_column: int
    end_pos: int

    def __new__(
        cls,
        type_,
        value,
        start_pos: int,
        line: int,
        column: int,
        end_line: int,
        end_column: int,
        end_pos: int,
    ):
        inst = super(Token, cls).__new__(cls, type_, value, start_pos)
        return inst

    # FIXME: for some reason this doesn't work, it sets int as None
    def from_lark(token: lark.Token):
        return Token(
            token.type,
            token.value,
            token.start_pos,
            token.line,
            token.column,
            token.end_line,
            token.end_column,
            token.end_pos,
        )  # type:ignore
