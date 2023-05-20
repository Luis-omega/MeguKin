from typing import Optional, Iterable, NamedTuple
from enum import Enum, auto

from lark import Token


class Context(Enum):
    ROOT = auto()
    VARIABLE_TOP = auto()
    LET = auto()
    IN = auto()
    CASE = auto()
    OF = auto()
    FORALL = auto()
    DOT = auto()
    DATA = auto()
    LAMBDA = auto()
    LAMBDA_ARROW = auto()
    RECORD = auto()
    PAREN = auto()


class Item(NamedTuple):
    context: Context
    column: int
    line: int


indentationErrorTokenName = "IndentationError"


class Indenter:
    stack: list[Item]
    nex_token_set_indentation: bool

    def __init__(self) -> None:
        self.stack = [Item(Context.ROOT, 0, 0)]
        self.nex_token_set_indentation = False

    def open_context(self, context: Context, token: Token):
        self.stack.append(Item(context, token.column, token.line))  # type: ignore

    def close_context(self, item: Item, token: Token) -> Optional[Token]:
        match item.context:
            case Context.ROOT:
                return Token.new_borrow_pos(
                    indentationErrorTokenName,
                    "A problem with the indentation handler  happened, please report this",
                    token,
                )
            case Context.VARIABLE_TOP:
                if token.column != 0:
                    return Token.new_borrow_pos(
                        indentationErrorTokenName,
                        "Bad indentation at top variable definition: " + repr(token),
                        token,
                    )
                else:
                    return Token.new_borrow_pos("VARIABLE_TOP_END", "", token)
            case Context.LET:
                if token.type != "IN":
                    return Token.new_borrow_pos(
                        indentationErrorTokenName,
                        f"Let indentation error at Token({token.type},{token.value})",
                        token,
                    )
                elif token.column != item.column:
                    return Token.new_borrow_pos(
                        indentationErrorTokenName,
                        "Indentation error, a `in` for a `let` should be at the same indentation level",
                        token,
                    )
                else:
                    return None
            case Context.IN:
                return Token.new_borrow_pos("IN_END", "", token)
            case Context.CASE:
                #TODO: The close_context should drop the context? I guess yes, but still 

    def close_until_indentation(self, token: Token) -> list[Token]:
        last_context = self.stack[-1]
        acc = []
        while last_context[1] > token.column:  # type: ignore
            maybe_closed_token = self.close_context(last_context, token)
            if isinstance(maybe_closed_token, Token):
                acc.append(maybe_closed_token)
        return acc

    def process(self, stream: Iterable[Token]) -> Iterable[Token]:
        for token in stream:
            # save line and column
            # very token can do two things: being a token that init/close a context or
            # be in a different line than the previous one
            # the two cases can be together, so we have 4 cases in total.
            # if same line and no context:
            #   just put the token
            # if same line and context:
            #    close context
            # if other line and no context:
            #    close all remain context if we can or send IndentationError token
            # if other line and context:
            #    close all remain context if we can or send IndentationError token and if we still good, send close context
            to_inject_first = self.close_until_indentation(token)
            if len(to_inject_first) > 0:
                yield from to_inject_first
            last_context = self.stack[-1]
            match last_context:
                case Context.VARIABLE_TOP:
                    match token.type:
                        case "LBRACE":
                            self.stack.append(Context.RECORD)
                            yield token
                        case "LAMBDA":
                            self.stack.append(token.column)

            # yield from self.handle_NL(token)
