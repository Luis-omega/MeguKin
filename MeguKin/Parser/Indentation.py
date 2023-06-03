"""
:mod:`Indentation` -- Indentation handling
==========================================

.. module:: MeguKin.Parser.Indentation
   :synopsis: Inject indentation tokens.
.. moduleauthor:: Luis Alberto Díaz Díaz

Segmentation
============
A file is separated in segments, that means that the 
only token that appears at column 1 is the firs one. 
We can safely assume that all other tokens in the 
original source have at least 2 as column.
"""


from typing import (
    Optional,
    Iterable,
    NamedTuple,
    Callable,
    Generic,
    TypeVar,
    Iterator,
    NewType,
)
from enum import Enum, auto
import logging


from MeguKin.Parser.Token import Token
from MeguKin.File import FileInfo
from MeguKin.Error import MeguKinError

log = logging.getLogger(__name__)
log.setLevel(logging.DEBUG)


def repr_token(token: Token):
    return f"{repr(token)} at line {token.line}, column {token.column}"


class LayoutError(MeguKinError):
    msg: str

    def __repr__(self):
        return self.msg


class MissMatchIndentation(LayoutError):
    pass


class LayoutClosedByBadToken(MissMatchIndentation):
    def __init__(self, start_token: Token, end_token: Token):
        self.msg = f"Unexpected indentation for {repr_token(end_token)}, while handling the indentation introduced by {repr_token(start_token)}"


class LetFirstValueBeforeLet(MissMatchIndentation):
    def __init__(self, start_token: Token, end_token: Token):
        self.msg = f"Unexpected indentation for {repr_token(end_token)}, we expected it to be lower than the indentation introduced by {repr_token(start_token)}"


class LetInMaybeMisplaced(MissMatchIndentation):
    def __init__(self, start_token: Token, end_token: Token):
        self.msg = f"Unexpected indentation for {repr_token(end_token)}, we expected it to be lower than the indentation introduced by {repr_token(start_token)}"


class UnexpectedEOF(LayoutError):
    report_at_token: Token
    msg: str

    def __init__(self, token: Token, expected_token: str):
        self.report_at_token = token
        self.msg = f"Unexpected end of input, we expected a {expected_token} after a {repr_token(token)}"


T = TypeVar("T")


class Context(Enum):
    """ """

    ROOT = auto()
    LET = auto()
    IN = auto()
    DO = auto()
    CASE = auto()
    OF = auto()
    RIGHT_ARROW = auto()
    COLON = auto()
    DOUBLE_COLON = auto()
    LAMBDA = auto()
    EQUAL = auto()
    FORALL = auto()
    DATA = auto()


class ContextItem(NamedTuple):
    column: int
    line: int
    originated_by: Token
    context: Context


ContextStack = NewType("ContextStack", list[ContextItem])


class Stream(Generic[T]):
    stream: Iterator[T]
    next_item: Optional[T]

    def __init__(self, stream: Iterator[T]):
        self.stream = stream
        self.next_item = None

    def get_next(self) -> Optional[T]:
        if self.next_item is None:
            try:
                new_value = next(self.stream)
                return new_value
            except StopIteration:
                return None
        else:
            new_value = self.next_item
            self.next_item = None
            return new_value

    def peek(self) -> Optional[T]:
        if self.next_item is None:
            try:
                next_item = next(self.stream)
                self.next_item = next_item
                return next_item
            except StopIteration:
                return None
        else:
            return self.next_item


def make_layout_end(token: Token, content: T) -> Token:
    return token.new_borrow_pos("LAYOUT_END", "\\)", token)


def make_layout_start(token: Token, content: T) -> Token:
    return token.new_borrow_pos("LAYOUT_START", "\\(", token)


def make_token_error(token: Token, error: LayoutError):
    return token.new_borrow_pos("LAYOUT_ERROR", error, token)


def make_context_with_next_token(
    token: Token, context: Context, stream: Stream, stack: ContextStack
) -> tuple[bool, list[Token]]:
    next_token = stream.peek()
    out = []
    if next_token is None:
        out.append(token)
        out.append(make_token_error(token, UnexpectedEOF(token, "let")))
        return (True, out)
    else:
        if next_token.column > token.column:
            if next_token.line > token.line:
                out.append(make_layout_start(token, ""))
                context_item = ContextItem(
                    next_token.column, next_token.line, token, context
                )
                stack.append(context_item)
                return (False, out)
            else:
                return (False, [])
        else:
            out.append(
                make_token_error(
                    next_token, LetFirstValueBeforeLet(token, next_token)
                )
            )
            return (True, out)


def make_separator(context: ContextItem, token: Token) -> Token:
    return Token.new_borrow_pos("LAYOUT_SEPARATOR", "\\;", token)


def gen_separator_if_same_level(
    token: Token, stack: ContextStack
) -> Optional[Token]:
    match stack:
        case [*others, last_context]:
            if (
                last_context.column == token.column
                and last_context.line == token.line
            ):
                return None
            elif last_context.column == token.column:
                return make_separator(last_context, token)
            else:
                return None
    return None


def unwind_stack(stack: ContextStack, token: Token) -> Iterable[Token]:
    log.debug(f"unwinding stack,token:{repr_token(token)}")
    while len(stack) > 0:
        last_context = stack[-1]
        log.debug(f"unwinding step, last_context={last_context}")
        if token.column < last_context.column:
            log.debug("removing context")
            stack.pop()
            yield make_layout_end(token, last_context)
        else:
            return


def handle_indentation(
    info: FileInfo,
    previous_stack: Optional[ContextStack],
    stream: Stream[Token],
) -> Iterable[Token]:
    root_context = ContextItem(
        0, 0, Token("SOF", "start_of_file", 0, 0, 0, 0, 0, 0), Context.ROOT
    )
    stack: ContextStack
    if previous_stack is None:
        stack = ContextStack([root_context])
    else:
        stack = previous_stack

    last_line = 0
    current_token: Optional[Token] = stream.get_next()
    while current_token is not None:
        log.debug(f"New loop\ntoken: {repr_token(current_token)}")
        log.debug(f"context: {stack}")

        maybe_separator = gen_separator_if_same_level(current_token, stack)
        if maybe_separator is not None:
            log.debug(f"inserting separator: {repr_token(maybe_separator)}")
            yield maybe_separator

        if current_token.line > last_line:
            tokens_to_serve = unwind_stack(stack, current_token)
            for token in tokens_to_serve:
                log.debug(f"insert unwind: {repr_token(token)}")
                yield token
                if token.type == "LayoutError":
                    return
            log.debug(f"context after unwind: {stack}")

        match current_token.type:
            case "LET" | "IN" | "DO" | "CASE" | "OF" | "RIGHT_ARROW" | "LAMBDA" | "COLON" | "DOUBLE_COLON":
                context = getattr(Context, current_token.type)
                (has_error, tokens) = make_context_with_next_token(
                    current_token, context, stream, stack
                )

                log.debug(
                    f"insert original layout token: {repr_token(current_token)}"
                )
                yield current_token
                for token in tokens:
                    log.debug(f"insert layout: {repr_token(token)}")
                    yield token
                if has_error:
                    return
            case "EQUAL":
                match stack:
                    case [ContextItem(0, 0, _, Context.ROOT)]:
                        (has_error, tokens) = make_context_with_next_token(
                            current_token, Context.LET, stream, stack
                        )

                        log.debug(
                            f"insert original layout token: {repr_token(current_token)}"
                        )
                        yield current_token
                        for token in tokens:
                            log.debug(f"insert layout: {repr_token(token)}")
                            yield token
                        if has_error:
                            return
                    case _:
                        log.debug(
                            f"insert default: {repr_token(current_token)}"
                        )
                        yield current_token
            case _:
                log.debug(f"insert default: {repr_token(current_token)}")
                yield current_token

        current_token = stream.get_next()
