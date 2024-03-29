from typing import (
    Optional,
    Iterable,
    NamedTuple,
    Generic,
    TypeVar,
    Iterator,
    NewType,
)
from enum import Enum, auto
from abc import ABC, abstractmethod
from dataclasses import dataclass

from lark.lark import PostLex
import lark


from MeguKin.Loggers import get_logger
from MeguKin.Parser.Token import Token
from MeguKin.File import FileInfo
from MeguKin.Error import MeguKinError

import sys

log = get_logger(__name__)


def repr_token(token: Token):
    return f"{repr(token)} at line {token.line}, column {token.column}"


class LayoutError(MeguKinError):
    msg: str

    def __repr__(self):
        return self.msg


@dataclass
class MissMatchIndentation(LayoutError):
    start_token: Token
    end_token: Token


class LayoutClosedByBadToken(MissMatchIndentation):
    pass
    # def __init__(self, start_token: Token, end_token: Token):
    #    self.msg = (
    #        "Unexpected indentation for "
    #        f"{repr_token(end_token)}, while handling the "
    #        f"indentation introduced by {repr_token(start_token)}"
    #    )


@dataclass
class ContextFirstValueBeforeStart(MissMatchIndentation):
    force: bool

    def __init__(self, start_token: Token, end_token: Token, force=False):
        self.start_token = start_token
        self.end_token = end_token
        self.force = force

    #    if force:
    #        self.msg = (
    #            "Unexpected indentation for "
    #            f"{repr_token(end_token)}, we expected it to have indentation"
    #        )
    #    else:
    #        self.msg = (
    #            "Unexpected indentation for "
    #            f"{repr_token(end_token)}, we expected it to be greater than the "
    #            f"indentation introduced by {repr_token(start_token)}"
    #        )


class LetInMaybeMisplaced(MissMatchIndentation):
    pass
    # def __init__(self, start_token: Token, end_token: Token):
    #    self.msg = (
    #        "Unexpected indentation for"
    #        f"{repr_token(end_token)}, we expected it to be lower than the "
    #        f"indentation introduced by {repr_token(start_token)}"
    #    )


@dataclass
class UnexpectedEOF(LayoutError):
    report_at_token: Token
    expected_token: str
    # msg: str

    # def __init__(self, token: Token, expected_token: str):
    #    self.report_at_token = token
    #    self.msg = (
    #        "Unexpected end of input, we expected a "
    #        f"{expected_token} after a {repr_token(token)}"
    #    )


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
    token: Token,
    context: Context,
    stream: Stream,
    stack: ContextStack,
    force=False,
) -> tuple[bool, list[Token]]:
    log.debug(f"make_context_with_next_token, token: {repr_token(token)}")
    next_token = stream.peek()
    out = []
    if next_token is None:
        log.debug("no next token found")
        out.append(token)
        out.append(make_token_error(token, UnexpectedEOF(token, "let")))
        return (True, out)
    else:
        log.debug(f"next_token: {repr_token(next_token)}")
        if next_token.column > token.column or (
            force and next_token.column > 1
        ):
            if next_token.line > token.line or (
                force and next_token.column > 1
            ):
                out.append(make_layout_start(next_token, ""))
                context_item = ContextItem(
                    next_token.column, next_token.line, token, context
                )
                stack.append(context_item)
                log.debug(f"returning out : {out}")
                return (False, out)
            else:
                return (False, [])
        else:
            out.append(
                make_token_error(
                    next_token,
                    ContextFirstValueBeforeStart(
                        token, next_token, force=force
                    ),
                )
            )
            return (True, out)


def make_separator(context: ContextItem, token: Token) -> Token:
    return Token.new_borrow_pos("LAYOUT_SEPARATOR", "\\;", token)


def gen_separator_if_same_level(
    token: Token, stack: ContextStack
) -> Optional[Token]:
    match stack:
        case [*_, last_context]:
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
    last_token = current_token
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
            in_import = current_token.type == "IMPORT"

        match current_token.type:
            case "LET" | "IN" | "DO" | "CASE" | "OF" | "RIGHT_ARROW" | "LAMBDA" | "DOUBLE_COLON" | "FORALL":
                log.debug(
                    "insert original layout token:"
                    f"{repr_token(current_token)}"
                )
                yield current_token

                context = getattr(Context, current_token.type)
                (has_error, tokens) = make_context_with_next_token(
                    current_token, context, stream, stack
                )
                for token in tokens:
                    log.debug(f"insert layout: {repr_token(token)}")
                    yield token
                if has_error:
                    return

            case "COLON":
                match stack:
                    case [ContextItem(0, 0, _, Context.ROOT)]:
                        log.debug(f"insert: {repr_token(current_token)}")
                        yield current_token
                        (has_error, tokens) = make_context_with_next_token(
                            current_token,
                            Context.EQUAL,
                            stream,
                            stack,
                            force=True,
                        )

                        log.debug(
                            "insert original layout token:"
                            f"{repr_token(current_token)}"
                        )
                        for token in tokens:
                            log.debug(f"insert layout: {repr_token(token)}")
                            yield token
                        if has_error:
                            return
                    case _:
                        log.debug(
                            "insert original layout token:"
                            f"{repr_token(current_token)}"
                        )
                        yield current_token

                        context = getattr(Context, current_token.type)
                        (has_error, tokens) = make_context_with_next_token(
                            current_token, context, stream, stack
                        )
                        for token in tokens:
                            log.debug(f"insert layout: {repr_token(token)}")
                            yield token
                        if has_error:
                            return

            case "RIGHT_ARROW":
                match stack:
                    case [
                        *_,
                        ContextItem(_, _, _, Context.OF)
                        | ContextItem(_, _, _, Context.LAMBDA),
                    ]:
                        log.debug(
                            "insert original layout token:"
                            f"{repr_token(current_token)}"
                        )
                        yield current_token
                        context = getattr(Context, current_token.type)
                        (has_error, tokens) = make_context_with_next_token(
                            current_token, context, stream, stack
                        )

                        for token in tokens:
                            log.debug(f"insert layout: {repr_token(token)}")
                            yield token
                        if has_error:
                            return
                    case _:
                        log.debug(f"insert default: repr_token{current_token}")
                        yield current_token
            case "EQUAL":
                match stack:
                    case [ContextItem(0, 0, _, Context.ROOT)]:
                        log.debug(f"insert: {repr_token(current_token)}")
                        yield current_token
                        if not in_import:
                            (has_error, tokens) = make_context_with_next_token(
                                current_token,
                                Context.EQUAL,
                                stream,
                                stack,
                                force=True,
                            )

                            log.debug(
                                "insert original layout token:"
                                f"{repr_token(current_token)}"
                            )
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

        last_token = current_token
        current_token = stream.get_next()

    log.debug("removing remaining context")
    if last_token is None:
        eof_token = Token("EOF", "EOF", 0, 0, 0, 0, 0, 0)
    else:
        eof_token = Token(
            "EOF",
            "EOF",
            last_token.end_pos,
            last_token.end_line,
            last_token.end_column,
            last_token.end_line,
            last_token.end_column,
            last_token.end_pos,
        )
    while stack:
        current = stack.pop()
        match current:
            case ContextItem(0, 0, _, Context.ROOT):
                return
            case _:
                log.debug("removing context")
                yield make_layout_end(eof_token, current)


class Indenter(PostLex):
    def process(self, stream: Iterator[lark.Token]) -> Iterator[Token]:
        return handle_indentation(
            None, Stream(map(lambda x: Token.from_lark(x), stream))
        )
