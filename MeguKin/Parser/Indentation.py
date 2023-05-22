from typing import Optional, Iterable, NamedTuple, Callable
from enum import Enum, auto
import logging

from lark import Token

from MeguKin.File import FileInfo
from MeguKin.Error import MeguKinError

log = logging.getLogger(__name__)
log.setLevel(logging.DEBUG)


class Context(Enum):
    ROOT = auto()
    EQUAL = auto()
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


def make_error_token(token: Token, msg: str):
    return Token.new_borrow_pos("IndentationError", f"Indentation error, {msg}", token)


class IndenterError(MeguKinError):
    pass


class NextTokenAtleastAt(NamedTuple):
    token: Token
    column: int
    error_msg: str
    gen_item: Callable[[Token], Item]


class IndenterState:
    stack: list[Item]
    expects: Optional[NextTokenAtleastAt]

    def __init__(self) -> None:
        self.stack = [Item(Context.ROOT, 0, 0)]
        self.expects = None

    def append(self, item: Item) -> None:
        self.stack.append(item)

    def __repr__(self):
        return f"IndenterState({repr(self.stack)},{repr(self.expects)})"


def gen_close_context(
    item: Item, previous_item: Optional[Item], token: Token
) -> Optional[Token]:
    match item.context:
        case Context.ROOT:
            return Token.new_borrow_pos(
                indentationErrorTokenName,
                "A problem with the indentation handler  happened, please report this",
                token,
            )
        case Context.EQUAL:
            if previous_item is None:
                return make_error_token(
                    token,
                    "A problem with the indentation handler  happened, please report this",
                )
            match previous_item.context:
                case Context.ROOT:
                    return Token.new_borrow_pos("EQUAL_TOP_END", "", token)
                case Context.LET:
                    return Token.new_borrow_pos("EQUAL_END", "", token)
                case Context.RECORD:
                    return None
                case _:
                    return None
        case Context.LET:
            if token.type != "IN":
                return make_error_token(
                    token,
                    "unexpected `in`, if you are trying to close a `let`, something is missing in the middle of them or maybe you forgot to use `let` to being with.",
                )
            elif token.column != item.column:
                return make_error_token(
                    token, "a `in` for a `let` should be at the same indentation level"
                )
            else:
                return None
        case Context.IN:
            return Token.new_borrow_pos("IN_END", "", token)
        case Context.CASE:
            # TODO: The close_context should drop the context? I guess yes, but still
            pass
        case Context.RECORD:
            match token.type:
                case "RBRACE":
                    if token.line == item.line or token.column == item.column:
                        return None
                    return make_error_token(
                        token,
                        "a closing `}` must be at the same level that it's corresponding `{`, maybe you miss one `}`? ",
                    )
                case _:
                    return make_error_token(
                        token, "expected `}` to close a `{` but wasn't found"
                    )


def gen_close_tokens_after(state: IndenterState, token: Token) -> list[Token]:
    log.debug(f"gen_close_tokens_after state: {state}")
    log.debug(f"token: {repr(token)} {token.column}")
    last_context = state.stack[-1]
    acc = []
    while last_context.column > token.column:  # type: ignore
        log.debug(f"last_context: {last_context}")
        if len(state.stack) > 1:
            previous_context = state.stack[-2]
        else:
            previous_context = None
        log.debug(f"previous_context : {previous_context}")
        maybe_closed_token = gen_close_context(last_context, previous_context, token)
        if isinstance(maybe_closed_token, Token):
            log.debug("generated token for close of context")
            acc.append(maybe_closed_token)
        state.stack.pop()
        last_context = state.stack[-1]
    return acc


def set_expectation_at_next_token(state: IndenterState, context: Context, token: Token):
    log.debug("setting expectation for indentation")
    state.expects = NextTokenAtleastAt(
        token,
        token.column + 1,  # type: ignore
        "expected indentation to be greater than the '=' for let",
        lambda tok: Item(context, tok.column, tok.line),  # type: ignore
    )
    log.debug(state.expects)
    log.debug(f"yield {token}")


def handle_indentation(
    info: FileInfo, previousState: Optional[IndenterState], stream: Iterable[Token]
) -> Iterable[IndenterError | Token]:
    log.debug(f"begin_with {info} {previousState}")
    if previousState is None:
        state = IndenterState()
    else:
        state = previousState

    log.debug(f"initial state {state}")

    for token in stream:
        log.debug(f"Beginning new loop")
        log.debug(f"Current token in stream: {token}")
        log.debug(f"Current state: {state}")

        if not state.expects is None:
            if token.column < state.expects.column:
                log.debug("new token is below expected indentation, reporting error")
                error_token = make_error_token(token, state.expects.error_msg)
                yield error_token
                return
            else:
                log.debug("new token pass the indentation threshold, cleaning state")
                new_context = state.expects.gen_item(token)
                state.append(new_context)
                state.expects = None

        for close_token in gen_close_tokens_after(state, token):
            log.debug(f"emitting closing token {close_token}")
            yield close_token

        last_context = state.stack[-1].context
        match token.type:
            case "EQUAL":
                match last_context:
                    case Context.ROOT:
                        log.debug("setting expectation for indentation")
                        state.expects = NextTokenAtleastAt(
                            token,
                            1,
                            # In this case as the file is segmented, if something
                            # got wrong, means that the segment input ended
                            # in other words in the original file we got
                            # something like:
                            # "a = \n\nc=2"
                            "expected a definition after a equal",
                            lambda tok: Item(Context.EQUAL, tok.column, tok.line),
                        )
                        log.debug(state.expects)
                        log.debug(f"yield {token}")
                        yield token
                    case Context.LET:
                        log.debug("setting expectation for indentation")
                        state.expects = NextTokenAtleastAt(
                            token,
                            token.column + 1,  # type: ignore
                            "expected indentation to be greater than the '=' for let",
                            lambda tok: Item(Context.EQUAL, tok.column, tok.line),
                        )
                        log.debug(state.expects)
                        log.debug(f"yield {token}")
                        yield token
                    case Context.RECORD:
                        log.debug("setting expectation for indentation")
                        state.expects = NextTokenAtleastAt(
                            token,
                            token.column + 1,  # type: ignore
                            "expected indentation to be greater than the '=' in the record",
                            lambda tok: Item(Context.EQUAL, tok.column, tok.line),
                        )
                        log.debug(state.expects)
                        log.debug(f"yield {token}")
                        yield token
            case "LET":
                log.debug("setting expectation for indentation")
                state.expects = NextTokenAtleastAt(
                    token,
                    token.column + 1,  # type: ignore
                    "expected indentation to be greater than the '=' in the record",
                    lambda tok: Item(Context.EQUAL, tok.column, tok.line),
                )
                log.debug(state.expects)
                log.debug(f"yield {token}")
                yield token

            case "IN":
                log.debug("trying to remove let context as we found a `in`")
                maybe_new = gen_close_context(
                    state.stack[-1], None, token  # We don't need more context for this.
                )
                if not maybe_new is None:
                    # not None means that a
                    log.debug("failed to remove let context")
                    yield maybe_new
                    return
            case _:
                log.debug(f"nothing to do, yielding token {token}")
                yield token

    log.debug("End of loop")
    log.debug(f"state: {state}")
    log.debug(f"last token: {token}")

    if not state.expects is None:
        log.debug("End of input while we expected more tokens")
        error_token = make_error_token(token, state.expects.error_msg)
        yield error_token
        return

    eof_token = Token("EOF", "EOF")
    eof_token.column = 0
    for close_token in gen_close_tokens_after(state, eof_token):
        log.debug(f"emitting closing tokens for remaining context {close_token}")
        yield close_token


# class Indenter:
#    state: IndenterState
#
#    def __init__(self) -> None:
#        self.state = IndenterState()
#
#    def close_until_indentation(self, token: Token) -> list[Token]:
#        last_context = self.stack[-1]
#        acc = []
#        while last_context[1] > token.column:  # type: ignore
#            maybe_closed_token = self.close_context(last_context, token)
#            if isinstance(maybe_closed_token, Token):
#                acc.append(maybe_closed_token)
#        return acc
#
#    def process(self, stream: Iterable[Token]) -> Iterable[Token]:
#        for token in stream:
#            # save line and column
#            # very token can do two things: being a token that init/close a context or
#            # be in a different line than the previous one
#            # the two cases can be together, so we have 4 cases in total.
#            # if same line and no context:
#            #   just put the token
#            # if same line and context:
#            #    close context
#            # if other line and no context:
#            #    close all remain context if we can or send IndentationError token
#            # if other line and context:
#            #    close all remain context if we can or send IndentationError token and if we still good, send close context
#            to_inject_first = self.close_until_indentation(token)
#            if len(to_inject_first) > 0:
#                yield from to_inject_first
#            last_context = self.stack[-1]
#            match last_context:
#                case Context.VARIABLE_TOP:
#                    match token.type:
#                        case "LBRACE":
#                            self.stack.append(Context.RECORD)
#                            yield token
#                        case "LAMBDA":
#                            self.stack.append(token.column)
#
#            # yield from self.handle_NL(token)
