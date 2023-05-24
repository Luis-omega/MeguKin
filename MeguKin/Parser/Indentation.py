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


Available contexts 
==================
 * ROOT
 * EQUAL
 * LET
 * IN
 * CASE
 * OF
 * FORALL
 * DOT
 * DATA
 * LAMBDA
 * LAMBDA_ARROW
 * RECORD
 * PAREN

The only token that isn't being or end by anything is the *ROOT* context. 
It is the default context when no other context are in effect.

All context (except for ROOT) can be closed by a token with a column level lower than the one stored
by the context. There are context that must be closed by specific tokens, if one of those is closed
by other token (that has less indentation than the context), we would generate a `IndentationError`
token.

Description of the intention
=================================

The principal tokens that can begin a indentation are:

* EQUAL

  Token(type="EQUAL",value="=")

  This token can appear in a *ROOT* context (top level definitions) a *RECORD* context or a *LET* context.

  In every context:

  * Began a `EQUAL` context.
  * It always needs at least one more token to be generated.
  * This context can always be closed by two events: 

    * We got a token with less indentation than the one in the context. 
    * We got out of input.
  
  Inside a *ROOT* context it: 

  * If next token is in the same line, the indentation level for the context is the column
    of such token.
  * If next token is in another line, we know that it has at least 2 in it's column, and we 
    use it to set the indentation level.
  * When closed it generates a `EQUAL_TOP_END` token that borrows the same position as the 
    token that is closing it or the end of file position.

  Inside a *RECORD* context it:

  * It uses next token to get the indentation level for the context, but that must be 
    greater than the column position of the corresponding *EQUAL* token.
  * When closed it won't generate a token. 

  Inside a *LET* context:
  
  * It uses next token to get the indentation level for the context, but that must be 
    greater than the column position of the corresponding *EQUAL* token.
  * It can also be closed by a *IN* token, and this context won't care about the position of the `IN`.
  * When closed by a `IN`, it generates a `EQUAL_END` token that would borrow the `IN` position.
  * When closed it generates a `EQUAL_END` token that borrows the same position as the 
    token that is closing it or the end of file position.

* LET

  Token(type="LET", value="let")

  It can appear anywhere.

  * Began a *LET* context.
  * Can only be closed by a *IN* token, conditions to close are:

    * The *IN* token appear in the same line as the *LET* token that began the context.
    * The *IN* token has the same indentation as the *LET* token that generated the context.
    * Every *IN* token can only close one *LET* context.

* IN 
  
  Token(type="IN",value="in")

  * Began a *IN* context.
  * Closes one *LET* context and a *EQUAL* context under ideal conditions. 
  * 
"""


from typing import Optional, Iterable, NamedTuple, Callable, Generic,TypeVar, Iterator
from enum import Enum, auto
import logging

from lark import Token

from MeguKin.File import FileInfo
from MeguKin.Error import MeguKinError

log = logging.getLogger(__name__)
log.setLevel(logging.DEBUG)

T = TypeVar("T")

class Context(Enum):
    """
    """
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
    originated_by: Token


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
        self.stack = [Item(Context.ROOT, 0, 0, Token("ROOT", "ROOT"))]
        self.expects = None

    def append(self, item: Item) -> None:
        self.stack.append(item)

    def __repr__(self):
        return f"IndenterState({repr(self.stack)},{repr(self.expects)})"

class Stream(Generic[T]):
    stream : Iterator[T]
    next_item : Optional[T]

    def __init__(self,stream:Iterator[T]):
        self.stream = stream
        self.next_item = None

    def get_next(self)->Optional[T]:
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

    def peek(self)->Optional[T]:
        if self.next_item is None:
            try: 
                next_item = next(self.stream)
                self.next_item = next_item
                return next_item
            except StopIteration:
                return None
        else:
            return self.next_item

StackItem = NamedTuple("StackItem",[("column",int),("line",int),("original_token",Token),("enforce_same_level",bool),("token_that_set_identation",Token)]) 

def format_token(token:Token)->str:
    return f"""{repr(token)}, at line {token.line}, column {token.column}"""

def make_error_token_with(token:Token,msg:str)->Token:
    return Token.new_borrow_pos("IndenterError",msg, token)

def make_expected_more_indentation_error(indentation_originator:Token, bad_token:Token)->Token:
    msg = f"expected more indentation at {format_token(bad_token)}, as we saw a{format_token(indentation_originator)}"
    return make_error_token_with(bad_token,msg)

def make_unexpected_end_of_input(token:Token,msg:Optional[str])->Token:
    if msg is None:
        full_msg = f"unexpected end of input at {format_token(token)}."
    else:
        full_msg = f"unexpected end of input at {format_token(token)}. {msg}"
    return make_error_token_with(token,full_msg)

def make_cant_find_close_token(token:Token)->Token:
    msg = "can't find the right match for {format_token(token)}."
    return make_error_token_with(token,msg)

def make_stack_item(origin_token:Token, same_level:bool, token_fund: Token):
    return StackItem(token_fund.column,token_fund.line,origin_token,same_level,token_fund)

def gen_close_token(item:StackItem,token:Token)->Optional[Token]:
    pass

def we_can_close(item:StackItem,token:Token):
    return item.column > token.column

def unwind_stack(items:list[StackItem],token:Token)->Iterator[Token]:
    indentation = token.column
    last_item = items[-1]
    while we_can_close(last_item,token):
        gen_token = gen_close_token(last_item,token)
        if gen_token is None:
            items.pop()
        else:
            yield gen_token
            items.pop()

        last_item = items[-1]

def safe_pop(l:list[T])->Optional[T]:
    if len(l)>1:
        return l.pop()
    else:
        return None


def handle_indentation(
    info: FileInfo, previousState: Optional[list[StackItem]], stream: Stream[Token]
) -> Iterable[Token]:
    if previousState is None :
        state = [StackItem(0,0,None,None,None)]
    else:
        state = previousState
    current_token = stream.get_next()
    line_changed = True
    previous_line = 0
    while not current_token is None : 
        line_changed =  previous_line < current_token.line

        if line_changed:
            log.debug("Line changed, unwinding stack")
            for close_token in unwind_stack(state, current_token):
                if not close_token is None :
                    yield close_token
                    if close_token.type == "IndenterError":
                        return


        match current_token.type:
            case "LET":
                next_token = stream.peek()
                if next_token is None:
                    error_token = make_unexpected_end_of_input(current_token, "A definition should go after a `let`.")
                    yield error_token 
                    return
                if next_token.column > current_token.column: #type: ignore
                    yield make_expected_more_indentation_error(current_token,next_token)
                    return 

                new_item = make_stack_item(current_token,True,next_token)
                state.append(new_item)
                log.debug("added let item to state")
                yield current_token
            case "IN":
                last_context = safe_pop(state)
                if last_context is None:
                    error_token = make_error_token_with(current_token,f"unmatched {format_token(current_token)}")
                    yield error_token
                    return 
                match last_context.original_token.type :
                    case "LET":
                        if last_context.original_token.column == current_token.column:
                            yield current_token
                            state.pop()
                        elif last_context.original_token.line == current_token.line:
                            # we know for sure that we have a problem 
                            # but it's something the parser must work on.
                            yield current_token
                        else:
                            msg = f"unexpected position of {format_token(current_token)}, maybe you were trying to close {format(last_context.original_token)}?"
                            error_token = make_error_token_with(current_token, msg)
                            yield error_token
                            return
                    case "EQUAL":
                        if last_context.original_token.line == current_token.line:
                            previous_context = safe_pop(state)
                            if previous_context is None:
                                error_token = make_error_token_with(current_token,f"unmatched {format_token(current_token)}")
                                yield error_token
                                return 
                            match previous_context.original_token.type:
                                case "LET":
                                    if last_context.original_token.column == current_token.column:
                                        yield current_token
                                        state.pop()
                                    elif last_context.original_token.line == current_token.line:
                                        # we know for sure that we have a problem 
                                        # but it's something the parser must work on.
                                        yield current_token
                                    else:
                                        msg = f"unexpected position of {format_token(current_token)}, maybe you were trying to close {format(last_context.original_token)}?"
                                        error_token = make_error_token_with(current_token, msg)
                                        yield error_token
                                        return
                
                


        current_token = stream.get_next()
        previous_line = current_token.line

    

# keep track of when we began a new line and use it to enforce same level of indentation for let


#def gen_close_context(
#    item: Item, previous_item: Optional[Item], token: Token
#) -> Optional[Token]:
#    match item.context:
#        case Context.ROOT:
#            return Token.new_borrow_pos(
#                indentationErrorTokenName,
#                "A problem with the indentation handler  happened, please report this, the indentation stack became empty!",
#                token,
#            )
#        case Context.EQUAL:
#            if previous_item is None:
#                return make_error_token(
#                    token,
#                    "A problem with the indentation handler  happened, please report this, expected to close a `equal` context",
#                )
#            match previous_item.context:
#                case Context.ROOT:
#                    return Token.new_borrow_pos("EQUAL_TOP_END", "", token)
#                case Context.LET:
#                    return Token.new_borrow_pos("EQUAL_END", "", token)
#                case Context.RECORD:
#                    return None
#                case _:
#                    return None
#        case Context.LET:
#            if token.type != "IN":
#                if token.value == "EOF":
#                    make_error_token(
#                        item.originated_by,
#                        "couldn't find the appropriate place to close this `let`, maybe you forgot a `in`? or if it is present, maybe the indentation of that `in` is mistaken?",
#                    )
#                return make_error_token(
#                    item.originated_by,
#                    "couldn't find the appropriate place to close this `let`, maybe you forgot a `in`? or if the `in` is present, maybe the indentation of that `in` is mistaken?",
#                )
#            elif token.column != item.originated_by.column:
#                if token.line == item.originated_by.line:
#                    return None
#                return make_error_token(
#                    token,
#                    f"a `in` for a `let` should be at the same indentation level, while trying to close the `let` at line {item.originated_by.line}, column {item.originated_by.column} with the `in` at line {token.line}, column {token.column}.",
#                )
#            else:
#                return None
#        case Context.IN:
#            return Token.new_borrow_pos("IN_END", "", token)
#        case Context.CASE:
#            pass
#        case Context.RECORD:
#            match token.type:
#                case "RBRACE":
#                    if token.line == item.line or token.column == item.column:
#                        return None
#                    return make_error_token(
#                        token,
#                        "a closing `}` must be at the same level that it's corresponding `{`, maybe you miss one `}` or it's indentation? , maybe  the `{` at line {item.originated_by.line}, column {item.originated_by.column} is the one that you try to close?.",
#                    )
#                case _:
#                    return make_error_token(
#                        token, "expected `}` to close a `{` but wasn't found"
#                    )
#
#
#def gen_close_tokens_after(state: IndenterState, token: Token) -> list[Token]:
#    log.debug(f"gen_close_tokens_after state: {state}")
#    log.debug(f"token: {repr(token)} {token.column}")
#    last_context = state.stack[-1]
#    acc = []
#    while last_context.column > token.column:  # type: ignore
#        log.debug(f"last_context: {last_context}, token: {token.column}, {token},{last_context.column}")
#        if len(state.stack) > 1:
#            previous_context = state.stack[-2]
#        else:
#            previous_context = None
#        log.debug(f"previous_context : {previous_context}")
#        maybe_closed_token = gen_close_context(last_context, previous_context, token)
#        if isinstance(maybe_closed_token, Token):
#            log.debug("generated token for close of context")
#            acc.append(maybe_closed_token)
#        state.stack.pop()
#        last_context = state.stack[-1]
#    return acc
#
#
#def set_expectation_at_next_token(
#    state: IndenterState,
#    context: Context,
#    token: Token,
#    error_msg: str,
#    use_level: Optional[int],
#):
#    log.debug("setting expectation for indentation")
#    if use_level is None:
#        level = token.column + 1  # type: ignore
#    else:
#        level = use_level
#    state.expects = NextTokenAtleastAt(
#        token,
#        level,
#        error_msg,
#        lambda tok: Item(context, tok.column, tok.line, token),  # type: ignore
#    )
#    log.debug(state.expects)
#
#
#def handle_indentation(
#    info: FileInfo, previousState: Optional[IndenterState], stream: Iterable[Token]
#) -> Iterable[IndenterError | Token]:
#    log.debug(f"begin_with {info} {previousState}")
#    if previousState is None:
#        state = IndenterState()
#    else:
#        state = previousState
#
#    log.debug(f"initial state {state}")
#
#    for token in stream:
#        log.debug("--------------------------------------------------------------------------------")
#        log.debug(f"Beginning new loop")
#        log.debug(f"Current token in stream: {token}")
#        log.debug(f"Current state: {state}")
#
#        if not state.expects is None:
#            if token.column < state.expects.column:
#                log.debug("new token is below expected indentation, reporting error")
#                error_token = make_error_token(token, state.expects.error_msg)
#                yield error_token
#                return
#            else:
#                log.debug("new token pass the indentation threshold, cleaning state")
#                new_context = state.expects.gen_item(token)
#                state.append(new_context)
#                state.expects = None
#        
#        found_fail_while_closing = False
#        for close_token in gen_close_tokens_after(state, token):
#            log.debug(f"emitting closing token {repr(close_token)}")
#            yield close_token
#            if close_token.type == "IndentationError":
#                log.info("a failure happened while closing context indentation.")
#                found_fail_while_closing = True
#
#        if found_fail_while_closing :
#            log.debug("returning due to a fail while closing indentation")
#            return
#        
#
#        last_context = state.stack[-1].context
#        match token.type:
#            case "EQUAL":
#                match last_context:
#                    case Context.ROOT:
#                        set_expectation_at_next_token(
#                            state,
#                            Context.EQUAL,
#                            token,
#                            "expected a definition after a equal",
#                            # In this case as the file is segmented, if something
#                            # got wrong, means that the segment input ended
#                            # in other words in the original file we got
#                            # something like:
#                            # "a = \n\nc=2"
#                            1,
#                        )
#                        yield token
#                    case Context.LET:
#                        set_expectation_at_next_token(
#                            state,
#                            Context.EQUAL,
#                            token,
#                            "expected indentation to be greater than the '=' for let",
#                            None,
#                        )
#                        yield token
#                    case Context.RECORD:
#                        set_expectation_at_next_token(
#                            state,
#                            Context.EQUAL,
#                            token,
#                            "expected indentation to be greater than the '=' in the record",
#                            None,
#                        )
#                        yield token
#            case "LET":
#                set_expectation_at_next_token(
#                    state,
#                    Context.LET,
#                    token,
#                    "expected a definition inside `let` with indentation greater than the `let`.",
#                    None,
#                )
#                yield token
#
#            case "IN":
#                # TODO: If we have nested lets, we end here but with the inner one already closed
#                # add a way to avoid doing this as we are closing a second context with 
#                # the same token.
#                # Two cases,
#                # in same line:
#                # let a = b in c
#                # in other line:
#                # let
#                #   a = b
#                # in
#                #  c
#                # In the first case we need to close a equal context
#                # then close the let context and append a `IN` context
#                # In the second case, the call to `gen_close_tokens_after`
#                # before the this `match` must handle it if it is right.
#                # If there is another kind of indentation error for second
#                # case, like putting the `in` in a different column than
#                # it's let, this leads to don't remove let context and
#                # introduce the `in` context. This context would be
#                # removed at the end of this function call and then
#                # it would degenerate in a IndentationError.
#                log.debug("trying to remove let context as we found a `in`")
#                current_item = state.stack[-1]
#                match current_item.context:
#                    case Context.EQUAL:
#                        log.debug("trying to remove a equal context")
#                        # this is guaranteed as we always have a ROOT in the stack
#                        previous_item = state.stack[-2]
#                        maybe_new = gen_close_context(current_item, previous_item, token)
#                        if maybe_new is None:
#                            log.debug("failed to remove let context")
#                            yield make_error_token(token, "unexpected `in`.")
#                            return
#                        state.stack.pop()
#                        log.debug("removed equal context")
#                        log.debug(state)
#                        yield maybe_new
#                        log.debug("trying to remove a let context")
#                        if previous_item.context != Context.LET:
#                            log.debug(f"let context not found in place, got {previous_item}")
#                            yield make_error_token(token, "unexpected `in`.")
#                            return
#                        maybe_close_let = gen_close_context(previous_item, None, token)
#                        if not maybe_close_let is None:
#                            log.debug(f"failed to remove let context, got back a {maybe_close_let}")
#                            yield maybe_close_let
#                            return
#
#                        state.stack.pop()
#                        log.debug("removed let context")
#                        log.debug(state)
#                        state.append(Item(Context.IN, token.column, token.line, token))
#                        yield token
#                    case _:
#                        log.debug("just passing the in token")
#                        state.append(Item(Context.IN, token.column, token.line, token))
#                        yield token
#            case _:
#                log.debug(f"nothing to do, yielding token {token}")
#                yield token
#
#    log.debug("End of loop")
#    log.debug(f"state: {state}")
#    log.debug(f"last token: {token}")
#
#    if not state.expects is None:
#        log.debug("End of input while we expected more tokens")
#        error_token = make_error_token(token, state.expects.error_msg)
#        yield error_token
#        return
#
#    eof_token = Token("EOF", "EOF")
#    eof_token.column = 0
#    for close_token in gen_close_tokens_after(state, eof_token):
#        log.debug(f"emitting closing tokens for remaining context {repr(close_token)}")
#        yield close_token


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
