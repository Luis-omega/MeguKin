from typing import Optional, Iterable
from pathlib import Path
import re

from lark import (
    Lark,
    Tree,
    UnexpectedInput,
    GrammarError,
    UnexpectedCharacters,
    UnexpectedToken,
)
from lark.exceptions import ConfigurationError

from MeguKin.Error import MeguKinError
from MeguKin.Parser.Indentation import Indenter
from MeguKin.Parser import Indentation
from MeguKin.Parser.Token import Token
from MeguKin.File import FileInfo
from MeguKin.Pretty import DocumentT, Text, AlwaysLineBreak, Nil
from dataclasses import dataclass


class ParserStageError(MeguKinError):
    pass


class LoadGrammarError(ParserStageError):
    pass


@dataclass
class LarkLoadError(ParserStageError):
    msg: str


@dataclass
class FileLoadError(ParserStageError):
    info: FileInfo


@dataclass
class ParserError(ParserStageError):
    document: DocumentT


def load_grammar(
    debug: Optional[bool] = None, start_symbols: Optional[list[str]] = ["top"]
) -> LoadGrammarError | LarkLoadError | Lark:
    if debug is None:
        debug = False
    grammarPath = "MeguKin/Parser/grammar.lark"
    if start_symbols is None:
        start_symbols = ["top"]
    try:
        with open(grammarPath, "r") as grammarFile:
            grammar = grammarFile.read()
            try:
                parser = Lark(
                    grammar,
                    start=start_symbols,
                    debug=debug,
                    cache=None,
                    propagate_positions=False,
                    maybe_placeholders=True,
                    keep_all_tokens=True,
                    parser="lalr",
                    lexer="basic",
                    postlex=Indenter(),
                )
            except Exception as e:
                return LarkLoadError(str(e))
    except OSError:
        return LoadGrammarError()
    return parser


def parse_string(
    lark: Lark, info: FileInfo, text: str
) -> ParserError | Tree[Token]:
    try:
        result = lark.parse(text)
        return result  # type:ignore
    except UnexpectedInput as uinput:
        return make_parser_error_from_lark(uinput, text, info)


def parse(
    path: Path, lark: Lark, debug: bool
) -> FileLoadError | tuple[FileInfo, ParserError | Tree[Token]]:
    info = FileInfo(path.name, path)
    try:
        with open(path, "r") as file:
            content = file.read()
            maybe_tree = parse_string(lark, info, content)
            return (info, maybe_tree)
    except OSError:
        return FileLoadError(info)


def make_parser_error_from_lark(
    error: UnexpectedInput, text: str, info: FileInfo
) -> ParserError:
    info_doc = (
        Text(f"At file: {info.path}")
        + AlwaysLineBreak()
        + Text(f"At Module: {info.name}")
    )
    if isinstance(error, UnexpectedToken):
        token = error.token
        print(type(token.value))
        if isinstance(token.value, Indentation.LayoutError):
            if isinstance(token.value, Indentation.MissMatchIndentation):
                context = make_error_context_from_tokens(
                    text, token.value.start_token, token.value.end_token
                )
                match token.value:
                    case Indentation.LayoutClosedByBadToken():
                        msg = (
                            "Unexpected indentation, for "
                            f"{repr_token(token.value.end_token)}, while handling the "
                            f"indentation introduced by {repr_token(token.value.start_token)}."
                        )
                    case Indentation.ContextFirstValueBeforeStart():
                        if token.value.force:
                            msg = (
                                "Unexpected indentation, for "
                                f"{repr_token(token.value.end_token)}, we expected it to have indentation."
                            )
                        else:
                            msg = (
                                "Unexpected indentation, for "
                                f"{repr_token(token.value.end_token)}, we expected it to be greater than the "
                                f"indentation introduced by {repr_token(token.value.start_token)}."
                            )
                    case Indentation.LetInMaybeMisplaced():
                        msg = (
                            "Unexpected indentation, for"
                            f"{repr_token(token.value.end_token)}, we expected it to be lower than the "
                            f"indentation introduced by {repr_token(token.value.start_token)}."
                        )

                    case _:
                        raise Exception(
                            "Unhandled error in make_parser_error_from_lark"
                        )

                return ParserError(
                    info_doc
                    + AlwaysLineBreak()
                    + AlwaysLineBreak()
                    + Text(msg)
                    + AlwaysLineBreak()
                    + AlwaysLineBreak()
                    + context
                )

            elif isinstance(token.value, Indentation.UnexpectedEOF):
                context = get_line_error_from_token(
                    text, token.value.report_at_token
                )
                msg = f"Unexpected end of input, at {repr_token(token.value.report_at_token)}"
                return ParserError(
                    info_doc
                    + AlwaysLineBreak()
                    + AlwaysLineBreak()
                    + Text(msg)
                    + AlwaysLineBreak()
                    + AlwaysLineBreak()
                    + context
                )
        elif (
            token.type == "LAYOUT_END"
            or token.type == "LAYOUT_SEPARATOR"
            or token.type == "LAYOUT_START"
        ):
            context = get_line_error_from_token(text, token)
            msg = f"Unexpected indentation error, at line {token.line}, column {token.column}"
            return ParserError(
                info_doc
                + AlwaysLineBreak()
                + AlwaysLineBreak()
                + Text(msg)
                + AlwaysLineBreak()
                + AlwaysLineBreak()
                + context
            )
        else:
            context = get_line_error_from_token(text, token)
            msg1 = f"Unexpected {repr_token(token)}"
            msg2 = error._format_expected(error.accepts or error.expected)
            return ParserError(
                info_doc
                + AlwaysLineBreak()
                + AlwaysLineBreak()
                + Text(msg1)
                + AlwaysLineBreak()
                + AlwaysLineBreak()
                + context
                + Text(msg2)
            )

    return ParserError(
        info_doc + AlwaysLineBreak() + AlwaysLineBreak() + Text(str(error))
    )


def repr_token(token: Token):
    return f"{repr(token)} at line {token.line}, column {token.column}"


def make_error_context_from_tokens(
    text: str, start_token: Token, end_token: Token
) -> DocumentT:
    start_doc = get_line_error_from_token(text, start_token)

    if start_token.line == end_token.line:
        return start_doc

    last_doc = get_line_error_from_token(text, end_token)

    if start_token.line == end_token.line + 1:
        return start_doc + AlwaysLineBreak() + AlwaysLineBreak() + last_doc

    raw_content = text[start_token.start_pos : end_token.end_pos]
    content = raw_content.expandtabs(1).split("\n")[1:-1]

    doc: DocumentT = Nil()
    for line in content:
        doc = doc + AlwaysLineBreak() + Text(line)

    return start_doc + AlwaysLineBreak() + doc + AlwaysLineBreak() + last_doc


def get_line_error_from_token(text, token: Token) -> DocumentT:
    line = get_line_from_pos(text, token.start_pos)
    token_lenght = token.end_column - token.column
    if token_lenght <= 0:
        token_lenght = 1

    doc = (
        Text(line)
        + AlwaysLineBreak()
        + Text((token.column - 1) * " " + token_lenght * "^")
        + AlwaysLineBreak()
    )
    return doc


def get_line_from_pos(text: str, pos: int) -> str:
    """
    The returned str already has tabs replaced,
    also it removes the "\n" from it.
    """
    span = 500
    init_raw = text[max(0, pos - span) : pos]
    init_split = init_raw.rsplit("\n", 1)
    init_last = init_split[-1]
    init = init_last.expandtabs(1)

    end_condition = min(pos + span, len(text))
    end_raw = text[pos:end_condition]
    end_split = end_raw.split("\n", 1)
    end_last = end_split[0]
    end = end_last.expandtabs(1)
    return init + end
