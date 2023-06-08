from typing import Optional, Iterable
from pathlib import Path

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


class ParserError(ParserStageError):
    pass


@dataclass
class LarkParseError(ParserError):
    text: str
    exception: UnexpectedInput
    info: FileInfo


@dataclass
class LayoutError(ParserError):
    msg: str
    line: int
    column: int
    info: FileInfo


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
        return build_parser_error_from_lark(uinput, text, info)


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


def build_parser_error_from_lark(
    error: UnexpectedInput, text: str, info: FileInfo
) -> ParserError:
    if isinstance(error, UnexpectedToken):
        token = error.token
        if isinstance(token.value, Indentation.LayoutError):
            match token.value:
                case Indentation.UnexpectedEOF(
                    report_at_token=report_token, msg=msg
                ):
                    return LayoutError(
                        msg, report_token.line, report_token.column, info
                    )
                case _:
                    return LayoutError(
                        token.value.msg, token.line, token.column, info
                    )
    return LarkParseError(text, error, info)
