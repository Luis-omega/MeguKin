from typing import Optional, Iterable
from pathlib import Path

from lark import Lark, Tree, UnexpectedInput, GrammarError
from lark.exceptions import ConfigurationError

from MeguKin.Error import MeguKinError
from MeguKin.Parser.SegmentFile import (
    segment_str,
    FileSegment,
)
from MeguKin.Parser.Indentation import Indenter
from MeguKin.File import FileInfo


class LoadGrammarError(MeguKinError):
    pass


class LarkLoadError(MeguKinError):
    msg: str

    def __init__(self, msg: str):
        self.msg = msg

    def __repr__(self):
        return f'LarkLoadError("{self.msg}")'


class ParserError(MeguKinError):
    pass


class FileLoadError(MeguKinError):
    pass


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


def parse(
    path: Path, lark: Lark, debug: bool
) -> FileLoadError | tuple[FileInfo, Iterable[ParserError | Tree]]:
    lines: list[str]
    try:
        with open(path, "r") as file:
            lines = file.readlines()
    except OSError:
        return FileLoadError()

    def parse_inner():
        for segment in segment_str(lines):
            yield parse_segment(lark, segment)

    return (FileInfo(path.name, path), parse_inner())


def parse_segment(lark: Lark, segment: FileSegment) -> ParserError | Tree:
    try:
        return lark.parse(segment.segment)
    except UnexpectedInput as e:
        return build_parser_error_from_lark(e)


def build_parser_error_from_lark(error: UnexpectedInput) -> ParserError:
    pass
