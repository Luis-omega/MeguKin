from typing import Optional, Iterable
from pathlib import Path

from lark import Lark, Tree, UnexpectedInput, GrammarError
from lark.exceptions import ConfigurationError

from MeguKin.Error import MeguKinError
from MeguKin.Parser.SegmentFile import (
    segment_file,
    FileSegment,
)
from MeguKin.File import FileInfo


class LoadGrammarError(MeguKinError):
    pass


class LarkLoadError(MeguKinError):
    pass


class ParserError(MeguKinError):
    pass


class FileLoadError(MeguKinError):
    pass


def load_grammar(
    debug: Optional[bool] = None,
) -> LoadGrammarError | LarkLoadError | Lark:
    if debug is None:
        debug = False
    grammarPath = "MeguKin/Parser/grammar.lark"
    startSymbols = ["top"]
    try:
        with open(grammarPath, "r") as grammarFile:
            grammar = grammarFile.read()
            try:
                parser = Lark(
                    grammar,
                    start=startSymbols,
                    debug=debug,
                    cache=None,
                    propagate_positions=True,
                    maybe_placeholders=True,
                    keep_all_tokens=True,
                    parser="lalr",
                    lexer="basic",
                )
            except:
                return LarkLoadError()
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
        for segment in segment_file(lines):
            yield parse_segment(lark, segment)

    return (FileInfo(path.name, path), parse_inner())


def parse_segment(lark: Lark, segment: FileSegment) -> ParserError | Tree:
    try:
        return lark.parse(segment.segment)
    except UnexpectedInput as e:
        return build_parser_error_from_lark(e)


def build_parser_error_from_lark(error: UnexpectedInput) -> ParserError:
    pass
