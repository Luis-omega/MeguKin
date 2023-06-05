from pathlib import Path
from typing import NamedTuple, Iterable, Optional
import re

import logging

from MeguKin.Error import MeguKinError
from MeguKin.File import FileInfo, Range

log = logging.getLogger(__name__)
log.setLevel(logging.DEBUG)


class SegmentFileError(MeguKinError):
    pass


class NoStartFound(SegmentFileError):
    info: FileInfo

    def __init__(self, info: FileInfo):
        self.info = info


FileSegment = NamedTuple(
    "FileSegment",
    [
        ("segment", str),
        ("info", FileInfo),
        ("line_start", int),
        ("line_end", int),
    ],
)


def no_has_spaces_at_start(s: str):
    """
    Only matches " "  and "\n" at line start
    or empty lines
    """
    return len(s) != 0 and s[0] != " " and s[0] != "\n"


def find_first_non_empty_zero_indent(lines) -> Optional[tuple[str, int]]:
    """
    Look up until we consume all the lines or get a non empty
    line with no spaces at the start.
    """
    counter = 0
    for line in lines:
        counter += 1
        if no_has_spaces_at_start(line):
            return (line, counter)
    return None


# def segment_str(
#    info: FileInfo, file_lines: list[str]
# ) -> Iterable[FileSegment] | SegmentFileError:
#    maybe_first = find_first_non_empty_zero_indent(file_lines)
#    if maybe_first is None:
#        return NoStartFound(info)
#    first_line, line_count = maybe_first
#    # FIXME: check that the first block is really empty.
#    segment_raw = [first_line]
#    segment_start = line_count
#    for line in file_lines[line_count + 1 :]:
#        print("thinking of: ", line)
#        line_count += 1
#        if no_has_spaces_at_start(line):
#            segment_raw.append(line)
#        elif len(segment_raw) != 0:
#            yield FileSegment(
#                "\n".join(segment_raw), info, segment_start, line_count - 1
#            )
#            segment_start = line_count
#            segment_raw = []
#    return


def segment_str(info: FileInfo, text: str) -> list[FileSegment]:
    segments = re.split("\n(?=[^\n ])", text)
    for segment in segments:
        out.append(FileSegment)
