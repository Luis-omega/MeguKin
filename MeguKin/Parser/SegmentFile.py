from pathlib import Path
from typing import NamedTuple, Iterable

from MeguKin.Error import MeguKinError
from MeguKin.File import FileInfo, Range


class SegmentFileError(MeguKinError):
    pass


FileSegment = NamedTuple(
    "FileSegment", [("segment", str), ("info", FileInfo), ("range_", Range)]
)


def segment_file(file_lines: Iterable[str]) -> Iterable[FileSegment]:
    pass
