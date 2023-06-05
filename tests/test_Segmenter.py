from pprint import pprint

from MeguKin.Parser.SegmentFile import (
    segment_str,
    no_has_spaces_at_start,
    find_first_non_empty_zero_indent,
)
from MeguKin.File import FileInfo


class TestSegmenter:
    @staticmethod
    def test_empty_checker():
        raw = ""
        assert no_has_spaces_at_start(raw) == False

    @staticmethod
    def test_space_checker():
        raw = " "
        assert no_has_spaces_at_start(raw) == False

    @staticmethod
    def test_line_break_checker():
        raw = "\n"
        assert no_has_spaces_at_start(raw) == False

    @staticmethod
    def test_find_first_empty():
        raw = ""
        assert find_first_non_empty_zero_indent(raw) is None

    @staticmethod
    def test_find_first_only_line_breaks():
        raw = ["\n", "\n", "\n", "\n"]
        assert find_first_non_empty_zero_indent(raw) is None

    @staticmethod
    def test_find_first_only_spaces_and_line_breaks():
        raw = [" \n", "\n \n", "\n\n \n", "    \n"]
        assert find_first_non_empty_zero_indent(raw) is None

    @staticmethod
    def test_find_first_no_spaces():
        raw = ["asdfas\n"]
        assert find_first_non_empty_zero_indent(raw) == ("asdfas\n", 1)

    @staticmethod
    def test_one_segment():
        raw = ["    \n", "\n", "a\n", "\n", " \n"]
        segment = list(segment_str(FileInfo("a", "b"), raw))
        pprint(segment)
        assert len(segment) == 1
        assert segment[0].line_start == 3
        assert segment[0].line_end == 3
        assert segment[0].segment == "a\n"
