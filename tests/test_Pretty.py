from typing import Iterable
import re
from pprint import pprint

from MeguKin.Pretty import (
    DocumentT,
    Text,
    Nil,
    Concat,
    LineBreak,
    pretty,
    defaultSettings,
    Group,
    DocumentSettings,
    Indent,
    Text,
)


def make_seetings(indent: int):
    return DocumentSettings(4, indent, True, True)


def split_string(s: str) -> list[str]:
    to_separate = re.compile(r"[^\n]|\n")
    return to_separate.findall(s)


def str2Document(s: str) -> DocumentT:
    words = split_string(s)
    doc: DocumentT = Nil()
    doc_temp: DocumentT
    for word in words[::-1]:
        if word == "\n":
            doc_temp = LineBreak()
        else:
            doc_temp = Text(word)
        doc = Group(Concat(doc_temp, doc))
    return doc


def test_same_line_one_char():
    raw = "a"
    document = str2Document(raw)
    p = pretty(defaultSettings, document)
    assert raw == p


def test_same_line_short_enough():
    raw = "a\nb"
    document = str2Document(raw)
    p = pretty(defaultSettings, document)
    assert "a b" == p


def test_same_line_to_short():
    raw = "a\nb\nc\nd"
    document = str2Document(raw)
    pprint(document)
    settigns = make_seetings(5)
    p = pretty(settigns, document)
    assert "a\nb c d" == p


def test_indent_simple():
    document = Text("a") + Indent(
        1, LineBreak() + Text("b") + LineBreak() + Text("c")
    )
    pprint(document)
    settigns = make_seetings(1)
    p = pretty(settigns, document)
    assert "a\n    b\n    c" == p
