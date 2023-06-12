from dataclasses import dataclass
from typing import TypeVar, Union
from enum import Enum, auto
from abc import ABC, abstractmethod

from MeguKin.Loggers import get_logger


log = get_logger(__name__)

T = TypeVar("T")

DocumentT = Union[
    "Nil",
    "Concat",
    "Indent",
    "Text",
    "LineBreak",
    "Group",
    "MaybeIndent",
    "NoSpaceLineBreak",
    "AlwaysLineBreak",
]

SimpleDocumentT = Union["SimpleNil", "SimpleText", "SimpleIndent"]


@dataclass
class DocumentSettings:
    indentation: int  # number of spaces for indentation
    line_length: int
    leading_comes: bool  # ignored for now as we enforce leading commas
    leading_arrow: bool  # ignored for now as we enforce leading arrows


defaultSettings = DocumentSettings(4, 80, True, True)


class ToDocument(ABC):
    @abstractmethod
    def to_document(self, settings: "DocumentSettings") -> DocumentT:
        """
        Convert the object to a document
        """


class Mode(Enum):
    Flat = auto()
    Break = auto()


class Document(ABC):
    def __add__(self, other: "Document") -> "Concat":
        if isinstance(other, Document):
            return Concat(self, other)  # type:ignore
        else:
            raise Exception(f"Bad concatenation of document with {other}")


@dataclass
class Nil(Document):
    pass


@dataclass
class Concat(Document):
    document1: DocumentT
    document2: DocumentT


@dataclass
class Indent(Document):
    level: int
    document: DocumentT


@dataclass
class Text(Document):
    text: str


@dataclass
class LineBreak(Document):
    pass


@dataclass
class NoSpaceLineBreak(Document):
    pass


@dataclass
class AlwaysLineBreak(Document):
    pass


@dataclass
class Group(Document):
    document: DocumentT


@dataclass
class MaybeIndent(Document):
    level: int
    document: DocumentT


class SimpleDocument(ABC):
    pass


@dataclass
class SimpleNil(SimpleDocument):
    pass


@dataclass
class SimpleText(SimpleDocument):
    text: str
    document: SimpleDocumentT


@dataclass
class SimpleIndent(SimpleDocument):
    level: int
    document: SimpleDocumentT


# mypy can't check this complex pattern match for exhaustion
def fits(  # type:ignore
    width: int, stack: list[tuple[int, Mode, DocumentT]]
) -> bool:
    if width < 0:
        return False
    # TODO: If performance becames a burden, we can convert this to loop.
    match stack:
        case []:
            return True
        case [*others, (i, _, Nil())]:
            return fits(width, others)
        case [*others, (i, m, Concat(left, right))]:
            others.append((i, m, right))
            others.append((i, m, left))
            return fits(width, others)
        case [*others, (i, m, Indent(level, doc))]:
            others.append((i + level, m, doc))
            return fits(width, others)
        case [*others, (i, m, Text(text))]:
            return fits(width - len(text), others)
        case [*others, (i, Mode.Flat, LineBreak())]:
            return fits(width - 1, others)
        case [*others, (i, Mode.Break, LineBreak())]:
            return True  # imposible we only call this outside in mode `flat`
            # and nothe that we always propate state in this function
        case [*others, (i, Mode.Flat, NoSpaceLineBreak())]:
            return fits(width, others)
        case [*others, (i, Mode.Break, NoSpaceLineBreak())]:
            return True
        case [*others, (i, Mode.Flat, AlwaysLineBreak())]:
            return fits(width, others)
        case [*others, (i, Mode.Break, AlwaysLineBreak())]:
            return True
        case [*others, (i, Mode.Flat, Group(doc))]:
            others.append((i, Mode.Flat, doc))
            return fits(width, others)
        case [*others, (i, m, MaybeIndent(_, doc))]:
            others.append((i, Mode.Flat, doc))
            return fits(width, others)
        case [*others, (i, Mode.Flat, MaybeIndent(level, doc))]:
            others.append((i, Mode.Flat, doc))
            return fits(width, others)
        case [*others, (i, Mode.Break, MaybeIndent())]:
            return True  # impossible read before


def format_inner(  # type:ignore
    width: int, consumed: int, stack: list[tuple[int, Mode, DocumentT]]
) -> SimpleDocumentT:
    if stack:
        log.debug(
            f"format_inner, width:{width}, consumed:{consumed}, stack_tip:{stack[-1]} "
        )
    match stack:
        case []:
            return SimpleNil()
        case [*others, (_, _, Nil())]:
            return format_inner(width, consumed, others)
        case [*others, (i, m, Concat(left, right))]:
            others.append((i, m, right))
            others.append((i, m, left))
            return format_inner(width, consumed, others)
        case [*others, (i, m, Indent(level, doc))]:
            others.append((i + level, m, doc))
            return format_inner(width, consumed, others)
        case [*others, (i, m, Text(text))]:
            return SimpleText(
                text, format_inner(width, (consumed + len(text)), others)
            )
        case [*others, (i, Mode.Flat, LineBreak())]:
            return SimpleText(
                " ",
                format_inner(width, consumed + 1, others),
            )
        case [*others, (i, Mode.Break, LineBreak())]:
            return SimpleIndent(i, format_inner(width, i, others))
        case [*others, (i, Mode.Flat, NoSpaceLineBreak())]:
            return SimpleText(
                "",
                format_inner(width, consumed, others),
            )
        case [*others, (i, Mode.Break, NoSpaceLineBreak())]:
            return SimpleIndent(i, format_inner(width, i, others))
        case [*others, (i, m, Group(doc))]:
            others2 = others.copy()
            others2.append((i, Mode.Flat, doc))
            if fits(width - consumed, others2):
                others.append((i, Mode.Flat, doc))
                return format_inner(width, consumed, others)
            else:
                others.append((i, Mode.Break, doc))
                return format_inner(width, consumed, others)
        case [*others, (i, m, AlwaysLineBreak())]:
            return SimpleIndent(i, format_inner(width, i, others))
        case [*others, (i, m, MaybeIndent(level, doc))]:
            others2 = others.copy()
            others2.append((i, Mode.Flat, doc))
            if fits(width - consumed, others2):
                others.append((i, Mode.Flat, doc))
                return format_inner(width, consumed, others)
            else:
                others.append((i, Mode.Break, Indent(level, doc)))
                return format_inner(width, consumed, others)
    log.debug("format_inner: Bad case reached")


def format(width: int, doc: DocumentT) -> SimpleDocumentT:
    return format_inner(width, 0, [(0, Mode.Flat, Group(doc))])


def layout(settings: DocumentSettings, doc: SimpleDocumentT):
    # TODO: maybe use a list and a loop here to improve
    # performance in the future?
    indentation_size = settings.indentation
    match doc:
        case SimpleNil():
            return ""
        case SimpleText(text, doc2):
            rended_text = text
            return rended_text + layout(settings, doc2)
        case SimpleIndent(level, doc2):
            print("To indent: ", level, doc2)
            return (
                "\n" + (level * indentation_size) * " " + layout(settings, doc2)
            )


def pretty(
    settings: DocumentSettings,
    doc: DocumentT,
):
    return layout(settings, format(settings.line_length, doc))


def pretty_as_console(doc: DocumentT) -> str:
    return pretty(defaultSettings, doc)


def parens(settings: DocumentSettings, doc: DocumentT):
    # the level of indentation came back to the initial at the end
    # so we can be sure the last parentheses behaves right.
    return Text("(") + MaybeIndent(1, doc) + Text(")")


def indent(doc: DocumentT):
    return Indent(1, LineBreak() + doc)


def maybe_indent(doc: DocumentT):
    return MaybeIndent(1, LineBreak() + doc)
