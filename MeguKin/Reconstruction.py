from lark import Token


class Range:
    line_start: int
    line_end: int
    column_start: int
    column_end: int
    position_start: int
    position_end: int

    def __init__(
        self,
        line_start: int,
        line_end: int,
        column_start: int,
        column_end: int,
        position_start: int,
        position_end: int,
    ) -> None:
        self.line_start = line_start
        self.line_end = line_end
        self.column_start = column_start
        self.column_end = column_end
        self.position_start = position_start
        self.position_end = position_end

    def pretty(self):
        return f"line:{self.line_start}, column={self.column_start}"

    def __str__(self):
        return repr(self)

    def __repr__(self):
        return f"Range(line_start={self.line_start},line_end={self.line_end},column_start={self.column_start},column_end={self.column_end})"


def token2Range(token: Token) -> Range:
    # type: ignore
    return Range(
        token.line,
        token.end_line,
        token.column,
        token.end_column,
        token.start_pos,
        token.end_pos,
    )


def mergeRanges(range1: Range, range2: Range) -> Range:
    if range1.position_start <= range2.position_start:
        if range1.position_end <= range2.position_end:
            return Range(
                range1.line_start,
                range2.line_end,
                range1.column_start,
                range2.column_end,
                range1.position_start,
                range2.position_end,
            )
        else:
            return range1
    else:
        return mergeRanges(range2, range1)
