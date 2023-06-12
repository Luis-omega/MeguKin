from MeguKin.SugaredSyntaxTree.PatternMatch import (
    PatternMatchT,
    PatternMatchVariable,
)
from MeguKin.Error import MeguKinError

from typing import Union

BoundErrorT = Union[int]


class MeguKinBoundError(MeguKinError):
    pass


def get_pattern_match_bound_variables(
    pattern: PatternMatchT,
) -> list[BoundErrorT] | list[PatternMatchVariable]:
    match pattern:
        case PatternMatchVariable(prefix, name):
            pass

        case _:
            raise Exception(
                "MeguKin: Unhandled case {pattern} in get_pattern_match_bound_variables"
            )
