from hypothesis.strategies import (
    composite,
    integers,
    from_regex,
    text,
    lists,
    integers,
    recursive,
    SearchStrategy,
    one_of,
)

from MeguKin.SugaredSyntaxTree.Expression import Variable
from MeguKin.File import Range

emptyRange = Range(0, 0, 0, 0, 0, 0)


@composite
def gen_simple_leter(draw) -> SearchStrategy[str]:
    generated = draw(from_regex(regex=r"[a-zA-Z]", fullmatch=True))
    return generated


@composite
def gen_lowercase_leter(draw) -> SearchStrategy[str]:
    generated = draw(from_regex(regex=r"[a-z]", fullmatch=True))
    return generated


@composite
def gen_upercasse_leter(draw) -> SearchStrategy[str]:
    generated = draw(from_regex(regex=r"[A-Z]", fullmatch=True))
    return generated


@composite
def gen_expression_simple_variable(draw) -> Variable:
    name = draw(gen_lowercase_leter())
    return Variable([], name, emptyRange)


@composite
def gen_expression_prefixed_variable(draw) -> Variable:
    prefix = draw(lists(gen_upercasse_leter(), min_size=1, max_size=3))
    name = draw(gen_lowercase_leter())
    return Variable(prefix, name, emptyRange)


gen_expression_variable: SearchStrategy[Variable] = one_of(
    gen_expression_simple_variable(), gen_expression_prefixed_variable()
)


def gen_from_string(s: str) -> SearchStrategy[Variable]:
    @composite
    def gen(draw) -> Variable:
        return Variable([], s, emptyRange)

    return gen()


gen_expression_variable2 = gen_lowercase_leter().flatmap(
    lambda x: gen_from_string(x)
)
