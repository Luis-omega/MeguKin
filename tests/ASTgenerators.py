from typing import List

from hypothesis.strategies import composite, integers, from_regex, text, lists, integers

# from Megukin.AST.Types import
from MeguKin.Ast.Types.Expression import (
    Literal,
    AnnotatedExpression,
    Variable,
    Application,
    Function,
    OperatorsWithoutMeaning,
    Let,
    LetBinding,
)

from MeguKin.Ast.Types.PatternMatch import PatternMatchVariable, PatternMatchConstructor
from MeguKin.Ast.Types.Type import TypeName, TypeArrow
from MeguKin.Ast.Types.Top import Definition, Declaration, DataType, Constructor

from MeguKin.Reconstruction import Range

emptyRange = Range(0, 0, 0, 0, 0, 0)


@composite
def gen_literal(draw):
    generated = draw(integers(0))
    return Literal(str(generated), emptyRange)


@composite
def gen_operator(draw):
    return draw(gen_composite_operator() | gen_operator_alone_character())


@composite
def gen_composite_operator(draw):
    tail = draw(text(gen_operator_alone_character(), min_size=1))
    head = draw(gen_operator_alone_character() | gen_operator_not_alone_character())
    return head + tail


@composite
def gen_operator_alone_character(draw):
    return draw(text([i for i in "+-~/!?¡¿$¬><%&*"], min_size=1, max_size=1))


@composite
def gen_operator_not_alone_character(draw):
    char = draw(text("=\\|", min_size=1, max_size=1))
    return char


@composite
def gen_lowercasse_identifier(draw):
    value = draw(from_regex(regex=r"[a-z][a-zA-Z_0-9]*", fullmatch=True))
    return value


@composite
def gen_capitalized_identifier(draw):
    value = draw(from_regex(r"[A-Z][a-zA-Z_0-9]*", fullmatch=True))
    return value


@composite
def gen_variable_lower(draw):
    value = draw(gen_lowercasse_identifier())
    return Variable(value, False, emptyRange, set(value))


@composite
def gen_variable_operator(draw):
    value = draw(gen_operator())
    return Variable(value, False, emptyRange, set(value))


@composite
def gen_expression_application(draw):
    function = draw(gen_function())
    argument = draw(gen_expression())
    free_variables = function.free_variables.union(argument.free_variables)
    return Application(function, argument, emptyRange, free_variables)


@composite
def gen_function(draw):
    pattern = draw(gen_pattern_match())
    value = draw(gen_expression())
    free_variables = value.free_variables - pattern.bound_variables
    return Function(pattern, value, emptyRange, free_variables)


@composite
def gen_annotated_expression(draw):
    expression = draw(gen_expression())
    annotation = draw(gen_type())
    free_variables = expression.free_variables
    return AnnotatedExpression(expression, annotation, emptyRange, free_variables)


@composite
def gen_operators_expression(draw):
    first = draw(gen_expression())
    out = [first]
    free_variables = first.free_variables
    number_of_operators = draw(integers(0, 6))
    if number_of_operators == 0:
        return first
    for i in range(number_of_operators):
        new_expression = draw(gen_expression())
        new_operator = draw(gen_operator())
        out.append(new_operator)
        out.append(new_expression)
        free_variables.union(new_operator, new_expression.free_variables)

    return OperatorsWithoutMeaning(out, emptyRange, free_variables)


@composite
def gen_expression_let_binding(draw):
    name = draw(gen_lowercasse_identifier())
    expression = draw(gen_expression())
    return LetBinding(name, expression, emptyRange, expression.free_variables)


@composite
def gen_expression_let(draw):
    bindings = draw(lists(gen_expression_let_binding(), min_size=1, max_size=6))
    expression = draw(gen_expression())
    free_variables = set.union(*(i.free_variables for i in bindings))
    return Let(bindings, expression, emptyRange, free_variables)


@composite
def gen_expression(draw):
    return draw(
        gen_literal()
        | gen_variable_lower()
        | gen_variable_operator()
        | gen_function()
        | gen_annotated_expression()
        | gen_operators_expression()
        | gen_expression_application()
        | gen_expression_let()
    )


# -------------------------------- PatternMatch -----------------------------


@composite
def gen_pattern_match_variable(draw):
    name = draw(gen_lowercasse_identifier())
    return PatternMatchVariable(name, emptyRange, set(name))


@composite
def gen_pattern_match_constructor(draw):
    name = draw(gen_capitalized_identifier())
    patterns = draw(lists(gen_pattern_match(), max_size=5))
    if len(patterns) == 0:
        return PatternMatchConstructor(name, patterns, emptyRange, set())
    else:
        bound_variables = set.union(*(pattern.bound_variables for pattern in patterns))
        return PatternMatchConstructor(name, patterns, emptyRange, bound_variables)


@composite
def gen_pattern_match(draw):
    return draw(gen_pattern_match_constructor() | gen_pattern_match_variable())


# -------------------------------- Types -----------------------------


@composite
def gen_type_name(draw):
    name = draw(gen_capitalized_identifier())
    return TypeName(name, emptyRange)


# not included right now
# @composite
# def gen_type_application(draw):
#    function = draw(gen_type_arrow())
#    return TypeName(name, emptyRange)


@composite
def gen_type_arrow(draw):
    domain = draw(gen_type())
    codomain = draw(gen_type())
    return TypeArrow(domain, codomain, emptyRange)


@composite
def gen_type(draw):
    return draw(gen_type_arrow() | gen_type_name())


# -------------------------------- Top -----------------------------


@composite
def gen_top_definition(draw):
    name = draw(gen_lowercasse_identifier())
    expression = draw(gen_expression())
    return Definition(name, expression, emptyRange)


@composite
def gen_top_declaration(draw):
    name = draw(gen_lowercasse_identifier())
    _type = draw(gen_type())
    return Declaration(name, _type, emptyRange)


@composite
def gen_constructor_definition(draw):
    name = draw(gen_capitalized_identifier())
    types = draw(lists(gen_type(), min_size=1, max_size=6))
    return Constructor(name, types, emptyRange)


@composite
def gen_top_data_constructor(draw):
    name = draw(gen_capitalized_identifier())
    constructors = draw(lists(gen_constructor_definition(), min_size=1, max_size=6))
    return DataType(name, constructors, emptyRange)
