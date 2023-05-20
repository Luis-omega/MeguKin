from lark import Lark
import pytest
from hypothesis import given, settings, HealthCheck
from hypothesis.strategies import composite

from tests.ASTgenerators import (
    gen_top_data_constructor,
    gen_top_definition,
    gen_top_declaration,
)
from tests.ASTgenerators import *

# gen_top_definition, gen_expression

from MeguKin.SugaredSyntaxTree.Transform import ToAST
from MeguKin.SugaredSyntaxTree.Top import Top


def load_grammar() -> Lark:
    grammarPath = "MeguKin/grammar.lark"
    startSymbols = ["top"]
    with open(grammarPath, "r") as grammarFile:
        grammar = grammarFile.read()
        parser = Lark(
            grammar,
            start=startSymbols,
            debug=True,
            propagate_positions=True,
            maybe_placeholders=True,
            keep_all_tokens=True,
            parser="lalr",
            lexer="contextual",
        )
        return parser


def parseToASt(parser: Lark, input_to_parse: str) -> Top:
    parsedValue = parser.parse(input_to_parse)
    astValue = ToAST().transform(parsedValue)
    return astValue


def rountrip_test(parser: Lark, input_to_parse: str):
    astValue = parseToASt(parser, input_to_parse)
    stringValue = astValue.pretty().replace(" ", "")
    assert stringValue == input_to_parse.replace(" ", "")


def rountrip_pretty_test(parser: Lark, ast_value):
    print(ast_value.pretty())
    new_ast_value = parseToASt(parser, ast_value.pretty())
    assert ast_value.pretty() == new_ast_value.pretty()


variableDeclarations = [
    "a : (A)",
    "b : (A->B)",
    "c : (A->B->C)",
    "d : ((A->B)->C)",
    "e : ((A->B->C)->D) ",
]

variableDefinitions = [
    "a = (a)",
    "b = (a+b)",
    "c = (\\ x->(y))",
    "d = (\\x -> (a+b))",
    "e = (\\Some x -> (a))",
    "f = (\\Some x -> (a+b))",
    "g = (\\Some (Make Mine Better now) -> (a+b))",
]


@pytest.mark.parametrize("variableDeclaration", variableDeclarations)
def test_variable_declaration(variableDeclaration):
    rountrip_test(parser_for_test, variableDeclaration)


@pytest.mark.parametrize("variableDefinition", variableDefinitions)
def test_variable_definition(variableDefinition):
    rountrip_test(parser_for_test, variableDefinition)


def test_show_gen():
    for i in range(10):
        value = gen_expression().example().pretty()
        print(value)
    assert False


@given(value=gen_top_declaration())
@settings(suppress_health_check=list(HealthCheck))
def test_top_declaration(value):
    rountrip_pretty_test(parser_for_test, value)


@given(value=gen_top_definition())
@settings(suppress_health_check=list(HealthCheck))
def test_top_definition(value):
    rountrip_pretty_test(parser_for_test, value)


@given(value=gen_top_data_constructor())
@settings(suppress_health_check=list(HealthCheck))
def test_top_data(value):
    rountrip_pretty_test(parser_for_test, value)


parser_for_test = load_grammar()
