from lark import Lark
from MeguKin.Transform import ToAST
from MeguKin.Types.Top import Top


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


def test_variable_declaration():
    rountrip_test(parser_for_test, "a : (A)")


parser_for_test = load_grammar()
