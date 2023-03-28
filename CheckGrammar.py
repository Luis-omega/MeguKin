from lark import Lark
from MeguKin.Transform import ToAST


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


if "__main__" == __name__:
    parser = load_grammar()
    import sys

    if len(sys.argv) == 2:
        try:
            result = parser.parse(sys.argv[1])
            print(result.pretty())
            astResult = ToAST().transform(result)
            print(astResult)
            for i in astResult:
                print(i.pretty())
        except Exception as e:
            print(e)
