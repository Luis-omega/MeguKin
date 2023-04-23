from lark import Lark

# from MeguKin.Ast.Transform import ToAST


def load_grammar() -> Lark:
    grammarPath = "MeguKin/grammar.lark"
    startSymbols = ["top"]
    with open(grammarPath, "r") as grammarFile:
        grammar = grammarFile.read()
        parser = Lark(
            grammar,
            start=startSymbols,
            debug=True,
            cache=None,
            propagate_positions=True,
            maybe_placeholders=True,
            keep_all_tokens=True,
            parser="lalr",
            lexer="basic",
        )
        return parser


if "__main__" == __name__:
    parser = load_grammar()
    import sys

    if len(sys.argv) == 2:
        result = parser.parse(sys.argv[1])
        print(result.pretty())
        # astResult = ToAST().transform(result)
        # print(astResult)
        # print(astResult.pretty())
    else:
        with open("grammar_test") as test_file:
            content = test_file.read()
            # lexed = parser.lex(content)
            # result2 = parser.parse_interactive(content)
            # for i in lexed:
            #    print(repr(i))
            #    result2.feed_token(i)
            #    print(result2.accepts())

            result = parser.parse(content)
            print(result.pretty())
        # astResult = ToAST().transform(result)
        # print(astResult)
        # print(astResult.pretty())
