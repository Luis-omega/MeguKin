from lark import Lark

from MeguKin.Parser.Parser import parse_string
from MeguKin.Parser import Parser
from MeguKin.Parser.Parser import load_grammar
from MeguKin.SugaredSyntaxTree.Transform import ToSST

import logging

log = logging.getLogger(__name__)
handler = logging.FileHandler("log")
log.addHandler(handler)
log.setLevel(logging.DEBUG)

log.debug("hi check")


def process(lark: Lark, to_parse: str):
    lexed = lark.lex(to_parse)
    print(list(lexed))
    result = parse_string(lark, None, to_parse)

    if isinstance(result, Parser.ParserStageError):
        if isinstance(result, Parser.LarkParseError):
            print(str(result.exception))
            return result.exception
        else:
            print(result)
    else:
        print(result.pretty())
        tranformed = ToSST().transform(result)
        print(tranformed)
    return result


def initialize():
    lark = load_grammar()
    import sys

    if len(sys.argv) == 2:
        to_parse = sys.argv[1]
    else:
        with open("grammar_test", "r") as file:
            to_parse = file.read()
    return (lark, to_parse)


def run_all():
    lark, to_parse = initialize()
    return process(lark, to_parse)


if "__main__" == __name__:
    run_all()
