from pathlib import Path
from argparse import ArgumentParser

from MeguKin.Loggers import get_logger
from MeguKin.Parser.Parser import (
    load_grammar,
    parse_string,
)
from MeguKin.Parser import Parser
from MeguKin.Error import MeguKinError
from MeguKin.File import FileInfo
from MeguKin.SugaredSyntaxTree.Transform import ToSST
from MeguKin.Pretty import defaultSettings, pretty_as_console
from pprint import pprint

log = get_logger(__name__)


def from_file(symbols: list[str], path: Path) -> None:
    with open(path, "r") as f:
        content = f.read()
    from_string(symbols, content)


def from_string(symbols: list[str], value: str) -> None:
    lark = load_grammar(False, symbols)
    if isinstance(lark, MeguKinError):
        print(lark)
        return
    parsed = parse_string(lark, FileInfo("cli_test", Path("cli")), value)
    if isinstance(parsed, Parser.ParserStageError):
        print("Error trying to parse it!")
        match parsed:
            case Parser.ParserError():
                p = pretty_as_console(parsed.document)
                print(p)
                return
            case Parser.FileLoadError():
                print(f"Can't load file: {parsed.info}")
                return
            case Parser.LoadGrammarError():
                print("Can't load grammar file")
                return
            case Parser.LarkLoadError():
                print("Can't process grammar file!\n", parsed.msg)
                return
        return
    print(40 * "-", "Lark Tree", 40 * "-", "\n")
    print(parsed.pretty())
    tranformed = ToSST().transform(parsed)
    print(40 * "-", "Transformed pprint", 40 * "-", "\n")
    pprint(tranformed)
    print(40 * "-", "Transformed doc", 40 * "-", "\n")
    doc = tranformed.to_document(defaultSettings)
    pprint(doc)
    print(40 * "-", "Transformed pretty", 40 * "-", "\n")
    print(pretty_as_console(doc))


def generate_arg_parser():
    parser = ArgumentParser(
        prog="MeguKin",
        description="MeguKin python fast test tool",
        epilog="End of help, have a nice day =)",
    )
    parser.add_argument(
        "-s",
        "--symbol",
        nargs=1,
        type=str,
        required=False,
        metavar="Lark_rule",
        help="The rule to apply",
    )
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "-i",
        "--inline",
        nargs=1,
        type=str,
        metavar="str",
        help="The text to parse",
    )
    group.add_argument(
        "-f",
        "--file",
        nargs=1,
        type=str,
        metavar="FILE",
        help="A file to parse",
    )
    return parser.parse_args()


def main():
    args = generate_arg_parser()
    print(args)

    if args.symbol is not None:
        symbols = args.symbol
    else:
        symbols = ["top"]

    if args.inline is not None:
        from_string(symbols, args.inline[0])
    else:
        from_file(symbols, args.file[0])

    return 0
