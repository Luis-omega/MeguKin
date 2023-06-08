from pathlib import Path
from argparse import ArgumentParser

from MeguKin.Parser.Parser import (
    load_grammar,
    parse,
    parse_string,
    FileLoadError,
    ParserError,
)
from MeguKin.Parser import Parser
from MeguKin.Error import MeguKinError
from MeguKin.File import FileInfo
from MeguKin.ModuleLoad.Load import solve_modules_path, load_modules
from MeguKin.ModuleLoad.Module import Module, ModuleLoadError
from MeguKin.SugaredSyntaxTree.Transform import ToSST
from MeguKin.Pretty import defaultSettings, pretty
from pprint import pprint


def compile_module(
    name: str, source_paths: list[Path]
) -> MeguKinError | Module:
    lark = load_grammar(False)
    if isinstance(lark, MeguKinError):
        print(lark)
        return lark
    # path_result = solve_modules_path([name], source_paths)
    # if isinstance(path_result, ModuleLoadError):
    #    print(ModuleLoadError)
    #    return ModuleLoadError
    module_result = load_modules([name], source_paths, lark)
    if isinstance(module_result, ModuleLoadError):
        return module_result
    loaded, modules = module_result
    module = modules[name]
    return module


def from_string(symbols: list[str], value: str) -> None:
    lark = load_grammar(False, symbols)
    if isinstance(lark, MeguKinError):
        print(lark)
        return
    parsed = parse_string(lark, FileInfo("cli_test", Path("cli")), value)
    if isinstance(parsed, Parser.ParserStageError):
        print("Error trying to parse it!")
        if isinstance(parsed, Parser.LarkParseError):
            print(str(parsed.exception))
            return
        elif isinstance(parsed, Parser.LayoutError):
            print(parsed.msg)
        else:
            print(parsed)
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
    print(pretty(defaultSettings, doc))


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
    parser.add_argument(
        "to_parse",
        nargs=1,
        type=str,
        metavar="str",
        help="The text to parse",
    )
    return parser.parse_args()


def main():
    args = generate_arg_parser()

    if args.symbol is not None:
        symbols = args.symbol
    else:
        symbols = ["top"]

    from_string(symbols, args.to_parse[0])
    return 0