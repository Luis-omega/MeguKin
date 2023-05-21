from pathlib import Path
from argparse import ArgumentParser

from MeguKin.Parser.Parser import load_grammar, parse, FileLoadError, ParserError
from MeguKin.Error import MeguKinError
from MeguKin.ModuleLoad.Load import solve_modules_path, load_modules
from MeguKin.ModuleLoad.Module import Module, ModuleLoadError


def compile_module(name: str, source_paths: list[Path]) -> MeguKinError | Module:
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


def generate_arg_parser():
    parser = ArgumentParser(
        prog="MeguKin",
        description="MeguKin python compiler and formatter",
        epilog="End of help, have a nice day =)",
    )
    parser.add_argument("use", choices=["compile", "format"])
    parser.add_argument("file", nargs="+", type=str, metavar="File")
    parser.add_argument(
        "-p", "--path", nargs=1, type=str, required=True, metavar="Path"
    )
    return parser.parse_args()


def main():
    args = generate_arg_parser()
    print(args)
    return 0


main()
