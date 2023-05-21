from pathlib import Path

from MeguKin.Parser.Parser import load_grammar, parse, FileLoadError, ParseError
from MeguKin.Error import MeguKinError


def compile_module(path: Path, module_handler: Handler, debug=bool) -> Module:
    lark = load_grammar(debug)
    if isinstance(lark, MeguKinError):
        print(lark)
        return -1
    parse_result = parse(path, lark, debug)
    if isinstance(parse_result, FileLoadError):
        print(parse_result)
        return -1
    file_info, trees = parse_result
    sugared_trees = tree2sugared(trees, debug)
    desugared_trees = desugar(trees, debug)
    semantic_analysis_result = semantic_analysis(desugared_trees, debug)


def main():
    pass
