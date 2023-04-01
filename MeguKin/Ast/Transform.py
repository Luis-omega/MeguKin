from typing import List, Optional, Union, TypeVar, Tuple
from functools import reduce

from lark import Transformer, v_args, Token

from MeguKin.Reconstruction import mergeRanges, token2Range
from MeguKin.Ast.Types.Expression import (
    AnnotatedExpression,
    Variable,
    Literal,
    Application,
    Function,
    OperatorsWithoutMeaning,
    ExpressionT,
)
from MeguKin.Ast.Types.PatternMatch import (
    PatternMatchT,
    PatternMatchVariable,
    PatternMatchConstructor,
)
from MeguKin.Ast.Types.Top import Constructor, DataType, TopT, Definition, Declaration
from MeguKin.Ast.Types.Type import TypeT, TypeName, TypeArrow


T = TypeVar("T")


def is_lowercase_token(token: Token) -> bool:
    return token.type == "LOWERCASSE_IDENTIFIER"


def is_capplitalized_identifier(token: Token) -> bool:
    return token.type == "CAPITALIZED_IDENTIFIER"


@v_args(inline=True)
class ToAST(Transformer):
    # ------------------ Combinators ------------------
    # Typing this is tricky since we would need to call
    # isInstance over every item to ensure mypy that
    # the list consist of things of only one type
    # that would be waste of time!
    def sep_by1(self, *init):
        return [init[i] for i in range(0, len(init), 2)]

    def braces(self, start, value: T, end) -> T:
        return value

    def parens(self, start, value: T, end) -> T:
        return value

    # ------------------ Expressions ------------------

    def expression_parens(
        self, expression: ExpressionT, colon=None, annotation: Optional[TypeT] = None
    ) -> ExpressionT:
        if annotation is None:
            return expression
        else:
            return AnnotatedExpression(
                expression,
                annotation,
                mergeRanges(expression._range, annotation._range),
                expression.free_variables,
            )

    def expression_lambda(
        self, lambda_symbol, pattern: PatternMatchT, arrow, expression: ExpressionT
    ) -> Function:
        init_range = token2Range(lambda_symbol)
        free_variables = expression.free_variables - pattern.bound_variables
        return Function(
            pattern,
            expression,
            mergeRanges(init_range, expression._range),
            free_variables,
        )

    def expression_literal(self, tok: Token) -> Literal:
        return Literal(tok, token2Range(tok))

    def expression_atom(self, atom: Union[Token, ExpressionT]) -> ExpressionT:
        if isinstance(atom, Token):
            if is_lowercase_token(atom):
                return Variable(atom.value, False, token2Range(atom), set(atom.value))
            elif is_capplitalized_identifier(atom):
                return Variable(atom.value, True, token2Range(atom), set(atom.value))
            else:
                print(atom)
                raise Exception("if you see this, someone updated the grammar")
        else:
            return atom

    def expression_application(self, *values: ExpressionT) -> ExpressionT:
        # rule uses "+" so, this is guaranteed
        out = values[0]
        for value in values[1:]:
            free_variables = out.free_variables.union(value.free_variables)
            out = Application(
                out, value, mergeRanges(out._range, value._range), free_variables
            )
        return out

    # See note about the return type [str, Expression, str, Expression,...]
    # in OperatorsWithoutMeaning
    def expression(self, *allValues: Tuple[Union[Token, ExpressionT]]) -> ExpressionT:
        match allValues:
            case [value]:
                if isinstance(value, Token):
                    raise Exception("if you see this, lark parsers has a bug")
                else:
                    return value
            # Grammar guaranty that we always have a value
            case _:
                arg = list(allValues)
                free_variables = set.union(
                    *(
                        set(i.value) if isinstance(i, Token) else i.free_variables
                        for i in arg
                    )
                )
                return OperatorsWithoutMeaning(
                    arg,
                    mergeRanges(allValues[0]._range, allValues[-1]._range),
                    free_variables,
                )

    # ------------------ PatternMatch ------------------
    def pattern_match_constructor_args(
        self, *args: Union[Token, PatternMatchT]
    ) -> List[PatternMatchT]:
        out: List[PatternMatchT] = []
        for arg in args:
            if isinstance(arg, Token):
                if is_lowercase_token(arg):
                    out.append(
                        PatternMatchVariable(
                            arg.value, token2Range(arg), set(arg.value)
                        )
                    )
                else:
                    out.append(
                        PatternMatchConstructor(
                            arg.value, [], token2Range(arg), set(arg.value)
                        )
                    )
            else:
                out.append(arg)
        return out

    def pattern_match(
        self,
        firstArg: Union[Token, PatternMatchT],
        secondArg: Optional[List[PatternMatchT]] = None,
    ) -> PatternMatchT:
        if isinstance(firstArg, Token):
            if is_lowercase_token(firstArg):
                out = PatternMatchVariable(
                    firstArg.value, token2Range(firstArg), set(firstArg.value)
                )
                return out
            else:
                if secondArg is None:
                    return PatternMatchConstructor(
                        firstArg.value, [], token2Range(firstArg), set(firstArg.value)
                    )
                else:
                    acc = token2Range(firstArg)
                    # we can look for shadowed variables here but that would
                    # add complexity to code instead of simplifiying.
                    # like, we can't report the shadowing issue here,
                    # the we need to propagate and that is a lot.
                    # we can store the shadowing in the pattern, but
                    # that's not it's place.
                    bound_variables = set(firstArg.value)
                    for i in secondArg:
                        acc = mergeRanges(acc, i._range)
                        bound_variables = bound_variables.union(i.bound_variables)
                    return PatternMatchConstructor(
                        firstArg.value, secondArg, acc, bound_variables
                    )
        else:
            return firstArg

    # ------------------ Data ------------------

    def data_type_constructor(
        self, name: Token, types: Optional[List[TypeT]]
    ) -> Constructor:
        realTypes: List[TypeT]
        if types is None:
            realTypes = []
        else:
            realTypes = types
        acc = token2Range(name)
        for i in realTypes:
            acc = mergeRanges(acc, i._range)
        return Constructor(name.value, realTypes, acc)

    def data_type_constructors(self, sep: List[Constructor]) -> List[Constructor]:
        return sep

    # ------------------ Types ------------------
    def type_atom(self, value: Union[Token, TypeT]) -> TypeT:
        if isinstance(value, Token):
            return TypeName(value.value, token2Range(value))
        else:
            return value

    def type_expression(self, value: List[TypeT]) -> TypeT:
        value = value[::-1]
        firstValue = value[0]
        if len(value) == 1:
            return firstValue
        else:
            out = firstValue
            for domaint in value[1:]:
                _range = mergeRanges(out._range, domaint._range)
                out = TypeArrow(domaint, out, _range)
            return out

    # ------------------ Top ------------------
    def top_variable_declaration(self, name: Token, colon, _type: TypeT) -> Declaration:
        return Declaration(
            name.value, _type, mergeRanges(token2Range(name), _type._range)
        )

    def top_variable_definition(
        self, name: Token, colon, expression: ExpressionT
    ) -> Definition:
        return Definition(
            name.value, expression, mergeRanges(token2Range(name), expression._range)
        )

    def top_data_type(
        self, data: Token, typeName: Token, eq, constructors: List[Constructor]
    ) -> DataType:
        # well, tecnically the last token is the one with the biggest range,
        # so, a mergeRanges between `data` and `constructors[-1]` must be enough.
        _range = mergeRanges(
            token2Range(data),
            reduce(mergeRanges, [i._range for i in constructors]),
        )
        return DataType(typeName.value, constructors, _range)

    def top(self, value: TopT) -> TopT:
        return value
