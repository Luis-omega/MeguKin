from typing import Optional, TypeVar
from functools import reduce

from lark import Transformer, v_args, Token, Tree

from MeguKin.File import mergeRanges, token2Range, Range
from MeguKin.SugaredSyntaxTree.Expression import (
    Literal,
    Variable,
    Operator,
    ConstructorName,
    RecordUpdate,
    Record,
    Selector,
    ExpressionTypeArgument,
    Application,
    Function,
    OperatorsWithoutMeaning,
    AnnotatedExpression,
    ExpressionT,
    CaseCase,
    Case,
    LetBinding,
    Let,
    IntercalatedList,
    IntercalatedListFist,
    IntercalatedListSecond,
)
from MeguKin.SugaredSyntaxTree.PatternMatch import (
    PatternMatchT,
    PatternMatchVariable,
    PatternMatchConstructor,
    PatternMatchLiteral,
    PatternMatchConstructorName,
)
from MeguKin.SugaredSyntaxTree.Top import TopT, Definition, Declaration
from MeguKin.SugaredSyntaxTree.Type import (
    TypeT,
    TypeVariable,
    TypeConcreteName,
    TypeArrow,
)


T = TypeVar("T")


def is_lowercase_token(token: Token) -> bool:
    return token.type == "LOWERCASSE_IDENTIFIER"


def is_capplitalized_identifier(token: Token) -> bool:
    return token.type == "CAPITALIZED_IDENTIFIER"


class ToASTException(Exception):
    pass


missing_case_exception_message = (
    "If you see this something was modified in the grammar of the language."
)


@v_args(inline=True)
class ToAST(Transformer):
    # ------------------ Combinators ------------------
    # Typing this is tricky since we would need to call
    # isInstance over every item to ensure mypy that
    # the list consist of things of only one type
    # that would such a waste of time!
    def sep_by1(self, *init):
        return [init[i] for i in range(0, len(init), 2)]

    def braces(self, start, value: T, end) -> T:
        return value

    def parens(self, start, value: T, end) -> T:
        return value

    def brackets(self, start, value: T, end) -> T:
        return value

    # ------------------ Expressions ------------------
    def expression_record_update_item(
        self, variable: Token, equal: Token, expression: ExpressionT
    ) -> tuple[str, Range, ExpressionT]:
        return (variable.value, token2Range(variable), expression)

    def exppression_record_update_inner(
        self, results: list[tuple[str, Range, ExpressionT]]
    ) -> list[tuple[str, Range, ExpressionT]]:
        return results

    def expression_record_update(
        self, items: list[tuple[str, Range, ExpressionT]]
    ) -> RecordUpdate:
        return RecordUpdate(items)

    def expression_record_item_single(self, variable: Token) -> tuple[str, Range, None]:
        return (variable.value, token2Range(variable), None)

    def expression_record_item(
        self, variable: Token, equal: Token, expression: ExpressionT
    ) -> tuple[str, Range, ExpressionT]:
        return (variable.value, token2Range(variable), expression)

    def exppression_record_inner(
        self, results: list[tuple[str, Range, Optional[ExpressionT]]]
    ) -> list[tuple[str, Range, Optional[ExpressionT]]]:
        return results

    def expression_record(
        self, items: list[tuple[str, Range, Optional[ExpressionT]]]
    ) -> Record:
        return Record(items)

    def expression_operator(self, operator: Token) -> Operator:
        match operator.type:
            case "PREFIXED_OPERATOR":
                return Operator.from_lark_token(operator)
            case "OPERATOR":
                return Operator.from_lark_token(operator)
            case _:
                raise ToASTException(missing_case_exception_message)

    def expression_operator_identifier(self, operator: Token) -> Variable:
        match operator.type:
            case "INFIX_IDENTIFIER":
                return Variable.from_lark_token(operator)
            case _:
                raise ToASTException(missing_case_exception_message)

    def expression_constructor(self, identifier: Token) -> ConstructorName:
        match identifier.type:
            case "CAPITALIZED_IDENTIFIER":
                return ConstructorName.from_lark_token(identifier)
            case "PREFIXED_CAPITALIZED":
                return ConstructorName.from_lark_token(identifier)
            case _:
                raise ToASTException(missing_case_exception_message)

    def expression_variable(self, identifier: Token) -> Variable:
        match identifier.type:
            case "VARIABLE_IDENTIFIER":
                return Variable.from_lark_token(identifier)
            case "PREFIXED_VARIABLE":
                return Variable.from_lark_token(identifier)
            case _:
                raise ToASTException(missing_case_exception_message)

    def expression_annotation(
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

    def type_arg(self, at, _type: TypeT) -> ExpressionTypeArgument:
        return ExpressionTypeArgument(_type, _type._range)

    def expression_literal(self, tok: Token) -> Literal:
        return Literal(tok, token2Range(tok))

    def expression_atom(self, atom: ExpressionT) -> ExpressionT:
        return atom

    def expression_selector(self, atom: ExpressionT, *remain: Token) -> ExpressionT:
        if len(remain) == 0:
            return atom
        else:
            _range = mergeRanges(atom._range, token2Range(remain[-1]))
            fields = [i.value for i in remain[::2]]
            return Selector(atom, fields, _range)

    def expression_application(self, *values: ExpressionT) -> ExpressionT:
        # rule uses "+" so, this is guaranteed
        out = values[0]
        for value in values[1:]:
            free_variables = out.free_variables.union(value.free_variables)
            out = Application(
                out, value, mergeRanges(out._range, value._range), free_variables
            )
        return out

    def expression_operators(self, *allValues: ExpressionT | Operator) -> ExpressionT:
        match allValues:
            case [value]:
                return value
            # Grammar guaranty that we always have a value
            case _:
                firstValue = allValues[0]
                acc1: IntercalatedList[ExpressionT, Operator] = IntercalatedListFist(
                    firstValue
                )
                acc2: IntercalatedList[Operator, ExpressionT]
                is_operator = True
                for value in allValues[1:]:
                    if is_operator:
                        acc2 = IntercalatedListSecond(value, acc1)  # type: ignore
                        is_operator = False
                    else:
                        acc1 = IntercalatedListSecond(value, acc2)
                        is_operator = True

                free_variables = set.union(*(i.free_variables for i in allValues))
                return OperatorsWithoutMeaning(
                    acc1,
                    mergeRanges(allValues[0]._range, allValues[-1]._range),
                    free_variables,
                )

    def expression_case_single(
        self, pattern: PatternMatchT, arrow: Token, expression: ExpressionT
    ) -> tuple[PatternMatchT, ExpressionT]:
        return (pattern, expression)

    def expression_case_cases(self, *cases: CaseCase) -> list[CaseCase]:
        return list(cases)

    def expression_case_operators(self, expression: ExpressionT) -> ExpressionT:
        return expression

    def expression_case(
        self,
        case: Token,
        expression: ExpressionT,
        of: Token,
        cases: list[CaseCase],
        of_end: Token,
    ) -> Case:
        first_range = token2Range(case)
        last_range = cases[-1]._range
        _range = mergeRanges(first_range, last_range)
        return Case(expression, cases, _range)

    def expression_lambda_case(self, expression: ExpressionT) -> ExpressionT:
        return expression

    def expression_lambda_arguments(self, *patterns: PatternMatchT):
        return list(patterns)

    def expression_lambda(
        self,
        _lambda: Token,
        patterns: list[PatternMatchT],
        arrow,
        expression: ExpressionT,
    ) -> Function:
        init_range = token2Range(_lambda)
        return Function(
            patterns,
            expression,
            mergeRanges(init_range, expression._range),
        )

    def expression_let_binding(
        self, name: Token, eq, expression: ExpressionT, separator: Token
    ) -> LetBinding:
        return LetBinding(
            name.value,
            expression,
        )

    def expression_let_inside(self, *bindings: LetBinding) -> list[LetBinding]:
        return list(bindings)

    def expression_let_lambda(self, expression: ExpressionT) -> ExpressionT:
        return expression

    def expression_let(
        self,
        let,
        bindings: list[LetBinding],
        _in: Token,
        expression: ExpressionT,
        _in_end: Token,
    ) -> Let:
        return Let(bindings, expression)

    def expression(self, alternative: Let | Function) -> ExpressionT:
        return alternative

    # ------------------ PatternMatch ------------------
    def pattern_match_constructor_identifier(
        self, token: Token
    ) -> PatternMatchConstructorName:
        return PatternMatchConstructorName.from_lark_token(token)

    def pattern_match_variable(self, token: Token) -> PatternMatchVariable:
        return PatternMatchVariable(token)

    def pattern_match_literal(self, token: Token) -> PatternMatchLiteral:
        return PatternMatchLiteral(token)

    def pattern_match_atom(self, value: PatternMatchT) -> PatternMatchT:
        return value

    def pattern_match_constructor_application(
        self, maybe_constructor: PatternMatchT, *arguments: PatternMatchT
    ) -> PatternMatchT:
        arguments_list = list(arguments)
        if len(arguments_list) == 0:
            return maybe_constructor
        else:
            # We can ignore error here thanks to the
            # grammar rule and the list len check
            return PatternMatchConstructor(maybe_constructor, arguments)  # type: ignore

    def pattern_match(self, pattern: PatternMatchT) -> PatternMatchT:
        return pattern

    def pattern_match_function_args_atoms(
        self, *atoms: PatternMatchT
    ) -> list[PatternMatchT]:
        return list(atoms)

    def pattern_match_function_args_comes(
        self, *mixedList: PatternMatchT | Token
    ) -> list[PatternMatchT]:
        out: list[PatternMatchT] = []
        for maybe_pattern in mixedList:
            if not isinstance(maybe_pattern, Token):
                out.append(maybe_pattern)
        return out

    def pattern_match_function_args(
        self, twoCases: list[PatternMatchT]
    ) -> list[PatternMatchT]:
        return twoCases

    # ------------------ Data ------------------

    # CONTINUE HERE __________________________________________________________________________

    def data_type_constructor(self, name: Token, *types: TypeT) -> Constructor:
        realTypes: list[TypeT]
        if types is None:
            realTypes = []
        else:
            realTypes = types
        acc = token2Range(name)
        for i in realTypes:
            acc = mergeRanges(acc, i._range)
        return Constructor(name.value, realTypes, acc)

    def data_type_constructors(self, sep: list[Constructor]) -> list[Constructor]:
        return sep

    # ------------------ Types ------------------
    def type_atom(self, value: Token | TypeT) -> TypeT:
        if isinstance(value, Token):
            return TypeName(value.value, token2Range(value))
        else:
            return value

    def type_expression(self, value: list[TypeT]) -> TypeT:
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
        self, data: Token, typeName: Token, eq, constructors: list[Constructor]
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


def tree2sugared(trees: list[Tree]) -> list[TopT]:
    pass
