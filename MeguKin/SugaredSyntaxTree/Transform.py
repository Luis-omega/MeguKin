from typing import Optional, TypeVar
from functools import reduce
import logging

from lark import Transformer, v_args, Tree

from MeguKin.File import mergeRanges, token2Range, Range
from MeguKin.Parser.Token import Token
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
    CaseCase,
    Function,
    AnnotatedExpression,
    ExpressionT,
    CaseCase,
    Case,
    LetBinding,
    ExpressionMeaninglessOperatorApplications,
    Let,
)
from MeguKin.SugaredSyntaxTree.PatternMatch import (
    PatternMatchT,
    PatternMatchVariable,
    PatternMatchConstructor,
    PatternMatchLiteral,
    PatternMatchConstructorName,
)
from MeguKin.SugaredSyntaxTree.Top import (
    TopT,
    Definition,
    Declaration,
    DataType,
    ConstructorDefinition,
)
from MeguKin.SugaredSyntaxTree.Type import (
    TypeT,
    TypeVariable,
    TypeOperator,
    TypeRecord,
    TypeMeaninglessOperatorApplications,
    TypeApplication,
    TypeConcreteName,
    TypeArrow,
    TypeForall,
)

from MeguKin.SugaredSyntaxTree.SST import (
    IntercalatedList,
    IntercalatedListFist,
    IntercalatedListSecond,
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
class ToSST(Transformer):
    # ------------------ Combinators ------------------
    # Typing this is tricky since we would need to call
    # isInstance over every item to ensure mypy that
    # the list consist of things of only one type.
    # That is such a waste of time!
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

    def expression_record_item(
        self, variable: Token, colon: Token, expression: ExpressionT
    ) -> tuple[str, Range, ExpressionT]:
        return (variable.value, token2Range(variable), expression)

    def expression_record_item_layout(
        self,
        variable: Token,
        colon: Token,
        layout_start: Token,
        expression: ExpressionT,
        layout_end: Token,
    ) -> tuple[str, Range, ExpressionT]:
        return (variable.value, token2Range(variable), expression)

    def expression_record_item_single(
        self, variable: Token
    ) -> tuple[str, Range, None]:
        return (variable.value, token2Range(variable), None)

    def expression_record_inner(
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

    def expression_annotation(self, expression: ExpressionT) -> ExpressionT:
        return expression

    def expression_annotation_no_layout(
        self,
        expression: ExpressionT,
        colon: Token,
        type_expression: TypeT,
    ) -> AnnotatedExpression:
        return AnnotatedExpression(expression, type_expression)

    def expression_annotation_layout(
        self,
        expression: ExpressionT,
        colon: Token,
        layout_start: Token,
        type_expression: TypeT,
        layout_end: Token,
    ) -> AnnotatedExpression:
        return AnnotatedExpression(expression, type_expression)

    def type_arg(self, at, _type: TypeT) -> ExpressionTypeArgument:
        return ExpressionTypeArgument(
            # We want the full @Type to be signaled on error
            _type,
            mergeRanges(token2Range(at), _type._range),
        )

    def expression_literal(self, tok: Token) -> Literal:
        return Literal(tok)

    def expression_atom(self, atom: ExpressionT) -> ExpressionT:
        return atom

    def expression_selector(
        self, atom: ExpressionT, *remain: Token
    ) -> ExpressionT:
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
            out = Application(
                out,
                value,
                mergeRanges(out._range, value._range),
            )
        return out

    def expression_operators(
        self, *allValues: ExpressionT | Operator
    ) -> ExpressionT:
        match allValues:
            case [value]:
                return value
            # Grammar guarantee that we always have a value
            case _:
                firstValue = allValues[0]
                acc1: IntercalatedList[
                    ExpressionT, Operator
                ] = IntercalatedListFist(firstValue)
                acc2: IntercalatedList[Operator, ExpressionT]
                is_operator = True
                for value in allValues[1:]:
                    if is_operator:
                        acc2 = IntercalatedListSecond(
                            value, acc1  # type:ignore
                        )
                        is_operator = False
                    else:
                        acc1 = IntercalatedListSecond(value, acc2)
                        is_operator = True

                return ExpressionMeaninglessOperatorApplications(
                    acc1,
                    mergeRanges(allValues[0]._range, allValues[-1]._range),
                )

    def expression_case_single(
        self, pattern: PatternMatchT, arrow: Token, expression: ExpressionT
    ) -> tuple[PatternMatchT, ExpressionT]:
        return (pattern, expression)

    def expression_case_single_layout(
        self,
        pattern: PatternMatchT,
        arrow: Token,
        layout_start: Token,
        expression: ExpressionT,
        layout_end: Token,
    ) -> tuple[PatternMatchT, ExpressionT]:
        return (pattern, expression)

    def expression_case_cases(
        self, cases: list[tuple[PatternMatchT, ExpressionT]]
    ) -> list[CaseCase]:
        print("case cases got: ", cases)
        return [CaseCase(case[0], case[1]) for case in cases]

    def expression_case_operators(self, expression: ExpressionT) -> ExpressionT:
        return expression

    def expression_case(
        self,
        case: Token,
        expression: ExpressionT,
        of: Token,
        cases: list[CaseCase],
    ) -> Case:
        first_range = token2Range(case)
        last_range = cases[-1]._range
        _range = mergeRanges(first_range, last_range)
        return Case(expression, cases, _range)

    def expression_case_2(
        self,
        case: Token,
        layout_start: Token,
        expression: ExpressionT,
        layout_end: Token,
        of: Token,
        cases: list[CaseCase],
    ) -> Case:
        first_range = token2Range(case)
        last_range = cases[-1]._range
        _range = mergeRanges(first_range, last_range)
        return Case(expression, cases, _range)

    def expression_case_3(
        self,
        case: Token,
        expression: ExpressionT,
        of: Token,
        layout_start: Token,
        cases: list[CaseCase],
        layout_end: Token,
    ) -> Case:
        first_range = token2Range(case)
        last_range = cases[-1]._range
        _range = mergeRanges(first_range, last_range)
        return Case(expression, cases, _range)

    def expression_case_4(
        self,
        case: Token,
        layout_start: Token,
        expression: ExpressionT,
        layout_end: Token,
        of: Token,
        layout_start_2: Token,
        cases: list[CaseCase],
        layout_end_2: Token,
    ) -> Case:
        first_range = token2Range(case)
        last_range = cases[-1]._range
        _range = mergeRanges(first_range, last_range)
        return Case(expression, cases, _range)

    def expression_lambda_arguments(self, *patterns: PatternMatchT):
        return list(patterns)

    def expression_lambda_case(self, expression: ExpressionT) -> ExpressionT:
        return expression

    def expression_lambda(
        self,
        _lambda: Token,
        patterns: list[PatternMatchT],
        arrow: Token,
        expression: ExpressionT,
    ) -> Function:
        init_range = token2Range(_lambda)
        return Function(
            patterns,
            expression,
            mergeRanges(init_range, expression._range),
        )

    def expression_lambda_2(
        self,
        _lambda: Token,
        layout_start: Token,
        patterns: list[PatternMatchT],
        layout_end: Token,
        arrow: Token,
        expression: ExpressionT,
    ) -> Function:
        init_range = token2Range(_lambda)
        return Function(
            patterns,
            expression,
            mergeRanges(init_range, expression._range),
        )

    def expression_lambda_3(
        self,
        _lambda: Token,
        patterns: list[PatternMatchT],
        arrow: Token,
        layout_start: Token,
        expression: ExpressionT,
        layout_end: Token,
    ) -> Function:
        init_range = token2Range(_lambda)
        return Function(
            patterns,
            expression,
            mergeRanges(init_range, expression._range),
        )

    def expression_lambda_4(
        self,
        _lambda: Token,
        layout_start: Token,
        patterns: list[PatternMatchT],
        layout_end: Token,
        arrow: Token,
        layout_start_2: Token,
        expression: ExpressionT,
        layout_end_2: Token,
    ) -> Function:
        init_range = token2Range(_lambda)
        return Function(
            patterns,
            expression,
            mergeRanges(init_range, expression._range),
        )

    def expression_let_binding(
        self, pattern: PatternMatchT, equal: Token, expression: ExpressionT
    ) -> LetBinding:
        return LetBinding(
            pattern,
            expression,
        )

    def expression_let_inside(
        self, bindings: list[LetBinding]
    ) -> list[LetBinding]:
        return bindings

    def expression_let(
        self,
        let,
        layout_start: Token,
        bindings: list[LetBinding],
        layout_end: Token,
        _in: Token,
        layout_start_2: Token,
        expression: ExpressionT,
        layout_end_2: Token,
    ) -> Let:
        return Let(bindings, expression)

    def expression_let_2(
        self,
        let,
        layout_start: Token,
        bindings: list[LetBinding],
        layout_end: Token,
        _in: Token,
        expression: ExpressionT,
    ) -> Let:
        return Let(bindings, expression)

    def expression_let_3(
        self,
        let,
        bindings: list[LetBinding],
        _in: Token,
        layout_start: Token,
        expression: ExpressionT,
        layout_end: Token,
    ) -> Let:
        return Let(bindings, expression)

    def expression_let_4(
        self,
        let,
        bindings: list[LetBinding],
        _in: Token,
        expression: ExpressionT,
    ) -> Let:
        return Let(bindings, expression)

    def expression_let_lambda(self, expression: ExpressionT) -> ExpressionT:
        return expression

    def expression(self, expression_1: ExpressionT) -> ExpressionT:
        return expression_1

    # TODO: I didn't check PatternMatch as it was woking after refactors
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

    def data_type_constructor(
        self, name: Token, types: Optional[list[TypeT]]
    ) -> ConstructorDefinition:
        realTypes: list[TypeT]
        if types is None:
            realTypes = []
        else:
            realTypes = types
        acc = token2Range(name)
        for i in realTypes:
            acc = mergeRanges(acc, i._range)
        return ConstructorDefinition(name.value, realTypes, acc)

    def data_type_constructors(
        self, sep: list[ConstructorDefinition]
    ) -> list[ConstructorDefinition]:
        return sep

    def data_type_constructors_layout(
        self, constructors: list[ConstructorDefinition]
    ) -> list[ConstructorDefinition]:
        return constructors

    # ------------------ Types ------------------

    def type_record_item(
        self, variable: Token, colon: Token, type_expression_inner: TypeT
    ) -> tuple[Token, TypeT]:
        return (variable, type_expression_inner)

    def type_record_item_layout(
        self,
        variable: Token,
        colon: Token,
        layout_start: Token,
        type_expression_inner: TypeT,
        layout_end: Token,
    ) -> tuple[Token, TypeT]:
        return (variable, type_expression_inner)

    def type_record_inner(
        items: list[tuple[Token, TypeT]]
    ) -> list[tuple[Token, TypeT]]:
        return items

    def type_record(self, items: list[tuple[Token, TypeT]]) -> TypeRecord:
        return TypeRecord(
            [(token.value, token2Range(token), type_) for token, type_ in items]
        )

    def type_operator(self, operator: Token) -> TypeOperator:
        return TypeOperator.from_lark_token(operator)

    def type_variable(self, variable: Token) -> TypeVariable:
        return TypeVariable.from_lark_token(variable)

    def type_concrete_type(self, token: Token) -> TypeConcreteName:
        return TypeConcreteName.from_lark_token(token)

    def type_atom(self, value: TypeT) -> TypeT:
        return value

    # mypy can't find that this is a exahustive pattern match
    def type_application(self, *atoms: TypeT) -> TypeT:  # type:ignore
        match atoms:
            case []:
                # grammar waranties that this won't happen!
                raise Exception(missing_case_exception_message)
            case [atom1]:
                return atom1
            case [atom1, atom2, *remain]:
                acc = TypeApplication(
                    atom1, atom2, mergeRanges(atom1._range, atom2._range)
                )
                for newItem in remain:
                    acc = TypeApplication(
                        acc, newItem, mergeRanges(acc._range, newItem._range)
                    )
                return acc

    def type_operators(self, *allValues: TypeT | TypeOperator) -> TypeT:
        match allValues:
            case [value]:
                return value
            # Grammar guarantee that we always have a value
            case _:
                firstValue: TypeT = allValues[0]  # type:ignore
                acc1: IntercalatedList[
                    TypeT, TypeOperator
                ] = IntercalatedListFist(firstValue)
                acc2: IntercalatedList[Operator, TypeT]
                is_operator = True
                for value in allValues[1:]:
                    if is_operator:
                        acc2 = IntercalatedListSecond(
                            value, acc1  # type:ignore
                        )
                        is_operator = False
                    else:
                        acc1 = IntercalatedListSecond(
                            value, acc2  # type:ignore
                        )
                        is_operator = True

                return ExpressionMeaninglessOperatorApplications(
                    acc1,  # type:ignore
                    mergeRanges(allValues[0]._range, allValues[-1]._range),
                )

    def type_expression_inner(self, types: list[TypeT]) -> TypeT:
        out = types[-1]
        for type_ in types[1:][::-1]:
            out = TypeArrow(type_, out, mergeRanges(out._range, type_._range))
        return out

    def type_scheme(self, type_expression: TypeT) -> TypeT:
        return type_expression

    def type_scheme_forall_no_layout(
        self,
        forall: Token,
        args: list[TypeVariable],
        dot: Token,
        expression: TypeT,
    ) -> TypeT:
        return TypeForall(
            args,
            expression,
            mergeRanges(token2Range(forall), expression._range),
        )

    def type_scheme_forall_layout(
        self,
        forall: Token,
        layout_start: Token,
        args: list[TypeVariable],
        layout_end: Token,
        dot: Token,
        expression: TypeT,
    ) -> TypeT:
        return TypeForall(
            args,
            expression,
            mergeRanges(token2Range(forall), expression._range),
        )

    def type_data_type_args(
        self, *variables: TypeVariable
    ) -> list[TypeVariable]:
        return list(variables)

    def type_data_type_args_layout(
        self, *variables: TypeVariable | Token
    ) -> list[TypeVariable]:
        return [
            maybe_var
            for maybe_var in variables
            if isinstance(maybe_var, TypeVariable)
        ]

    ## ------------------ Top ------------------

    def top_variable_declaration(
        self, name: Token, colon: Token, _type: TypeT
    ) -> Declaration:
        return Declaration(
            name.value, _type, mergeRanges(token2Range(name), _type._range)
        )

    def top_variable_declaration_layout(
        self,
        name: Token,
        colon: Token,
        layout_start: Token,
        _type: TypeT,
        layout_end: Token,
    ) -> Declaration:
        print(_type)
        return Declaration(
            name.value, _type, mergeRanges(token2Range(name), _type._range)
        )

    def top_variable_definition(
        self,
        name: Token,
        pattern: Optional[list[PatternMatchT]],
        equal: Token,
        expression: ExpressionT,
    ) -> Definition:
        _range = mergeRanges(token2Range(name), expression._range)
        if pattern is None:
            return Definition(name.value, expression, _range)
        return Definition(
            name.value, Function(pattern, expression, _range), _range
        )

    def top_variable_definition_layout(
        self,
        name: Token,
        pattern: Optional[list[PatternMatchT]],
        equal: Token,
        layout_start: Token,
        expression: ExpressionT,
        layout_end: Token,
    ) -> Definition:
        _range = mergeRanges(token2Range(name), expression._range)
        if pattern is None:
            return Definition(name.value, expression, _range)
        return Definition(
            name.value, Function(pattern, expression, _range), _range
        )

    # def top_data_type(
    #    self, data: Token, typeName: Token, eq, constructors: list[Constructor]
    # ) -> DataType:
    #    # well, tecnically the last token is the one with the biggest range,
    #    # so, a mergeRanges between `data` and `constructors[-1]` must be enough.
    #    _range = mergeRanges(
    #        token2Range(data),
    #        reduce(mergeRanges, [i._range for i in constructors]),
    #    )
    #    return DataType(typeName.value, constructors, _range)

    # def top(self, value: TopT) -> TopT:
    #    return value


def tree2sugared(trees: list[Tree]) -> list[TopT]:
    toSST = ToSST()
    return [toSST.transform(tree) for tree in trees]
