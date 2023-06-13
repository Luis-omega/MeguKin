from typing import Optional, TypeVar
from functools import reduce
import logging
import re

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
    PrefixOperator,
)
from MeguKin.SugaredSyntaxTree.PatternMatch import (
    PatternMatchT,
    PatternMatchVariable,
    PatternMatchConstructor,
    PatternMatchLiteral,
    PatternMatchConstructorName,
    PatternMatchHole,
)
from MeguKin.SugaredSyntaxTree.Top import (
    TopT,
    Definition,
    Declaration,
    DataType,
    ConstructorDefinition,
    Exports,
    Module,
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
    TypeRecordField,
)


from MeguKin.SugaredSyntaxTree.Module import (
    ImportConstructorName,
    ImportTypeName,
    ImportType,
    ImportFunction,
    ImportOperatorExpression,
    ImportOperatorType,
    ImportModuleName,
    ImportModule,
    ExportConstructorName,
    ExportTypeName,
    ExportType,
    ExportFunction,
    ExportOperatorExpression,
    ExportOperatorType,
    ExportModuleName,
    ImportNameT,
    ExportNameT,
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
    ) -> tuple[Variable, Range, ExpressionT]:
        return (
            Variable.from_lark_token(variable),
            mergeRanges(token2Range(variable), expression._range),
            expression,
        )

    def expression_record_update_inner(
        self, results: list[tuple[Variable, Range, ExpressionT]]
    ) -> list[tuple[Variable, Range, ExpressionT]]:
        return results

    def expression_record_update(
        self, items: list[tuple[Variable, Range, ExpressionT]]
    ) -> RecordUpdate:
        return RecordUpdate(items)

    def expression_record_item(
        self, variable: Token, colon: Token, expression: ExpressionT
    ) -> tuple[Variable, Range, ExpressionT]:
        return (
            Variable.from_lark_token(variable),
            mergeRanges(token2Range(variable), expression._range),
            expression,
        )

    def expression_record_item_layout(
        self,
        variable: Token,
        colon: Token,
        layout_start: Token,
        expression: ExpressionT,
        layout_end: Token,
    ) -> tuple[Variable, Range, ExpressionT]:
        return (
            Variable.from_lark_token(variable),
            mergeRanges(token2Range(variable), expression._range),
            expression,
        )

    def expression_record_item_single(
        self, variable: Token
    ) -> tuple[Variable, Range, None]:
        return (Variable.from_lark_token(variable), token2Range(variable), None)

    def expression_record_inner(
        self, results: list[tuple[Variable, Range, Optional[ExpressionT]]]
    ) -> list[tuple[Variable, Range, Optional[ExpressionT]]]:
        return results

    def expression_record(
        self, items: list[tuple[Variable, Range, Optional[ExpressionT]]]
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

    def expression_operator_parens(self, operator: Operator) -> PrefixOperator:
        return PrefixOperator(operator.prefix, operator.name, operator._range)

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

    def expression_literal(self, tok: Token) -> Literal:
        return Literal(tok)

    def expression_atom(self, atom: ExpressionT) -> ExpressionT:
        return atom

    def expression_type_arg_parens(
        self, at_rparen: Token, type_expression: TypeT, lparen: Token
    ) -> ExpressionTypeArgument:
        return ExpressionTypeArgument(
            type_expression,
            mergeRanges(token2Range(at_rparen), type_expression._range),
        )

    def expression_type_arg_prefixed(
        self, token: Token
    ) -> ExpressionTypeArgument:
        full = token.value[1:]
        list_of = full.split(".")
        name = list_of[-1]
        prefix = list_of[:-1]
        type_ = TypeVariable(prefix, name, token2Range(token))
        return ExpressionTypeArgument(type_, type_._range)

    def expression_type_arg_alone(self, token: Token):
        name = token.value[1:]
        type_ = TypeVariable([], name, token2Range(token))
        return ExpressionTypeArgument(type_, type_._range)

    def expression_type_arg(
        self, exp: ExpressionTypeArgument
    ) -> ExpressionTypeArgument:
        return exp

    def expression_selector_parens(
        self, lparen: Token, expression: ExpressionT, selector_parens: Token
    ) -> ExpressionT:
        # dicard the ")." token
        fields = selector_parens.value.split(".", 1)[1]
        for selector_field, selector_range in selectors_split(
            fields, expression._range
        ):
            expression = Selector(
                expression,
                selector_field,
                mergeRanges(expression._range, selector_range),
            )
        return expression

    def expression_selector_prefixed(self, token: Token) -> ExpressionT:
        prefixed: list[str] = []
        prefix_re = r"([A-Z]([a-zA-Z0-9])*)(\.[A-Z][a-zA-Z0-9]*)*"
        find = re.search(prefix_re, token.value)
        group = find.group()  # type:ignore
        prefixed = group.split(".")
        prefix_len = len(group)
        expression_range = Range(
            token.line,
            token.line,
            token.column,
            token.column + prefix_len,
            token.start_pos,
            token.start_pos + prefix_len,
        )
        remain = token.value[len(group) + 1 :]
        fields = remain.split(".")
        name = fields[0]
        expression: ExpressionT = Variable(prefixed, name, expression_range)
        for selector_field, selector_range in selectors_split(
            ".".join(fields[1:]), expression._range
        ):
            expression = Selector(
                expression,
                selector_field,
                mergeRanges(expression._range, selector_range),
            )
        return expression

    def expression_selector_variable(self, token: Token) -> ExpressionT:
        all_of = token.value.split(".", 1)
        name = all_of[0]
        accessors = all_of[1]

        prefix_len = len(name)
        expression_range = Range(
            token.line,
            token.line,
            token.column,
            token.column + prefix_len + 1,
            token.start_pos,
            token.start_pos + prefix_len + 1,
        )
        expression: ExpressionT = Variable([], name, expression_range)
        for selector_field, selector_range in selectors_split(
            accessors, expression._range
        ):
            expression = Selector(
                expression,
                selector_field,
                mergeRanges(expression._range, selector_range),
            )
        return expression

    def expression_selector(self, expression: ExpressionT) -> ExpressionT:
        return expression

    def expression_application(self, *values: ExpressionT) -> ExpressionT:
        # rule warantie at least one item
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
                return ExpressionMeaninglessOperatorApplications(
                    list(allValues),
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
        binding: LetBinding,
        _in: Token,
        expression: ExpressionT,
    ) -> Let:
        return Let([binding], expression)

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
        return PatternMatchVariable.from_lark_token(token)

    def pattern_match_hole(self, token: Token) -> PatternMatchHole:
        return PatternMatchHole(token2Range(token))

    def pattern_match_literal(self, token: Token) -> PatternMatchLiteral:
        return PatternMatchLiteral(token)

    def pattern_match_atom(self, value: PatternMatchT) -> PatternMatchT:
        return value

    def pattern_match_constructor_application(
        self,
        maybe_constructor: PatternMatchConstructorName,
        *arguments: PatternMatchT,
    ) -> PatternMatchT:
        arguments_list = list(arguments)
        if len(arguments_list) == 0:
            return maybe_constructor
        else:
            # We can ignore error here thanks to the
            # grammar rule and the list len check
            return PatternMatchConstructor(maybe_constructor, list(arguments))

    def pattern_match(self, pattern: PatternMatchT) -> PatternMatchT:
        return pattern

    def pattern_match_function_args_atoms(
        self, *atoms: PatternMatchT
    ) -> list[PatternMatchT]:
        return list(atoms)

    # DEPRECATED
    # def pattern_match_function_args_comes(
    #    self, *mixedList: PatternMatchT | Token
    # ) -> list[PatternMatchT]:
    #    out: list[PatternMatchT] = []
    #    for maybe_pattern in mixedList:
    #        if not isinstance(maybe_pattern, Token):
    #            out.append(maybe_pattern)
    #    return out

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

    def data_type_constructors_layout(
        self, constructors: list[ConstructorDefinition]
    ) -> list[ConstructorDefinition]:
        return constructors

    # ------------------ Types ------------------

    def type_record_item(
        self, variable: Token, colon: Token, type_expression_inner: TypeT
    ) -> tuple[TypeRecordField, Range, TypeT]:
        return (
            TypeRecordField.from_lark_token(variable),
            mergeRanges(token2Range(variable), type_expression_inner._range),
            type_expression_inner,
        )

    def type_record_item_layout(
        self,
        variable: Token,
        colon: Token,
        layout_start: Token,
        type_expression_inner: TypeT,
        layout_end: Token,
    ) -> tuple[TypeRecordField, Range, TypeT]:
        return (
            TypeRecordField.from_lark_token(variable),
            mergeRanges(token2Range(variable), type_expression_inner._range),
            type_expression_inner,
        )

    def type_record_inner(
        self, items: list[tuple[TypeRecordField, Range, TypeT]]
    ) -> list[tuple[TypeRecordField, Range, TypeT]]:
        return items

    def type_record(
        self, items: list[tuple[TypeRecordField, Range, TypeT]]
    ) -> TypeRecord:
        return TypeRecord(items)

    def type_operator(self, operator: Token) -> TypeOperator:
        return TypeOperator.from_lark_token(operator)

    def type_variable(self, variable: Token) -> TypeVariable:
        return TypeVariable.from_lark_token(variable)

    def type_concrete_type(self, token: Token) -> TypeConcreteName:
        return TypeConcreteName.from_lark_token(token)

    def type_atom(self, value: TypeT) -> TypeT:
        return value

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
                return TypeMeaninglessOperatorApplications(
                    list(allValues),
                    mergeRanges(allValues[0]._range, allValues[-1]._range),
                )

    def type_expression_inner(self, types: list[TypeT]) -> TypeT:
        out = types[-1]
        for type_ in types[::-1][1:]:
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

    ## ----------------- Modules ---------------

    def import_constructor(self, token: Token) -> ImportConstructorName:
        return ImportConstructorName.from_lark_token(token)

    def import_constructors(
        self, constructors: list[ImportConstructorName]
    ) -> list[ImportConstructorName]:
        return constructors

    def import_type(
        self,
        type_name: Token,
        maybe_constructors: Optional[list[ImportConstructorName]],
    ) -> ImportType:
        constructors: list[ImportConstructorName]
        if maybe_constructors is None:
            constructors = []
            _range1 = token2Range(type_name)
        else:
            constructors = maybe_constructors
            _range1 = constructors[-1]._range

        name = ImportTypeName.from_lark_token(type_name)
        return ImportType(mergeRanges(name._range, _range1), name, constructors)

    def import_function(self, name: Token) -> ImportFunction:
        return ImportFunction.from_lark_token(name)

    def import_operator(
        self, maybe_type: Optional[Token], name: Token
    ) -> ImportNameT:
        if maybe_type is None:
            return ImportOperatorType.from_lark_token(name)
        else:
            return ImportOperatorExpression.from_lark_token(name)

    def module_import(self, name: ImportNameT) -> ImportNameT:
        return name

    def module_imports(self, imports: list[ImportNameT]) -> list[ImportNameT]:
        return imports

    def export_constructor(self, name: Token) -> ExportConstructorName:
        return ExportConstructorName.from_lark_token(name)

    def export_constructors(
        self, value: list[ExportConstructorName]
    ) -> list[ExportConstructorName]:
        return value

    def export_type(
        self,
        name: Token,
        maybe_constructors: Optional[list[ExportConstructorName]],
    ) -> ExportType:
        type_name = ExportTypeName.from_lark_token(name)
        if maybe_constructors is None:
            constructors = []
        else:
            constructors = maybe_constructors
        _range = mergeRanges(token2Range(name), constructors[-1]._range)
        return ExportType(_range, type_name, constructors)

    def export_function(self, variable: Token) -> ExportFunction:
        return ExportFunction.from_lark_token(variable)

    def export_operator(
        self, maybe_type: Optional[Token], operator: Token
    ) -> ExportNameT:
        if maybe_type is None:
            return ExportOperatorType.from_lark_token(operator)
        else:
            return ExportOperatorExpression.from_lark_token(operator)

    def module_export(self, export: ExportNameT) -> ExportNameT:
        return export

    def module_exports(self, exports: list[ExportNameT]) -> list[ExportNameT]:
        return exports

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

    def top_data_type(
        self,
        data: Token,
        constructor_or_operator: Token,
        maybe_args: Optional[list[TypeVariable]],
        equal: Token,
        constructors: list[ConstructorDefinition],
    ) -> DataType:
        name: Operator | ConstructorName
        if constructor_or_operator.type == "OPERATOR":
            name = Operator.from_lark_token(constructor_or_operator)
        else:
            name = ConstructorName.from_lark_token(constructor_or_operator)
        if maybe_args is None:
            maybe_args = []
        return DataType(
            name,
            maybe_args,
            constructors,
            mergeRanges(token2Range(data), constructors[-1]._range),
        )

    def top_data_type_layout(
        self,
        data: Token,
        constructor_or_operator: Token,
        maybe_args: Optional[list[TypeVariable]],
        equal: Token,
        layout_start: Token,
        constructors: list[ConstructorDefinition],
        layout_end: Token,
    ) -> DataType:
        name: Operator | ConstructorName
        if constructor_or_operator.type == "OPERATOR":
            name = Operator.from_lark_token(constructor_or_operator)
        else:
            name = ConstructorName.from_lark_token(constructor_or_operator)
        if maybe_args is None:
            maybe_args = []
        return DataType(
            name,
            maybe_args,
            constructors,
            mergeRanges(token2Range(data), constructors[-1]._range),
        )

    def top_module(
        self,
        module: Token,
        name: Token,
        exports: Optional[list[ExportNameT]],
        where: Token,
    ) -> Exports:
        module_name = ExportModuleName.from_lark_token(name)
        if exports:
            _range = mergeRanges(module_name._range, exports[-1]._range)
        else:
            _range = module_name._range
        if exports is None:
            exports = []
        return Exports(_range, module_name, exports)

    # FIXME:
    def top(self, top_module: Exports, imports, *all_others) -> TopT:
        return Module(None, top_module, imports, [], [], list(all_others))


def tree2sugared(trees: list[Tree]) -> list[TopT]:
    toSST = ToSST()
    return [toSST.transform(tree) for tree in trees]


def selectors_split(
    selectors: str, old_range: Range
) -> list[tuple[str, Range]]:
    list_selectors = selectors.split(".")
    intermediate_range: Range = old_range
    out: list[tuple[str, Range]] = []
    for selector in list_selectors[:-1]:
        intermediate_range = range_from_range_and_string(
            intermediate_range, selector
        )
        out.append((selector, intermediate_range))
    selector = list_selectors[-1]
    final = range_from_range_and_string(intermediate_range, selector)
    final = Range(
        intermediate_range.line_start,
        intermediate_range.line_end,
        intermediate_range.column_end,
        intermediate_range.column_end + len(selector) + 1,
        intermediate_range.position_end,
        intermediate_range.position_end + len(selector) + 1,
    )
    out.append((selector, final))
    return out


def range_from_range_and_string(_range: Range, value: str) -> Range:
    """
    to use for selectors split
    """
    return Range(
        _range.line_start,
        _range.line_end,
        _range.column_end,
        _range.column_end + len(value) + 1,
        _range.position_end,
        _range.position_end + len(value) + 1,
    )
