from typing import List, Optional, Union, TypeVar

from lark import Transformer, v_args, Token

from MeguKin.Types.Expression import (
    Expression,
    AnnotatedExpression,
    Variable,
    Int,
    Application,
    Function,
    OperatorsWithoutMeaning,
)
from MeguKin.Types.PatternMatch import (
    PatternMatch,
    PatternMatchVariable,
    PatternMatchConstructor,
)
from MeguKin.Types.Top import Constructor, DataType, Top, Definition, Declaration
from MeguKin.Types.Type import Type, TypeName, TypeArrow


T = TypeVar("T")


def is_lowercase_token(token: Token) -> bool:
    return token.type == "LOWERCASSE_IDENTIFIER"


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
        self, expression: Expression, colon=None, annotation: Optional[Type] = None
    ) -> AnnotatedExpression:
        return AnnotatedExpression(expression, annotation)

    def expression_lambda(
        self, lambda_symbol, pattern: PatternMatch, arrow, expression: Expression
    ) -> Function:
        return Function(pattern, expression)

    def expression_atom(self, atom: Union[Token, Expression]) -> Expression:
        if isinstance(atom, Token):
            if is_lowercase_token(atom):
                return Variable(atom.value)
            else:
                return Int(int(atom.value.replace("_", "")))
        else:
            return atom

    def expression_application(self, *values: Expression) -> Expression:
        # rule uses "+" so, this is guaranteed
        out = values[0]
        for value in values[1:]:
            out = Application(out, value)
        return out

    # See note about the return type [str, Expression, str, Expression,...]
    # in OperatorsWithoutMeaning
    def expression(self, *allValues) -> Expression:
        if len(allValues) == 1:
            return allValues[0]
        arg = [
            allValues[i].value if i % 2 == 1 else allValues[i]
            for i in range(len(allValues))
        ]
        return OperatorsWithoutMeaning(arg)

    # ------------------ PatternMatch ------------------
    def pattern_match_constructor_args(
        self, *args: Union[Token, PatternMatch]
    ) -> List[PatternMatch]:
        out: List[PatternMatch] = []
        for arg in args:
            if isinstance(arg, Token):
                if is_lowercase_token(arg):
                    out.append(PatternMatchVariable(arg.value))
                else:
                    out.append(PatternMatchConstructor(arg.value, []))
            else:
                out.append(arg)
        return out

    def pattern_match(
        self,
        firstArg: Union[Token, PatternMatch],
        secondArg: Optional[List[PatternMatch]] = None,
    ) -> PatternMatch:
        if isinstance(firstArg, Token):
            if is_lowercase_token(firstArg):
                return PatternMatchVariable(firstArg.value)
            else:
                if secondArg is None:
                    return PatternMatchConstructor(firstArg.value, [])
                else:
                    return PatternMatchConstructor(firstArg.value, secondArg)
        else:
            return firstArg

    # ------------------ Data ------------------

    def data_type_constructor(
        self, name: Token, types: Optional[List[Type]]
    ) -> Constructor:
        realTypes: List[Type]
        if types is None:
            realTypes = []
        else:
            realTypes = types
        return Constructor(name.value, realTypes)

    def data_type_constructors(self, sep: List[Constructor]) -> List[Constructor]:
        return sep

    # ------------------ Types ------------------
    def type_atom(self, value: Union[Token, Type]) -> Type:
        if isinstance(value, Token):
            return TypeName(value.value)
        else:
            return value

    def type_expression(self, value: List[Type]) -> Type:
        value = value[::-1]
        firstValue = value[0]
        if len(value) == 1:
            return firstValue
        else:
            out = firstValue
            for domaint in value[1:]:
                out = TypeArrow(domaint, out)
            return out

    # ------------------ Top ------------------
    def top_variable_declaration(self, name: Token, colon, _type: Type) -> Declaration:
        return Declaration(name, _type)

    def top_variable_definition(
        self, name: Token, colon, expression: Expression
    ) -> Definition:
        return Definition(name, expression)

    def top_data_type(
        self, data, typeName: Token, eq, constructors: List[Constructor]
    ) -> DataType:
        return DataType(typeName.value, constructors)

    def top(self, *values: Top) -> List[Top]:
        return list(values)
