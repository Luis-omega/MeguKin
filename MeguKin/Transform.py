from typing import List, Optional, Union
from lark import Transformer, v_args, Token, Tree

from MeguKin.Types.Top import Constructor, DataType, Top, Definition, Declaration
from MeguKin.Types.Type import Type, TypeName, TypeApplication, TypeArrow
from MeguKin.Types.Expression import Expression


@v_args(inline=True)
class ToAST(Transformer):
    # ------------------ Combinators ------------------
    def sep_by1(self, *init):
        others = [init[i] for i in range(0, len(init), 2)]
        return others

    def braces(self, start, value, end):
        return value

    def parens(self, start, value, end):
        return value

    # ------------------ Expressions ------------------

    # ------------------ PatternMatch ------------------

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
    def type_atom(self, value: List[Union[Token, Type]]) -> Type:
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
    def top_variable_declaration(
        self, name: [Token], colon, _type: [Type]
    ) -> Declaration:
        return Declaration(name, _type)

    def top_variable_definition(
        self, name: [Token], colon, expression: [Expression]
    ) -> Definition:
        return Definition(name, _type)

    def top_data_type(
        self, data, typeName: Token, eq, constructors: List[Constructor]
    ) -> DataType:
        return DataType(typeName.value, constructors)

    def top(self, *values: List[Top]) -> List[Top]:
        return values
