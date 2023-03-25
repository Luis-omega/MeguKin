from typing import List, Optional, Union
from lark import Transformer, v_args, Token, Tree

from MeguKin.Types.Top import Constructor, DataType
from MeguKin.Types.Type import Type, TypeName, TypeApplication


@v_args(inline=True)
class ToAST(Transformer):
    def sep_by1(self, *init):
        others = [init[i] for i in range(0, len(init), 2)]
        return others

    def braces(self, start, value, end):
        return value

    def parens(self, start, value, end):
        return value

    def type_expression(self, value: List[Type]) -> Type:
        firstValue = value[0]
        if len(value) == 1:
            return firstValue
        else:
            out = firstValue
            for argument in value[1:]:
                out = TypeApplication(out, argument)
            return out

    def type_constructor(
        self, name, arguments: Optional[List[Union[Type, Token]]] = None
    ) -> Type:
        typeName = TypeName(name.value)
        if arguments is None:
            return typeName

        firstArgument: Union[Type, Token] = arguments[0]
        if isinstance(firstArgument, Token):
            typedArgument: Type = TypeName(firstArgument.value)
        else:
            typedArgument = firstArgument

        out = TypeApplication(typeName, typedArgument)
        for argument in arguments[1:]:
            newArgument: Type
            if isinstance(argument, Token):
                newArgument = TypeName(argument.value)
            else:
                newArgument = argument
            out = TypeApplication(out, newArgument)
        return out

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

    def top_data_type(
        self, data, typeName: Token, eq, constructors: List[Constructor]
    ) -> DataType:
        return DataType(typeName.value, constructors)

    def top(self, *values):
        return values
