from typing import Union, TypeVar, Optional
from MeguKin.SugaredSyntaxTree.Top import TopT, Module
from MeguKin.SugaredSyntaxTree.SST import MetaVar
from MeguKin.Error import MeguKinError
import MeguKin.SugaredSyntaxTree as SST
from MeguKin.File import Range
from typing import NewType
from MeguKin.SugaredSyntaxTree.Expression import (
    Variable,
    Literal,
    Operator,
    PrefixOperator,
    ConstructorName,
    RecordUpdate,
    Record,
    Selector,
    Application,
    AnnotatedExpression,
    ExpressionTypeArgument,
    ExpressionMeaninglessOperatorApplications,
    CaseCase,
    Case,
    Function,
    LetBinding,
    Let,
    ExpressionT,
)

T = TypeVar("T")


class Top:
    pass


def desugar(elems: list[TopT]) -> Top:
    pass
