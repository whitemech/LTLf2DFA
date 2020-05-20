# -*- coding: utf-8 -*-

"""Base classes for the implementation of a generic syntax tree."""

from abc import abstractmethod, ABC
from typing import Sequence, Set, Tuple, TypeVar, Generic, cast, Union, Optional, Any
import re

from ltlf2dfa.symbols import Symbols, OpSymbol
from ltlf2dfa.helpers import Hashable, Wrapper

AtomSymbol = Union["QuotedFormula", str]


class Formula(Hashable, ABC):
    """Abstract class for a formula."""

    @abstractmethod
    def find_labels(self) -> Set[AtomSymbol]:
        """Return the set of symbols."""

    def to_nnf(self) -> "Formula":
        """Transform the formula in NNF."""
        return self

    @abstractmethod
    def negate(self) -> "Formula":
        """Negate the formula. Used by 'to_nnf'."""

    @abstractmethod
    def to_mona(self, v: Optional[Any] = None) -> str:
        """Transform the formula in MONA."""


class AtomicFormula(Formula, ABC):
    """An abstract atomic formula.

    Both formulas and names can be used as atomic symbols.
    A name must be a string made of letters, numbers, underscores, or it must
    be a quoted string.
    """

    name_regex = re.compile(r'(\w+)|(".*")')

    def __init__(self, s: Union[AtomSymbol, Formula]):
        """Inintializes the atomic formula.

        :param s: the atomic symbol. Formulas are implicitly converted to
            quoted formulas.
        """
        super().__init__()

        # If formula
        if isinstance(s, Formula):
            self.s = QuotedFormula(s)  # type: AtomSymbol

        # If name
        else:
            self.s = str(s)
            if not self.name_regex.fullmatch(self.s):
                raise ValueError(
                    "The symbol name does not respect the naming convention."
                )

    def _members(self):
        return self.s

    def __str__(self):
        """Get the string representation."""
        return str(self.s)

    def find_labels(self) -> Set[AtomSymbol]:
        """Return the set of symbols."""
        return {self.s}


class QuotedFormula(Wrapper):
    """This object is a constant representation of a formula.

    This can be used as a normal formula. Quoted formulas can also be used as
    hashable objects and for atomic symbols.
    """

    def __init__(self, f: Formula):
        """Initialize.

        :param f: formula to represent.
        """
        super().__init__(f)
        self.__dict__["_QuotedFormula__str"] = '"' + str(f) + '"'

    def __str__(self):
        """Cache str."""
        return self.__str

    def __repr__(self):
        """Nice representation."""
        return str(self)


class MonaProgram:
    """Implements a MONA program."""

    header = "m2l-str"
    vars: Set[str] = set()

    def __init__(self, f: Formula):
        """Initialize.

        :param f: formula to encode.
        :param i: instant of evaluation in the trace.
        """
        self.formula = f
        self._set_vars()

    def _set_vars(self):
        """Set MONA vars."""
        self.vars = set([v.upper() for v in self.formula.find_labels()])

    def __repr__(self):
        """Nice representation."""
        return str(self)

    def mona_program(self) -> str:
        """Construct the MONA program."""
        if self.vars:
            return "#{};\n{};\nvar2 {};\n{};\n".format(
                str(self.formula),
                self.header,
                ", ".join(self.vars),
                self.formula.to_mona(),
            )
        else:
            return "#{};\n{};\n{};\n".format(
                str(self.formula), self.header, self.formula.to_mona()
            )


class Operator(Formula, ABC):
    """Implements an operator."""

    base_expression = (
        Symbols.ROUND_BRACKET_LEFT.value + "%s" + Symbols.ROUND_BRACKET_RIGHT.value
    )

    @property
    @abstractmethod
    def operator_symbol(self) -> OpSymbol:
        """Get the symbol of the operator."""


T = TypeVar("T")
OperatorChildren = Sequence[T]


class UnaryOperator(Generic[T], Operator, ABC):
    """A class to represent unary operator."""

    def __init__(self, f: T):
        """
        Instantiate the unary operator over a formula.

        :param f: the formula on which the operator is applied.
        """
        super().__init__()
        self.f = f

    def __str__(self):
        """Get the string representation."""
        return (
            str(self.operator_symbol)
            + Symbols.ROUND_BRACKET_LEFT.value
            + str(self.f)
            + Symbols.ROUND_BRACKET_RIGHT.value
        )

    def _members(self):
        return self.operator_symbol, self.f

    def __lt__(self, other):
        """Compare the formula with another formula."""
        return self.f.__lt__(other.f)

    def find_labels(self) -> Set[AtomSymbol]:
        """Return the set of symbols."""
        return cast(Formula, self.f).find_labels()


class BinaryOperator(Generic[T], Operator, ABC):
    """A generic binary formula."""

    def __init__(self, formulas: OperatorChildren):
        """
        Initialize the binary operator.

        :param formulas: the children formulas of the operator.
        """
        super().__init__()
        assert len(formulas) >= 2
        self.formulas = tuple(formulas)  # type: OperatorChildren

    def __str__(self):
        """Return the string representation."""
        return (
            "("
            + (" " + str(self.operator_symbol) + " ").join(map(str, self.formulas))
            + ")"
        )

    def _members(self) -> Tuple[OpSymbol, OperatorChildren]:
        return self.operator_symbol, self.formulas

    def find_labels(self) -> Set[AtomSymbol]:
        """Return the set of symbols."""
        return set.union(*map(lambda f: f.find_labels(), self.formulas))

    def to_nnf(self):
        """Transform in NNF."""
        return type(self)([f.to_nnf() for f in self.formulas])
