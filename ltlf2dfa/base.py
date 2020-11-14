#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# This file is part of ltlf2dfa.
#
# ltlf2dfa is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ltlf2dfa is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ltlf2dfa.  If not, see <https://www.gnu.org/licenses/>.
#

"""Base classes for the implementation of a generic syntax tree."""
import functools
import re
from abc import ABC, abstractmethod
from typing import Any, Generic, List, Optional, Sequence, Tuple, TypeVar, Union, cast

from ltlf2dfa.helpers import Hashable, Wrapper
from ltlf2dfa.symbols import OpSymbol, Symbols

AtomSymbol = Union["QuotedFormula", str]


class Formula(Hashable, ABC):
    """Abstract class for a formula."""

    @abstractmethod
    def find_labels(self) -> List[AtomSymbol]:
        """Return the list of symbols."""

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

    def find_labels(self) -> List[AtomSymbol]:
        """Return the list of symbols."""
        return [self.s]


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

    HEADER = "var2 $ where ~ex1 p where true: p notin $ & p+1 in $;\nallpos $"
    vars: List[str] = list()

    def __init__(self, f: Formula):
        """Initialize.

        :param f: formula to encode.
        :param i: instant of evaluation in the trace.
        """
        self.formula = f
        self._vars()

    def _vars(self):
        """List MONA vars."""
        self.vars = [v.upper() for v in self.formula.find_labels()]

    def __repr__(self):
        """Nice representation."""
        return str(self)

    def mona_program(self) -> str:
        """Construct the MONA program."""
        if self.vars:
            return "#{};\n{};\nvar2 {};\n{};\n".format(
                str(self.formula),
                self.HEADER,
                ", ".join(self.vars),
                self.formula.to_mona(),
            )
        else:
            return "#{};\n{};\n{};\n".format(
                str(self.formula), self.HEADER, self.formula.to_mona()
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

    def find_labels(self) -> List[AtomSymbol]:
        """Return the list of symbols."""
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

    def find_labels(self) -> List[AtomSymbol]:
        """Return the list of symbols."""
        # return set.union(*map(lambda f: f.find_labels(), self.formulas))))
        # seen = set()
        # result = []
        # for f in self.formulas:
        #     for lab in f.find_labels():
        #         if lab not in seen:
        #             result.append(lab)
        #             seen.add(lab)
        # return result
        return flatten(self.formulas)

    def to_nnf(self):
        """Transform in NNF."""
        return type(self)([f.to_nnf() for f in self.formulas])


@functools.singledispatch
def flatten(lst) -> List:
    """Flatten a list of lists."""
    return [item for sublist in lst for item in sublist.find_labels()]


def _flatten_as_set(lst):
    """Flatten a list of lists removing duplicates."""
    seen = set()
    result = []
    for f in lst:
        for lab in f.find_labels():
            if lab not in seen:
                result.append(lab)
                seen.add(lab)
    return result


@flatten.register(tuple)
def _(lst: Tuple[Formula]):
    """Flatten a list of lists of formulas."""
    return _flatten_as_set(lst)
