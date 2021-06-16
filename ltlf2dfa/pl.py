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

"""This module provides support for Propositional Logic."""

import functools
from abc import ABC, abstractmethod
from typing import Any, List, Optional, Set

from ltlf2dfa.base import AtomicFormula, BinaryOperator, Formula, UnaryOperator
from ltlf2dfa.symbols import OpSymbol, Symbols


class PLFormula(Formula):
    """A class to represent propositional formulas."""

    def __init__(self):
        """Initialize a PL formula."""
        Formula.__init__(self)
        self._atoms = None  # type: Optional[Set[PLAtomic]]

    def __repr__(self):
        """Return a representation of the formula."""
        return str(self)

    def find_atomics(self) -> Set["PLAtomic"]:
        """
        Find all the atomic formulas in the propositional formulas.

        That is, find the leaves in the syntax tree.

        :return: the set of  atomic formulas.
        """
        if self._atoms is None:
            self._atoms = self._find_atomics()
        return self._atoms

    @abstractmethod
    def _find_atomics(self) -> Set["PLAtomic"]:
        """Find all the atomic formulas in the propositional formulas."""

    @abstractmethod
    def negate(self) -> "PLFormula":
        """Negate the formula. Used by 'to_nnf'."""

    def to_mona(self, v: Optional[Any] = None) -> str:
        """
        Tranform the PL formula into its encoding in MONA.

        :return: a string.
        """


class PLAtomic(AtomicFormula, PLFormula):
    """A class to represent propositional atomic formulas."""

    def find_labels(self) -> List[Any]:
        """Return the list of symbols."""
        return [self.s]

    def _find_atomics(self):
        return {self}

    def negate(self) -> PLFormula:
        """Negate the formula."""
        return PLNot(self)

    def to_mona(self, v="0") -> str:
        """Return the MONA encoding of a PL atomic formula."""
        return f"({v} in {self.s.upper()})"


class PLBinaryOperator(BinaryOperator[PLFormula], PLFormula, ABC):
    """An operator for Propositional Logic."""

    def _find_atomics(self) -> Set[PLAtomic]:
        return functools.reduce(
            set.union, [f.find_atomics() for f in self.formulas]  # type: ignore
        )


class PLTrue(PLAtomic):
    """Propositional true."""

    def __init__(self):
        """Initialize the PL true formula."""
        PLAtomic.__init__(self, Symbols.TRUE.value)

    def negate(self) -> "PLFalse":
        """Negate the formula."""
        return PLFalse()

    def find_labels(self) -> List[Any]:
        """Return the list of symbols."""
        return list()

    def to_nnf(self):
        """Transform in NNF."""
        return self

    def to_mona(self, v="0") -> str:
        """Return the MONA encoding of a PL atomic formula."""
        return Symbols.TRUE.value


class PLFalse(PLAtomic):
    """Propositional false."""

    def __init__(self):
        """Initialize the formula."""
        PLAtomic.__init__(self, Symbols.FALSE.value)

    def negate(self) -> "PLTrue":
        """Negate the formula."""
        return PLTrue()

    def find_labels(self) -> List[Any]:
        """Return the list of symbols."""
        return list()

    def to_nnf(self):
        """Transform in NNF."""
        return self

    def to_mona(self, v="0") -> str:
        """Return the MONA encoding of a PL atomic formula."""
        return Symbols.FALSE.value


class PLNot(UnaryOperator[PLFormula], PLFormula):
    """Propositional Not."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.NOT.value

    def to_nnf(self):
        """Transform in NNF."""
        if not isinstance(self.f, AtomicFormula):
            return self.f.negate().to_nnf()
        else:
            return self

    def negate(self) -> PLFormula:
        """Negate the formula."""
        return self.f

    def _find_atomics(self) -> Set["PLAtomic"]:
        return self.f.find_atomics()


class PLOr(PLBinaryOperator):
    """Propositional Or."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.OR.value

    def to_nnf(self):
        """Transform in NNF."""
        return PLOr([f.to_nnf() for f in self.formulas])

    def negate(self) -> PLFormula:
        """Negate the formula."""
        return PLAnd([f.negate() for f in self.formulas])


class PLAnd(PLBinaryOperator):
    """Propositional And."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.AND.value

    def to_nnf(self):
        """Transform in NNF."""
        return PLAnd([f.to_nnf() for f in self.formulas])

    def negate(self) -> PLFormula:
        """Negate the formula."""
        return PLOr([f.negate() for f in self.formulas])


class PLImplies(PLBinaryOperator):
    """Propositional Implication."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.IMPLIES.value

    def negate(self) -> PLFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def to_nnf(self):
        """Transform in NNF."""
        first, second = self.formulas[0:2]
        final_formula = PLOr([PLNot(first).to_nnf(), second.to_nnf()])
        for subformula in self.formulas[2:]:
            final_formula = PLOr([PLNot(final_formula).to_nnf(), subformula.to_nnf()])
        return final_formula


class PLEquivalence(PLBinaryOperator):
    """Propositional Equivalence."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.EQUIVALENCE.value

    def to_nnf(self):
        """Transform in NNF."""
        fs = self.formulas
        pos = PLAnd(fs)
        neg = PLAnd([PLNot(f) for f in fs])
        res = PLOr([pos, neg]).to_nnf()
        return res

    def negate(self) -> PLFormula:
        """Negate the formula."""
        return self.to_nnf().negate()
