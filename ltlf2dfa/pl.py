# -*- coding: utf-8 -*-

"""This module provides support for Propositional Logic."""

import functools
from abc import abstractmethod, ABC
from typing import Set, Any, Optional

# import sympy
# from sympy.logic.boolalg import Boolean, BooleanTrue, BooleanFalse
# from pythomata import PropositionalInterpretation

from ltlf2dfa.base import Formula, AtomicFormula, BinaryOperator, UnaryOperator
from ltlf2dfa.symbols import Symbols, OpSymbol


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


# def to_sympy(
#     formula: Formula, replace: Optional[Dict[AtomSymbol, sympy.Symbol]] = None
# ) -> Boolean:
#     """
#     Translate a PLFormula object into a SymPy expression.
#
#     :param formula: the formula to translate.
#     :param replace: an optional mapping from symbols to replace to other replacement symbols.
#     :return: the SymPy formula object equivalent to the formula.
#     """
#     if replace is None:
#         replace = {}
#
#     if isinstance(formula, PLTrue):
#         return BooleanTrue()
#     elif isinstance(formula, PLFalse):
#         return BooleanFalse()
#     elif isinstance(formula, PLAtomic):
#         symbol = replace.get(formula.s, formula.s)
#         return sympy.Symbol(symbol)
#     elif isinstance(formula, PLNot):
#         return sympy.Not(to_sympy(formula.f, replace=replace))
#     elif isinstance(formula, PLOr):
#         return sympy.simplify(
#             sympy.Or(*[to_sympy(f, replace=replace) for f in formula.formulas])
#         )
#     elif isinstance(formula, PLAnd):
#         return sympy.simplify(
#             sympy.And(*[to_sympy(f, replace=replace) for f in formula.formulas])
#         )
#     elif isinstance(formula, PLImplies):
#         return sympy.simplify(
#             sympy.Implies(*[to_sympy(f, replace=replace) for f in formula.formulas])
#         )
#     elif isinstance(formula, PLEquivalence):
#         return sympy.simplify(
#             sympy.Equivalent(*[to_sympy(f, replace=replace) for f in formula.formulas])
#         )
#     else:
#         raise ValueError("Formula is not valid.")


class PLAtomic(AtomicFormula, PLFormula):
    """A class to represent propositional atomic formulas."""

    def find_labels(self) -> Set[Any]:
        """Return the set of symbols."""
        return {self.s}

    def _find_atomics(self):
        return {self}

    def negate(self) -> PLFormula:
        """Negate the formula."""
        return PLNot(self)

    def to_mona(self, v="0") -> str:
        """Return the MONA encoding of a PL atomic formula."""
        if v != "0":
            return "({} in {})".format(v, self.s.upper())
        else:
            return "(0 in {})".format(self.s.upper())


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

    def find_labels(self) -> Set[Any]:
        """Return the set of symbols."""
        return set()

    def to_nnf(self):
        """Transform in NNF."""
        return self


class PLFalse(PLAtomic):
    """Propositional false."""

    def __init__(self):
        """Initialize the formula."""
        PLAtomic.__init__(self, Symbols.FALSE.value)

    def negate(self) -> "PLTrue":
        """Negate the formula."""
        return PLTrue()

    def find_labels(self) -> Set[Any]:
        """Return the set of symbols."""
        return set()

    def to_nnf(self):
        """Transform in NNF."""
        return self


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
