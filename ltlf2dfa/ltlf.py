# -*- coding: utf-8 -*-
"""
This module contains the implementation of Linear Temporal Logic on finite traces.
"""
from abc import abstractmethod, ABC
from typing import Set
import re

# from pythomata import PropositionalInterpretation
# from pythomata.impl.symbolic import SymbolicDFA

from ltlf2dfa.base import (
    Formula,
    AtomicFormula,
    UnaryOperator,
    BinaryOperator,
    AtomSymbol,
)
# from flloat.delta import Delta
# from ltlf2dfa.ltlf2dfa import to_automaton
from ltlf2dfa.pl import PLFalse, PLTrue, PLAtomic, PLOr, PLAnd, PLFormula
from ltlf2dfa.symbols import Symbols, OpSymbol


class LTLfFormula(Formula, ABC):
    """A class for the LTLf formula."""

    # def delta(self, epsilon=False) -> PLFormula:
    #     """Apply the delta function."""
    #     f = self.to_nnf()
    #     d = f._delta(i, epsilon=epsilon)
    #     if epsilon:
    #         # By definition, if epsilon=True, then the result must be either PLTrue or PLFalse
    #         # Now, the output is a Propositional Formula with only PLTrue or PLFalse as atomics
    #         # Hence, we just evaluate the formula with a dummy PropositionalInterpretation
    #         d = PLTrue() if d.truth({}) else PLFalse()
    #     return d

    # @abstractmethod
    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply delta function, assuming that 'self' is a LTLf formula in Negative Normal Form."""

    # # @abstractmethod
    # def to_ldlf(self):
    #     """
    #     Tranform the formula into an equivalent LDLf formula.
    #
    #     :return: an LDLf formula.
    #     """

    def to_nnf(self) -> "LTLfFormula":
        """Convert an LTLf formula in NNF."""
        return self

    @abstractmethod
    def negate(self) -> "LTLfFormula":
        """Negate the formula."""

    def __repr__(self):
        """Get the representation."""
        return self.__str__()

    # def to_automaton(self) -> SymbolicDFA:
    #     """Translate into an automaton."""
    #     return to_automaton(self)


class LTLfUnaryOperator(UnaryOperator[LTLfFormula], LTLfFormula, ABC):
    """A unary operator for LTLf."""


class LTLfBinaryOperator(BinaryOperator[LTLfFormula], LTLfFormula, ABC):
    """A binary operator for LTLf."""


class LTLfAtomic(AtomicFormula, LTLfFormula):
    """Class for LTLf atomic formulas."""

    name_regex = re.compile(r"[a-z][a-z0-9_]*")

    def negate(self):
        """Negate the formula."""
        return LTLfNot(self)

    # def _delta(self, i: PropositionalInterpretation, epsilon: bool = False):
    #     """Apply the delta function."""
    #     if epsilon:
    #         return PLFalse()
    #     return PLTrue() if PLAtomic(self.s).truth(i) else PLFalse()
    #
    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     if len(i) > 0:
    #         return PLAtomic(self.s).truth(i[pos])
    #     else:
    #         return False

    def find_labels(self) -> Set[AtomSymbol]:
        """Find the labels."""
        return PLAtomic(self.s).find_labels()

    # def to_ldlf(self):
    #     return LDLfPropositional(PLAtomic(self.s)).convert()


class LTLfTrue(LTLfAtomic):
    """Class for the LTLf True formula."""

    def __init__(self):
        """Initialize the formula."""
        super().__init__(Symbols.TRUE.value)

    # def _delta(self, i: PropositionalInterpretation, epsilon: bool = False):
    #     """Apply the delta function."""
    #     if epsilon:
    #         return PLFalse()
    #     else:
    #         return PLTrue()
    #
    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     return len(i) > 0

    def negate(self):
        """Negate the formula."""
        return LTLfFalse()

    def find_labels(self) -> Set[AtomSymbol]:
        """Find the labels."""
        return set()


class LTLfFalse(LTLfAtomic):
    """Class for the LTLf False formula."""

    def __init__(self):
        """Initialize the formula."""
        super().__init__(Symbols.FALSE.value)

    def negate(self):
        """Negate the formula."""
        return LTLfTrue()

    # def _delta(self, i: PropositionalInterpretation, epsilon: bool = False):
    #     """Apply the delta function."""
    #     return PLFalse()
    #
    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     return False

    def find_labels(self) -> Set[AtomSymbol]:
        """Find the labels."""
        return set()


class LTLfNot(LTLfUnaryOperator):
    """Class for the LTLf not formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.NOT.value

    def to_nnf(self) -> LTLfFormula:
        """Transform to NNF."""
        if not isinstance(self.f, AtomicFormula):
            return self.f.negate().to_nnf()
        else:
            return self

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return self.f

    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply the delta function."""
    #     if isinstance(self.f, LTLfAtomic) or isinstance(self.f, LTLfEnd):
    #         if epsilon:
    #             return PLFalse()
    #         else:
    #             return PLTrue() if self.f._delta(i, epsilon) == PLFalse() else PLFalse()
    #     else:
    #         # the formula must be in NNF form!!!
    #         raise Exception
    #
    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     if len(i) == 0:
    #         if isinstance(self.f, LTLfAtomic):
    #             return False
    #         else:
    #             return self.to_nnf().truth(i, pos)
    #     else:
    #         return not self.f.truth(i, pos)

    # def to_ldlf(self):
    #     return LDLfNot(self.f.to_ldlf())


class LTLfAnd(LTLfBinaryOperator):
    """Class for the LTLf And formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.AND.value

    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply the delta function."""
    #     return PLAnd([f._delta(i, epsilon) for f in self.formulas])
    #
    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     return all(f.truth(i, pos) for f in self.formulas)

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return LTLfOr([f.negate() for f in self.formulas])

    # def to_ldlf(self):
    #     return LDLfAnd([f.to_ldlf() for f in self.formulas])


class LTLfOr(LTLfBinaryOperator):
    """Class for the LTLf Or formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.OR.value

    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply the delta function."""
    #     return PLOr([f._delta(i, epsilon) for f in self.formulas])
    #
    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     return any(f.truth(i, pos) for f in self.formulas)

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return LTLfAnd([f.negate() for f in self.formulas])


class LTLfImplies(LTLfBinaryOperator):
    """Class for the LTLf Implication formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.IMPLIES.value

    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     return self.to_nnf().truth(i, pos)
    #
    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply the delta function."""
    #     return self.to_nnf()._delta(i, epsilon=epsilon)

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def to_nnf(self) -> LTLfFormula:
        """Transform to NNF."""
        first, second = self.formulas[0:2]
        final_formula = LTLfOr([LTLfNot(first).to_nnf(), second.to_nnf()])
        for subformula in self.formulas[2:]:
            final_formula = LTLfOr(
                [LTLfNot(final_formula).to_nnf(), subformula.to_nnf()]
            )
        return final_formula


class LTLfEquivalence(LTLfBinaryOperator):
    """Class for the LTLf Equivalente formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.EQUIVALENCE.value

    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     return self.to_nnf().truth(i, pos)
    #
    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply the delta function."""
    #     return self.to_nnf()._delta(i, epsilon=epsilon)

    def to_nnf(self) -> LTLfFormula:
        """Transform to NNF."""
        fs = self.formulas
        pos = LTLfAnd(fs)
        neg = LTLfAnd([LTLfNot(f) for f in fs])
        res = LTLfOr([pos, neg]).to_nnf()
        return res

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return self.to_nnf().negate()


class LTLfNext(LTLfUnaryOperator):
    """Class for the LTLf Next formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.NEXT.value

    def to_nnf(self) -> LTLfFormula:
        """Transform to NNF."""
        return LTLfNext(self.f.to_nnf())

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return LTLfWeakNext(self.f.negate())

    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     return pos < len(i) - 1 and self.f.truth(i, pos + 1)
    #
    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply the delta function."""
    #     if epsilon:
    #         return PLFalse()
    #     else:
    #         return PLAnd([PLAtomic(self.f), PLAtomic(LTLfNot(LTLfEnd()).to_nnf())])

    # def to_ldlf(self):
    #     return LDLfDiamond(
    #         RegExpPropositional(PLTrue()),
    #         LDLfAnd([self.f.to_ldlf(), LDLfNot(LDLfEnd())]),
    #     )


class LTLfWeakNext(LTLfUnaryOperator):
    """Class for the LTLf Weak Next formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.WEAK_NEXT.value

    def to_nnf(self) -> LTLfFormula:
        """Transform to NNF."""
        return LTLfWeakNext(self.f.to_nnf())

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return LTLfNext(self.f.negate())

    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     return not (pos < len(i) - 1) or self.f.truth(i, pos + 1)
    #
    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply the delta function."""
    #     if epsilon:
    #         return PLTrue()
    #     else:
    #         return PLOr([PLAtomic(self.f), PLAtomic(LTLfEnd().to_nnf())])

    # def to_ldlf(self):
    #     return self.convert().to_ldlf()


class LTLfUntil(LTLfBinaryOperator):
    """Class for the LTLf Until formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.UNTIL.value

    def to_nnf(self):
        """Transform to NNF."""
        return LTLfUntil([f.to_nnf() for f in self.formulas])

    def negate(self):
        """Negate the formula."""
        return LTLfRelease([f.negate() for f in self.formulas])

    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     f1 = self.formulas[0]
    #     f2 = (
    #         LTLfUntil(self.formulas[1:]) if len(self.formulas) > 2 else self.formulas[1]
    #     )
    #
    #     return any(
    #         f2.truth(i, j) and all(f1.truth(i, k) for k in range(pos, j))
    #         for j in range(pos, len(i))
    #     )
    #
    # def _delta(self, i: PropositionalInterpretation, epsilon: bool = False):
    #     """Apply the delta function."""
    #     if epsilon:
    #         return PLFalse()
    #     f1 = self.formulas[0]
    #     f2 = (
    #         LTLfUntil(self.formulas[1:]) if len(self.formulas) > 2 else self.formulas[1]
    #     )
    #     return PLOr(
    #         [
    #             f2._delta(i, epsilon),
    #             PLAnd([f1._delta(i, epsilon), LTLfNext(self)._delta(i, epsilon)]),
    #         ]
    #     )

    # def to_ldlf(self):
    #     f1 = self.formulas[0].to_ldlf()
    #     f2 = (
    #         LTLfUntil(self.formulas[1:]).to_ldlf()
    #         if len(self.formulas) > 2
    #         else self.formulas[1].to_ldlf()
    #     )
    #     return LDLfDiamond(
    #         RegExpStar(RegExpSequence([RegExpTest(f1), RegExpPropositional(PLTrue())])),
    #         LDLfAnd([f2, LDLfNot(LDLfEnd())]),
    #     )


class LTLfRelease(LTLfBinaryOperator):
    """Class for the LTLf Release formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.RELEASE.value

    def to_nnf(self):
        """Transform to NNF."""
        return LTLfRelease([f.to_nnf() for f in self.formulas])

    def negate(self):
        """Negate the formula."""
        return LTLfUntil([f.negate() for f in self.formulas])

    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     f1 = self.formulas[0]
    #     f2 = (
    #         LTLfRelease(self.formulas[1:])
    #         if len(self.formulas) > 2
    #         else self.formulas[1]
    #     )
    #     return all(
    #         f2.truth(i, j) or any(f1.truth(i, k) for k in range(pos, j))
    #         for j in range(pos, len(i))
    #     )
    #
    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply the delta function."""
    #     if epsilon:
    #         return PLTrue()
    #     f1 = self.formulas[0]
    #     f2 = (
    #         LTLfRelease(self.formulas[1:])
    #         if len(self.formulas) > 2
    #         else self.formulas[1]
    #     )
    #     return PLAnd(
    #         [
    #             f2._delta(i, epsilon),
    #             PLOr([f1._delta(i, epsilon), LTLfWeakNext(self)._delta(i, epsilon)]),
    #         ]
    #     )


class LTLfEventually(LTLfUnaryOperator):
    """Class for the LTLf Eventually formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.EVENTUALLY.value

    def to_nnf(self) -> LTLfFormula:
        """Transform to NNF."""
        return LTLfUntil([LTLfTrue(), self.f])

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     return self.to_nnf().truth(i, pos)
    #
    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply the delta function."""
    #     return self.to_nnf()._delta(i, epsilon=epsilon)

    # def to_ldlf(self):
    #     return LDLfDiamond(
    #         RegExpStar(RegExpPropositional(PLTrue())),
    #         LDLfAnd([self.f.to_ldlf(), LDLfNot(LDLfEnd())]),
    #     )


class LTLfAlways(LTLfUnaryOperator):
    """Class for the LTLf Always formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.ALWAYS.value

    def to_nnf(self) -> LTLfFormula:
        """Transform to NNF."""
        return LTLfRelease([LTLfFalse(), self.f.to_nnf()])

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     return self.to_nnf().truth(i, pos)
    #
    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply the delta function."""
    #     return self.to_nnf()._delta(i, epsilon=epsilon)


class LTLfLast(LTLfFormula):
    """Class for the LTLf Last formula."""

    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply the delta function."""
    #     return self.to_nnf()._delta(i, epsilon=epsilon)
    #
    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     return self.to_nnf().truth(i, pos)

    def to_nnf(self) -> LTLfFormula:
        """Transform to NNF."""
        return LTLfAnd([LTLfWeakNext(LTLfFalse()), LTLfNot(LTLfEnd())]).to_nnf()

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def find_labels(self) -> Set[AtomSymbol]:
        """Find the labels."""
        return set()

    def _members(self):
        return (Symbols.LAST.value,)

    def __str__(self):
        """Get the string representation."""
        return Symbols.LAST.value


class LTLfEnd(LTLfFormula):
    """Class for the LTLf End formula."""

    # def _delta(self, i: PropositionalInterpretation, epsilon=False):
    #     """Apply the delta function."""
    #     return self.to_nnf()._delta(i, epsilon=epsilon)
    #
    # def truth(self, i: FiniteTrace, pos: int = 0):
    #     """Evaluate the formula."""
    #     return self.to_nnf().truth(i, pos)

    def find_labels(self) -> Set[AtomSymbol]:
        """Find the labels."""
        return set()

    def _members(self):
        return (Symbols.END.value,)

    def to_nnf(self) -> LTLfFormula:
        """Transform to NNF."""
        return LTLfAlways(LTLfFalse()).to_nnf()

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def __str__(self):
        """Get the string representation."""
        return "_".join(map(str, self._members()))
