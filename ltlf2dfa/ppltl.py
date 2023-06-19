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

"""This module contains the implementation of Past Linear Temporal Logic on finite traces."""

import re
from abc import ABC, abstractmethod
from typing import Any, List, Optional

from ltlf2dfa.base import (
    AtomicFormula,
    AtomSymbol,
    BinaryOperator,
    Formula,
    UnaryOperator,
)
from ltlf2dfa.helpers import new_var
from ltlf2dfa.ltlf2dfa import to_dfa
from ltlf2dfa.pl import PLAtomic
from ltlf2dfa.symbols import OpSymbol, Symbols


class PPLTLFormula(Formula, ABC):
    """A class for the PPLTL formula."""

    def to_nnf(self) -> "PPLTLFormula":
        """Convert an PPLTL formula in NNF."""
        return self

    @abstractmethod
    def negate(self) -> "PPLTLFormula":
        """Negate the formula."""

    def __repr__(self):
        """Get the representation."""
        return self.__str__()

    def to_mona(self, v: Optional[Any] = None) -> str:
        """
        Tranform the PPLTL formula into its encoding in MONA.

        :return: a string.
        """
        raise NotImplementedError()

    # def to_pldlf(self):
    #     """
    #     Tranform the formula into an equivalent LDLf formula.
    #
    #     :return: an LDLf formula.
    #     """

    def to_dfa(self, mona_dfa_out: bool = False) -> str:
        """
        Translate into a DFA.

        :param mona_dfa_out: flag for DFA output in MONA syntax.
        """
        return to_dfa(self, mona_dfa_out)


class PPLTLUnaryOperator(UnaryOperator[PPLTLFormula], PPLTLFormula, ABC):
    """A unary operator for PPLTL."""


class PPLTLBinaryOperator(BinaryOperator[PPLTLFormula], PPLTLFormula, ABC):
    """A binary operator for PPLTL."""


class PPLTLAtomic(AtomicFormula, PPLTLFormula):
    """Class for PPLTL atomic formulas."""

    name_regex = re.compile(r"[a-z][a-z0-9_]*")

    def negate(self):
        """Negate the formula."""
        return PPLTLNot(self)

    def find_labels(self) -> List[AtomSymbol]:
        """Find the labels."""
        return PLAtomic(self.s).find_labels()

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL atomic formula."""
        if v != "max($)":
            return f"({v} in {self.s.upper()})"
        return PLAtomic(self.s).to_mona(v="max($)")

    # def to_pldlf(self):
    #     return LDLfPropositional(PLAtomic(self.s)).convert()


class PPLTLTrue(PPLTLAtomic):
    """Class for the PPLTL True formula."""

    def __init__(self):
        """Initialize the formula."""
        super().__init__(Symbols.TRUE.value)

    def negate(self):
        """Negate the formula."""
        return PPLTLFalse()

    def find_labels(self) -> List[AtomSymbol]:
        """Find the labels."""
        return []

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding for True."""
        return Symbols.TRUE.value


class PPLTLFalse(PPLTLAtomic):
    """Class for the PPLTL False formula."""

    def __init__(self):
        """Initialize the formula."""
        super().__init__(Symbols.FALSE.value)

    def negate(self):
        """Negate the formula."""
        return PPLTLTrue()

    def find_labels(self) -> List[AtomSymbol]:
        """Find the labels."""
        return []

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding for False."""
        return Symbols.FALSE.value


class PPLTLNot(PPLTLUnaryOperator):
    """Class for the PPLTL not formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.NOT.value

    def to_nnf(self) -> PPLTLFormula:
        """Transform to NNF."""
        if not isinstance(self.f, AtomicFormula):
            return self.f.negate().to_nnf()
        return self

    def negate(self) -> PPLTLFormula:
        """Negate the formula."""
        return self.f

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL Not formula."""
        return f"~({self.f.to_mona(v)})"

    # def to_pldlf(self):
    #     return LDLfNot(self.f.to_pldlf())


class PPLTLAnd(PPLTLBinaryOperator):
    """Class for the PPLTL And formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.AND.value

    def negate(self) -> PPLTLFormula:
        """Negate the formula."""
        return PPLTLOr([f.negate() for f in self.formulas])

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL And formula."""
        return f"({' & '.join([f.to_mona(v) for f in self.formulas])})"

    # def to_pldlf(self):
    #     return LDLfAnd([f.to_pldlf() for f in self.formulas])


class PPLTLOr(PPLTLBinaryOperator):
    """Class for the PPLTL Or formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.OR.value

    def negate(self) -> PPLTLFormula:
        """Negate the formula."""
        return PPLTLAnd([f.negate() for f in self.formulas])

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL Or formula."""
        return f"({' | '.join([f.to_mona(v) for f in self.formulas])})"


class PPLTLImplies(PPLTLBinaryOperator):
    """Class for the PPLTL Implication formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.IMPLIES.value

    def negate(self) -> PPLTLFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def to_nnf(self) -> PPLTLFormula:
        """Transform to NNF."""
        first, second = self.formulas[0:2]
        final_formula = PPLTLOr([PPLTLNot(first).to_nnf(), second.to_nnf()])
        for subformula in self.formulas[2:]:
            final_formula = PPLTLOr(
                [PPLTLNot(final_formula).to_nnf(), subformula.to_nnf()]
            )
        return final_formula

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL Implication formula."""
        return self.to_nnf().to_mona(v)


class PPLTLEquivalence(PPLTLBinaryOperator):
    """Class for the PPLTL Equivalente formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.EQUIVALENCE.value

    def to_nnf(self) -> PPLTLFormula:
        """Transform to NNF."""
        fs = self.formulas
        pos = PPLTLAnd(fs)
        neg = PPLTLAnd([PPLTLNot(f) for f in fs])
        res = PPLTLOr([pos, neg]).to_nnf()
        return res

    def negate(self) -> PPLTLFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL Equivalence formula."""
        return self.to_nnf().to_mona(v)


class PPLTLBefore(PPLTLUnaryOperator):
    """Class for the PPLTL Before formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.BEFORE.value

    def to_nnf(self) -> PPLTLFormula:
        """Transform to NNF."""
        return PPLTLBefore(self.f.to_nnf())

    def negate(self) -> PPLTLFormula:
        """Negate the formula."""
        return PPLTLWeakBefore(self.f.negate())

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL Before formula."""
        ex_var = new_var(v)
        if v != "max($)":
            return f"(ex1 {ex_var}: {ex_var} in $ & {ex_var}={v}-1 & {v}>0 & {self.f.to_mona(ex_var)})"
        return f"(ex1 {ex_var}: {ex_var} in $ & {ex_var}=max($)-1 & max($)>0 & {self.f.to_mona(ex_var)})"

    # def to_pldlf(self):
    #     return LDLfDiamond(
    #         RegExpPropositional(PLTrue()),
    #         LDLfAnd([self.f.to_pldlf(), LDLfNot(LDLfEnd())]),
    #     )


class PPLTLWeakBefore(PPLTLUnaryOperator):
    """Class for the PPLTL Weak Before formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.WEAK_BEFORE.value

    def to_nnf(self) -> PPLTLFormula:
        """Transform to NNF."""
        return PPLTLWeakBefore(self.f.to_nnf())

    def negate(self) -> PPLTLFormula:
        """Negate the formula."""
        return PPLTLBefore(self.f.negate())

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL Weak Before formula."""
        ex_var = new_var(v)
        if v != "max($)":
            return f"~(ex1 {ex_var}: {ex_var} in $ & {ex_var}={v}-1 & {v}>0 & ~({self.f.to_mona(ex_var)}))"
        return f"~(ex1 {ex_var}: {ex_var} in $ & {ex_var}=max($)-1 & max($)>0 & ~({self.f.to_mona(ex_var)}))"

    # def to_pldlf(self):
    #     return LDLfDiamond(
    #         RegExpPropositional(PLTrue()),
    #         LDLfAnd([self.f.to_pldlf(), LDLfNot(LDLfEnd())]),
    #     )


class PPLTLSince(PPLTLBinaryOperator):
    """Class for the PPLTL Since formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.SINCE.value

    def to_nnf(self):
        """Transform to NNF."""
        return PPLTLSince([f.to_nnf() for f in self.formulas])

    def negate(self):
        """Negate the formula."""
        return PPLTLPastRelease([f.negate() for f in self.formulas])

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL Since formula."""
        ex_var = new_var(v)
        all_var = new_var(ex_var)
        f1 = self.formulas[0].to_mona(v=all_var)
        f2 = (
            PPLTLSince(self.formulas[1:]).to_mona(v=ex_var)
            if len(self.formulas) > 2
            else self.formulas[1].to_mona(v=ex_var)
        )
        return (
            f"(ex1 {ex_var}: {ex_var} in $ & 0<={ex_var}&{ex_var}<={v} & {f2} & "
            f"(all1 {all_var}: {all_var} in $ & {ex_var}<{all_var}&{all_var}<={v} => {f1}))"
        )

    # def to_pldlf(self):
    #     f1 = self.formulas[0].to_pldlf()
    #     f2 = (
    #         PPLTLSince(self.formulas[1:]).to_pldlf()
    #         if len(self.formulas) > 2
    #         else self.formulas[1].to_pldlf()
    #     )
    #     return LDLfDiamond(
    #         RegExpStar(RegExpSequence([RegExpTest(f1), RegExpPropositional(PLTrue())])),
    #         LDLfAnd([f2, LDLfNot(LDLfEnd())]),
    #     )


class PPLTLPastRelease(PPLTLBinaryOperator):
    """Class for the PPLTL Past Release formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.PAST_RELEASE.value

    def to_nnf(self):
        """Transform to NNF."""
        return PPLTLPastRelease([f.to_nnf() for f in self.formulas])

    def negate(self):
        """Negate the formula."""
        return PPLTLSince([f.negate() for f in self.formulas])

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL Past Release formula."""
        ex_var = new_var(v)
        all_var = new_var(ex_var)
        f1 = self.formulas[0].to_mona(v=all_var)
        f2 = (
            PPLTLSince(self.formulas[1:]).to_mona(v=ex_var)
            if len(self.formulas) > 2
            else self.formulas[1].to_mona(v=ex_var)
        )
        return (
            f"~(ex1 {ex_var}: {ex_var} in $ & 0<={ex_var}&{ex_var}<={v} & ~({f2}) & "
            f"(all1 {all_var}: {all_var} in $ & {ex_var}<{all_var}&{all_var}<={v} => ~({f1})))"
        )

    # def to_pldlf(self):
    #     f1 = self.formulas[0].to_pldlf()
    #     f2 = (
    #         PPLTLSince(self.formulas[1:]).to_pldlf()
    #         if len(self.formulas) > 2
    #         else self.formulas[1].to_pldlf()
    #     )
    #     return LDLfDiamond(
    #         RegExpStar(RegExpSequence([RegExpTest(f1), RegExpPropositional(PLTrue())])),
    #         LDLfAnd([f2, LDLfNot(LDLfEnd())]),
    #     )


class PPLTLOnce(PPLTLUnaryOperator):
    """Class for the PPLTL Once formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.ONCE.value

    def to_nnf(self) -> PPLTLFormula:
        """Transform to NNF."""
        return PPLTLSince([PPLTLTrue(), self.f])

    def negate(self) -> PPLTLFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL Once formula."""
        return PPLTLSince([PPLTLTrue(), self.f]).to_mona(v)

    # def to_pldlf(self):
    #     return LDLfDiamond(
    #         RegExpStar(RegExpPropositional(PLTrue())),
    #         LDLfAnd([self.f.to_pldlf(), LDLfNot(LDLfEnd())]),
    #     )


class PPLTLHistorically(PPLTLUnaryOperator):
    """Class for the PPLTL Historically formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.HISTORICALLY.value

    def to_nnf(self) -> PPLTLFormula:
        """Transform to NNF."""
        return PPLTLPastRelease([PPLTLFalse(), self.f.to_nnf()])

    def negate(self) -> PPLTLFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL Historically formula."""
        return PPLTLNot(PPLTLOnce(PPLTLNot(self.f))).to_mona(v)


class PPLTLStart(PPLTLFormula):
    """Class for the PPLTL Start formula."""

    # def to_nnf(self) -> PPLTLFormula:
    #     """Transform to NNF."""
    #     return PPLTLAnd([PPLTLWeakBefore(PPLTLFalse()), PPLTLNot(PPLTLEnd())]).to_nnf()

    def negate(self) -> PPLTLFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def find_labels(self) -> List[AtomSymbol]:
        """Find the labels."""
        return []

    def _members(self):
        return (Symbols.START.value,)

    def __str__(self):
        """Get the string representation."""
        return Symbols.START.value

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PPLTL Start formula."""
        return PPLTLWeakBefore(PPLTLFalse()).to_mona(v)


# class PPLTLEnd(PPLTLFormula):
#     """Class for the PPLTL End formula."""
#
#     def find_labels(self) -> List[AtomSymbol]:
#         """Find the labels."""
#         return []
#
#     def _members(self):
#         return (Symbols.END.value,)
#
#     def to_nnf(self) -> PPLTLFormula:
#         """Transform to NNF."""
#         return PPLTLHistorically(PPLTLFalse()).to_nnf()
#
#     def negate(self) -> PPLTLFormula:
#         """Negate the formula."""
#         return self.to_nnf().negate()
#
#     def __str__(self):
#         """Get the string representation."""
#         return "_".join(map(str, self._members()))
