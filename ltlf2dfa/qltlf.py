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

"""This module contains the implementation of Quantified Linear Temporal Logic on finite traces."""

from abc import ABC, abstractmethod
from typing import Any, Optional

from ltlf2dfa.helpers import new_var
from ltlf2dfa.ltlf import LTLfFormula, LTLfUnaryOperator
from ltlf2dfa.ltlf2dfa import to_dfa
from ltlf2dfa.symbols import OpSymbol, Symbols


class QLTLfFormula(LTLfFormula, ABC):
    """A class for the Quantified LTLf formula."""

    def to_nnf(self) -> "QLTLfFormula":
        """Convert an LTLf formula in NNF."""
        return self

    @abstractmethod
    def negate(self) -> "QLTLfFormula":
        """Negate the formula."""

    def __repr__(self):
        """Get the representation."""
        return self.__str__()

    def to_mona(self, v: Optional[Any] = None, w: Optional[Any] = None) -> str:
        """
        Tranform the QLTLf formula into its encoding in MONA.

        :return: a string.
        """

    #
    # def to_ldlf(self):
    #     """
    #     Tranform the formula into an equivalent LDLf formula.
    #
    #     :return: an LDLf formula.
    #     """

    def to_dfa(
        self, start: str = "0", end: str = "j", mona_dfa_out: bool = False
    ) -> str:
        """
        Translate into a DFA.

        :param start: instant of evaluation in the trace.
        :param end: last instant of evaluation in the trace.
        :param mona_dfa_out: flag for DFA output in MONA syntax.
        """
        return to_dfa(self, start, end, mona_dfa_out)


class QLTLfExist(LTLfUnaryOperator):
    """Class for the QLTLf Exist formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.EXIST.value

    # def to_nnf(self) -> QLTLfFormula:
    #     """Transform to NNF."""
    #     return QLTLfForAll(self.f.to_nnf().negate())

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return QLTLfForAll(self.f.negate())

    def to_mona(self, v="0", w="j") -> str:
        """Return the MONA encoding of an QLTLf Exist formula."""
        ex_var = new_var(w, False)
        if v == "0" and w == "j":
            return "(ex1 {0}, {2}: {1}<={0} & {0}<={2} & {3})".format(
                ex_var, v, w, self.f.to_mona(v, ex_var)
            )
        else:
            return "(ex1 {0}: {1}<={0} & {0}<={2} & {3})".format(
                ex_var, v, w, self.f.to_mona(v, ex_var)
            )


class QLTLfForAll(LTLfUnaryOperator):
    """Class for the QLTLf QLTLfForAll formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.FORALL.value

    # def to_nnf(self) -> QLTLfFormula:
    #     """Transform to NNF."""
    #     return QLTLfExist(self.f.to_nnf().negate())

    def negate(self) -> LTLfFormula:
        """Negate the formula."""
        return QLTLfExist(self.f.negate())

    def to_mona(self, v="0", w="j") -> str:
        """Return the MONA encoding of an QLTLf Forall formula."""
        ex_var = new_var(w, False)
        if v == "0" and w == "j":
            return "(all1 {0}, {2}: {1}<={0} & {0}<={2} => {3})".format(
                ex_var, v, w, self.f.to_mona(v, ex_var)
            )
        else:
            return "(all1 {0}: {1}<={0} & {0}<={2} => {3})".format(
                ex_var, v, w, self.f.to_mona(v, ex_var)
            )
