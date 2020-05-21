# -*- coding: utf-8 -*-

"""This module contains the implementation of Past Linear Temporal Logic on finite traces."""

from abc import abstractmethod, ABC
from typing import Set, Optional, Any
import re

from ltlf2dfa.base import (
    Formula,
    AtomicFormula,
    UnaryOperator,
    BinaryOperator,
    AtomSymbol,
)
from ltlf2dfa.ltlf2dfa import to_dfa
from ltlf2dfa.pl import PLAtomic
from ltlf2dfa.symbols import Symbols, OpSymbol
from ltlf2dfa.helpers import new_var


class PLTLfFormula(Formula, ABC):
    """A class for the PLTLf formula."""

    def to_nnf(self) -> "PLTLfFormula":
        """Convert an PLTLf formula in NNF."""
        return self

    @abstractmethod
    def negate(self) -> "PLTLfFormula":
        """Negate the formula."""

    def __repr__(self):
        """Get the representation."""
        return self.__str__()

    def to_mona(self, v: Optional[Any] = None) -> str:
        """
        Tranform the PLTLf formula into its encoding in MONA.

        :return: a string.
        """

    # def to_ldlf(self):
    #     """
    #     Tranform the formula into an equivalent LDLf formula.
    #
    #     :return: an LDLf formula.
    #     """

    def to_dfa(self) -> str:
        """Translate into a DFA."""
        return to_dfa(self)


class PLTLfUnaryOperator(UnaryOperator[PLTLfFormula], PLTLfFormula, ABC):
    """A unary operator for PLTLf."""


class PLTLfBinaryOperator(BinaryOperator[PLTLfFormula], PLTLfFormula, ABC):
    """A binary operator for PLTLf."""


class PLTLfAtomic(AtomicFormula, PLTLfFormula):
    """Class for PLTLf atomic formulas."""

    name_regex = re.compile(r"[a-z][a-z0-9_]*")

    def negate(self):
        """Negate the formula."""
        return PLTLfNot(self)

    def find_labels(self) -> Set[AtomSymbol]:
        """Find the labels."""
        return PLAtomic(self.s).find_labels()

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PLTLf atomic formula."""
        if v != "max($)":
            return "({} in {})".format(v, self.s.upper())
        else:
            return PLAtomic(self.s).to_mona()

    # def to_ldlf(self):
    #     return LDLfPropositional(PLAtomic(self.s)).convert()


class PLTLfTrue(PLTLfAtomic):
    """Class for the PLTLf True formula."""

    def __init__(self):
        """Initialize the formula."""
        super().__init__(Symbols.TRUE.value)

    def negate(self):
        """Negate the formula."""
        return PLTLfFalse()

    def find_labels(self) -> Set[AtomSymbol]:
        """Find the labels."""
        return set()

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding for True."""
        return Symbols.TRUE.value


class PLTLfFalse(PLTLfAtomic):
    """Class for the PLTLf False formula."""

    def __init__(self):
        """Initialize the formula."""
        super().__init__(Symbols.FALSE.value)

    def negate(self):
        """Negate the formula."""
        return PLTLfTrue()

    def find_labels(self) -> Set[AtomSymbol]:
        """Find the labels."""
        return set()

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding for False."""
        return Symbols.FALSE.value


class PLTLfNot(PLTLfUnaryOperator):
    """Class for the PLTLf not formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.NOT.value

    def to_nnf(self) -> PLTLfFormula:
        """Transform to NNF."""
        if not isinstance(self.f, AtomicFormula):
            return self.f.negate().to_nnf()
        else:
            return self

    def negate(self) -> PLTLfFormula:
        """Negate the formula."""
        return self.f

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PLTLf Not formula."""
        return "~({})".format(self.f.to_mona(v))

    # def to_ldlf(self):
    #     return LDLfNot(self.f.to_ldlf())


class PLTLfAnd(PLTLfBinaryOperator):
    """Class for the PLTLf And formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.AND.value

    def negate(self) -> PLTLfFormula:
        """Negate the formula."""
        return PLTLfOr([f.negate() for f in self.formulas])

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PLTLf And formula."""
        return "({})".format(" & ".join([f.to_mona(v) for f in self.formulas]))

    # def to_ldlf(self):
    #     return LDLfAnd([f.to_ldlf() for f in self.formulas])


class PLTLfOr(PLTLfBinaryOperator):
    """Class for the PLTLf Or formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.OR.value

    def negate(self) -> PLTLfFormula:
        """Negate the formula."""
        return PLTLfAnd([f.negate() for f in self.formulas])

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PLTLf Or formula."""
        return "({})".format(" | ".join([f.to_mona(v) for f in self.formulas]))


class PLTLfImplies(PLTLfBinaryOperator):
    """Class for the PLTLf Implication formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.IMPLIES.value

    def negate(self) -> PLTLfFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def to_nnf(self) -> PLTLfFormula:
        """Transform to NNF."""
        first, second = self.formulas[0:2]
        final_formula = PLTLfOr([PLTLfNot(first).to_nnf(), second.to_nnf()])
        for subformula in self.formulas[2:]:
            final_formula = PLTLfOr(
                [PLTLfNot(final_formula).to_nnf(), subformula.to_nnf()]
            )
        return final_formula

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PLTLf Implication formula."""
        return self.to_nnf().to_mona(v)


class PLTLfEquivalence(PLTLfBinaryOperator):
    """Class for the PLTLf Equivalente formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.EQUIVALENCE.value

    def to_nnf(self) -> PLTLfFormula:
        """Transform to NNF."""
        fs = self.formulas
        pos = PLTLfAnd(fs)
        neg = PLTLfAnd([PLTLfNot(f) for f in fs])
        res = PLTLfOr([pos, neg]).to_nnf()
        return res

    def negate(self) -> PLTLfFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PLTLf Equivalence formula."""
        return self.to_nnf().to_mona(v)


class PLTLfBefore(PLTLfUnaryOperator):
    """Class for the PLTLf Before formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.BEFORE.value

    def to_nnf(self) -> PLTLfFormula:
        """Transform to NNF."""
        return PLTLfBefore(self.f.to_nnf())

    def negate(self) -> PLTLfFormula:
        """Negate the formula."""
        return PLTLfNot(self.f.negate())

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PLTLf Before formula."""
        ex_var = new_var(v)
        if v != "max($)":
            return "(ex1 {0}: {0}={1}-1 & {0}>=0 & {2})".format(
                ex_var, v, self.f.to_mona(ex_var)
            )
        else:
            return "(ex1 {0}: {0}=max($)-1 & max($)>0 & {1})".format(
                ex_var, self.f.to_mona(ex_var)
            )

    # def to_ldlf(self):
    #     return LDLfDiamond(
    #         RegExpPropositional(PLTrue()),
    #         LDLfAnd([self.f.to_ldlf(), LDLfNot(LDLfEnd())]),
    #     )


class PLTLfSince(PLTLfBinaryOperator):
    """Class for the PLTLf Since formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.SINCE.value

    def to_nnf(self):
        """Transform to NNF."""
        return PLTLfSince([f.to_nnf() for f in self.formulas])

    def negate(self):
        """Negate the formula."""
        return PLTLfNot([f.negate() for f in self.formulas])

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PLTLf Since formula."""
        ex_var = new_var(v)
        all_var = new_var(ex_var)
        f1 = self.formulas[0].to_mona(v=all_var)
        f2 = (
            PLTLfSince(self.formulas[1:]).to_mona(v=ex_var)
            if len(self.formulas) > 2
            else self.formulas[1].to_mona(v=ex_var)
        )
        if v != "max($)":
            return (
                "(ex1 {0}: 0<={0}&{0}<={1} & {2} & "
                "(all1 {3}: {0}<{3}&{3}<={1} => {4}))".format(
                    ex_var, v, f2, all_var, f1
                )
            )
        else:
            return (
                "(ex1 {0}: 0<={0}&{0}<=max($) & {1} & "
                "(all1 {2}: {0}<{2}&{2}<=max($) => {3}))".format(
                    ex_var, f2, all_var, f1
                )
            )

    # def to_ldlf(self):
    #     f1 = self.formulas[0].to_ldlf()
    #     f2 = (
    #         PLTLfSince(self.formulas[1:]).to_ldlf()
    #         if len(self.formulas) > 2
    #         else self.formulas[1].to_ldlf()
    #     )
    #     return LDLfDiamond(
    #         RegExpStar(RegExpSequence([RegExpTest(f1), RegExpPropositional(PLTrue())])),
    #         LDLfAnd([f2, LDLfNot(LDLfEnd())]),
    #     )


class PLTLfOnce(PLTLfUnaryOperator):
    """Class for the PLTLf Once formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.ONCE.value

    def to_nnf(self) -> PLTLfFormula:
        """Transform to NNF."""
        return PLTLfSince([PLTLfTrue(), self.f])

    def negate(self) -> PLTLfFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PLTLf Once formula."""
        return PLTLfSince([PLTLfTrue(), self.f]).to_mona(v)

    # def to_ldlf(self):
    #     return LDLfDiamond(
    #         RegExpStar(RegExpPropositional(PLTrue())),
    #         LDLfAnd([self.f.to_ldlf(), LDLfNot(LDLfEnd())]),
    #     )


class PLTLfHistorically(PLTLfUnaryOperator):
    """Class for the PLTLf Historically formula."""

    @property
    def operator_symbol(self) -> OpSymbol:
        """Get the operator symbol."""
        return Symbols.HISTORICALLY.value

    # def to_nnf(self) -> PLTLfFormula:
    #     """Transform to NNF."""
    #     return PLTLfRelease([PLTLfFalse(), self.f.to_nnf()])

    def negate(self) -> PLTLfFormula:
        """Negate the formula."""
        return self.to_nnf().negate()

    def to_mona(self, v="max($)") -> str:
        """Return the MONA encoding of a PLTLf Historically formula."""
        return PLTLfNot(PLTLfOnce(PLTLfNot(self.f))).to_mona(v)


class PLTLfLast(PLTLfFormula):
    """Class for the PLTLf Last formula."""

    # def to_nnf(self) -> PLTLfFormula:
    #     """Transform to NNF."""
    #     return PLTLfAnd([PLTLfWeakBefore(PLTLfFalse()), PLTLfNot(PLTLfEnd())]).to_nnf()

    def negate(self) -> PLTLfFormula:
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


# class PLTLfEnd(PLTLfFormula):
#     """Class for the PLTLf End formula."""
#
#     def find_labels(self) -> Set[AtomSymbol]:
#         """Find the labels."""
#         return set()
#
#     def _members(self):
#         return (Symbols.END.value,)
#
#     def to_nnf(self) -> PLTLfFormula:
#         """Transform to NNF."""
#         return PLTLfHistorically(PLTLfFalse()).to_nnf()
#
#     def negate(self) -> PLTLfFormula:
#         """Negate the formula."""
#         return self.to_nnf().negate()
#
#     def __str__(self):
#         """Get the string representation."""
#         return "_".join(map(str, self._members()))
