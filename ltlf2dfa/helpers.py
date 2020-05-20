# -*- coding: utf-8 -*-

"""Helpers module."""

from abc import ABC, abstractmethod
from copy import copy

# from itertools import chain, combinations
# from typing import Iterable, Set, FrozenSet, List

# from pythomata import PropositionalInterpretation as PropInt
# from sympy import Symbol
# from sympy.logic.boolalg import Boolean, BooleanFalse, BooleanTrue

from ltlf2dfa.symbols import Symbols

ParsingError = ValueError("Parsing error.")


class Hashable(ABC):
    """A base class to represent hashable objects."""

    def __init__(self):
        """Initialize."""
        self._hash = None

    @abstractmethod
    def _members(self):
        raise NotImplementedError

    def __eq__(self, other):
        """Compare."""
        if type(other) is type(self):
            return self._members() == other._members()
        else:
            return False

    def __hash__(self):
        """Compute the hash."""
        if self._hash is None:
            members = self._members()
            self._hash = hash(members)
        return self._hash

    def __getstate__(self):
        """Get the state."""
        d = copy(self.__dict__)
        d.pop("_hash")
        return d

    def __setstate__(self, state):
        """Set the state."""
        self.__dict__ = state
        self._hash = None


class Wrapper(Hashable):
    """Wrap other objects and expose the same interface.

    This helper class can be subclassed to create a constant view on wrapped
    objects, exposing the same interface.
    This is an immutable object: either add members to _mutable list, or
    modify them through __dict__.
    """

    _mutable = ["_hash"]

    def __init__(self, obj):
        """Initialize: save the wrapped object."""
        super().__init__()
        self.__dict__["_Wrapper__obj"] = obj

    def __str__(self):
        """Just forward to obj."""
        return str(self.__obj)

    def __repr__(self):
        """Just forward to obj."""
        return repr(self.__obj)

    def __getattr__(self, attr):
        """Redirect to obj."""
        return getattr(self.__obj, attr)

    def __setattr__(self, attr, value):
        """If immutable, raises an error."""
        if attr in self._mutable:
            self.__dict__[attr] = value
        else:
            raise AttributeError("Can't modify: immutable object.")

    def __delattr__(self, attr):
        """Raise an error, because del is not supported."""
        raise AttributeError("Can't modify: immutable object.")

    def __dir__(self):
        """Expose the same interface as wrapped."""
        members = set(dir(self.__obj)).union(object.__dir__(self))
        return sorted(members)

    def _members(self):
        return self.__obj

    @property
    def wrapped(self):
        """Return the wrapped object."""
        return self.__obj


# def powerset(s: Set) -> FrozenSet:
#     """
#     Compute the power set of a set.
#
#     >>> sorted(map(tuple, powerset({1,2,3})))
#     [(), (1,), (1, 2), (1, 2, 3), (1, 3), (2,), (2, 3), (3,)]
#
#
#     :param s: the set of elements on which to compute the power set.
#     :return: the power set of the set provided in input.
#     """
#     combs = iter_powerset(s)
#     res = frozenset(frozenset(x) for x in combs)
#     return res


# def iter_powerset(s: Set) -> Iterable:
#     """
#     Compute the generative version of the power set function.
#
#     >>> sorted(map(tuple, powerset({1,2,3})))
#     [(), (1,), (1, 2), (1, 2, 3), (1, 3), (2,), (2, 3), (3,)]
#
#     :param s: the set of elements on which to compute the power set.
#     :return: the generator that generates the poweset.
#     """
#     s_list = list(s)  # type: List
#     combs = chain.from_iterable(combinations(s_list, r) for r in range(len(s_list) + 1))
#     for c in combs:
#         yield c


def new_var(prev_var: str) -> str:
    """Compute next variable."""
    if prev_var == "0" or prev_var == "max($)":
        return "v_1"
    else:
        s = prev_var.split("_")
        s[1] = str(int(s[1]) + 1)
        return "_".join(s)


def sym2regexp(sym: Symbols):
    """Transform a symbol to regex."""
    s = sym.value
    if s in r"|()+?*.[]":
        return r"\%s" % s
    else:
        return s


# def evaluate(formula: Boolean, i: PropInt) -> bool:
#     """
#     Evaluate a SymPy boolean expression against a propositional interpretation.
#
#     The symbols not present in the propositional interpretation will be considered False.
#
#     >>> from sympy.parsing.sympy_parser import parse_expr
#     >>> evaluate(parse_expr("a & b"), {"a": True})
#     False
#
#     >>> evaluate(parse_expr("a | b"), {"b": True})
#     True
#
#     >>> evaluate(parse_expr("a"), {"b": True})
#     False
#
#     :param formula: a sympy.logic.boolalg.Boolean.
#     :param i: a propositional interpretation,
#               i.e. a mapping from symbol identifiers to True/False
#     :return: True if the formula is true in the interpretation, False o/w.
#     """
#     return formula.subs(i).replace(Symbol, BooleanFalse) == BooleanTrue()


MAX_CACHE_SIZE = 1024
