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

"""Helpers module."""

from abc import ABC, abstractmethod
from copy import copy

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


MAX_CACHE_SIZE = 1024
