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

"""This module contains the definition to deal with symbols."""
from enum import Enum
from typing import Set

OpSymbol = str


class Symbols(Enum):
    """A set of symbols that can be used in a logical formula."""

    NOT = "!"
    AND = "&"
    OR = "|"
    EQUAL = "="
    IMPLIES = "->"
    EQUIVALENCE = "<->"
    NEXT = "X"
    WEAK_NEXT = "WX"
    UNTIL = "U"
    RELEASE = "R"
    EVENTUALLY = "F"
    ALWAYS = "G"
    BEFORE = "Y"
    WEAK_BEFORE = "WY"
    ONCE = "O"
    SINCE = "S"
    PAST_RELEASE = "P"
    HISTORICALLY = "H"
    ROUND_BRACKET_LEFT = "("
    ROUND_BRACKET_RIGHT = ")"
    LAST = "last"
    START = "start"
    END = "end"
    TRUE = "true"
    FALSE = "false"


ALL_SYMBOLS = {v.value for v in Symbols}  # type: Set[str]
