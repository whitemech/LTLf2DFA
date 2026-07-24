#!/usr/bin/env python3
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
"""Implementation of the LTLf parser."""

from lark import Lark, Transformer

from ltlf2dfa.helpers import ParsingError, check_
from ltlf2dfa.ltlf import (
    LTLfAlways,
    LTLfAnd,
    LTLfAtomic,
    LTLfEquivalence,
    LTLfEventually,
    LTLfFalse,
    LTLfImplies,
    LTLfLast,
    LTLfNext,
    LTLfNot,
    LTLfOr,
    LTLfRelease,
    LTLfTrue,
    LTLfUntil,
    LTLfWeakNext,
)
from ltlf2dfa.parser import LTLF_GRAMMAR_FILE, PARSERS_DIRECTORY
from ltlf2dfa.parser.pl import PLTransformer


class LTLfTransformer(Transformer):
    """LTLf Transformer."""

    def __init__(self):
        """Initialize."""
        super().__init__()
        self._pl_transformer = PLTransformer()

    def start(self, args):
        """Entry point."""
        check_(len(args) == 1)
        return args[0]

    def ltlf_formula(self, args):
        """Parse LTLf formula."""
        check_(len(args) == 1)
        return args[0]

    def ltlf_equivalence(self, args):
        """Parse LTLf Equivalence."""
        if len(args) == 1:
            return args[0]
        if (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LTLfEquivalence(subformulas)
        raise ParsingError

    def ltlf_implication(self, args):
        """Parse LTLf Implication."""
        if len(args) == 1:
            return args[0]
        if (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LTLfImplies(subformulas)
        raise ParsingError

    def ltlf_or(self, args):
        """Parse LTLf Or."""
        if len(args) == 1:
            return args[0]
        if (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LTLfOr(subformulas)
        raise ParsingError

    def ltlf_and(self, args):
        """Parse LTLf And."""
        if len(args) == 1:
            return args[0]
        if (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LTLfAnd(subformulas)
        raise ParsingError

    def ltlf_until(self, args):
        """Parse LTLf Until."""
        if len(args) == 1:
            return args[0]
        if (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LTLfUntil(subformulas)
        raise ParsingError

    def ltlf_release(self, args):
        """Parse LTLf Release."""
        if len(args) == 1:
            return args[0]
        if (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LTLfRelease(subformulas)
        raise ParsingError

    def ltlf_always(self, args):
        """Parse LTLf Always."""
        if len(args) == 1:
            return args[0]
        f = args[-1]
        for _ in args[:-1]:
            f = LTLfAlways(f)
        return f

    def ltlf_eventually(self, args):
        """Parse LTLf Eventually."""
        if len(args) == 1:
            return args[0]
        f = args[-1]
        for _ in args[:-1]:
            f = LTLfEventually(f)
        return f

    def ltlf_next(self, args):
        """Parse LTLf Next."""
        if len(args) == 1:
            return args[0]
        f = args[-1]
        for _ in args[:-1]:
            f = LTLfNext(f)
        return f

    def ltlf_weak_next(self, args):
        """Parse LTLf Weak Next."""
        if len(args) == 1:
            return args[0]
        f = args[-1]
        for _ in args[:-1]:
            f = LTLfWeakNext(f)
        return f

    def ltlf_not(self, args):
        """Parse LTLf Not."""
        if len(args) == 1:
            return args[0]
        f = args[-1]
        for _ in args[:-1]:
            f = LTLfNot(f)
        return f

    def ltlf_wrapped(self, args):
        """Parse LTLf wrapped formula."""
        if len(args) == 1:
            return args[0]
        if len(args) == 3:
            _, formula, _ = args
            return formula
        raise ParsingError

    def ltlf_atom(self, args):
        """Parse LTLf Atom."""
        check_(len(args) == 1)
        return args[0]

    def ltlf_true(self, _args):
        """Parse LTLf True."""
        return LTLfTrue()

    def ltlf_false(self, _args):
        """Parse LTLf False."""
        return LTLfFalse()

    def ltlf_last(self, _args):
        """Parse LTLf Last."""
        return LTLfLast()

    # def ltlf_end(self, _args):
    #     raise NotImplementedError("LTLf end not supported, yet")

    def ltlf_symbol(self, args):
        """Parse LTLf Symbol."""
        check_(len(args) == 1)
        token = args[0]
        symbol = str(token)
        return LTLfAtomic(symbol)


_ltlf_parser_lark = LTLF_GRAMMAR_FILE.read_text()


class LTLfParser:
    """LTLf Parser class."""

    def __init__(self):
        """Initialize."""
        self._transformer = LTLfTransformer()
        self._parser = Lark(_ltlf_parser_lark, parser="lalr", import_paths=[PARSERS_DIRECTORY])

    def __call__(self, text):
        """Call."""
        tree = self._parser.parse(text)
        formula = self._transformer.transform(tree)
        return formula
