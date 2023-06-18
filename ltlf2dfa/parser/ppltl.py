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
"""Implementation of the PPLTL parser."""

from lark import Lark, Transformer

from ltlf2dfa.helpers import ParsingError, check_
from ltlf2dfa.parser import PARSERS_DIRECTORY, PPLTL_GRAMMAR_FILE
from ltlf2dfa.parser.pl import PLTransformer
from ltlf2dfa.ppltl import (
    PPLTLAnd,
    PPLTLAtomic,
    PPLTLBefore,
    PPLTLEquivalence,
    PPLTLFalse,
    PPLTLHistorically,
    PPLTLImplies,
    PPLTLNot,
    PPLTLOnce,
    PPLTLOr,
    PPLTLPastRelease,
    PPLTLSince,
    PPLTLStart,
    PPLTLTrue,
    PPLTLWeakBefore,
)


class PPLTLTransformer(Transformer):
    """PPLTL Transformer."""

    def __init__(self):
        """Initialize."""
        super().__init__()
        self._pl_transformer = PLTransformer()

    def start(self, args):
        """Entry point."""
        check_(len(args) == 1)
        return args[0]

    def ppltl_formula(self, args):
        """Parse PPLTL formula."""
        check_(len(args) == 1)
        return args[0]

    def ppltl_equivalence(self, args):
        """Parse PPLTL Equivalence."""
        if len(args) == 1:
            return args[0]
        if (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PPLTLEquivalence(subformulas)
        raise ParsingError

    def ppltl_implication(self, args):
        """Parse PPLTL Implication."""
        if len(args) == 1:
            return args[0]
        if (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PPLTLImplies(subformulas)
        raise ParsingError

    def ppltl_or(self, args):
        """Parse PPLTL Or."""
        if len(args) == 1:
            return args[0]
        if (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PPLTLOr(subformulas)
        raise ParsingError

    def ppltl_and(self, args):
        """Parse PPLTL And."""
        if len(args) == 1:
            return args[0]
        if (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PPLTLAnd(subformulas)
        raise ParsingError

    def ppltl_since(self, args):
        """Parse PPLTL Since."""
        if len(args) == 1:
            return args[0]
        if (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PPLTLSince(subformulas)
        raise ParsingError

    def ppltl_pastrelease(self, args):
        """Parse PPLTL Past Release."""
        if len(args) == 1:
            return args[0]
        if (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PPLTLPastRelease(subformulas)
        raise ParsingError

    def ppltl_historically(self, args):
        """Parse PPLTL Historically."""
        if len(args) == 1:
            return args[0]
        f = args[-1]
        for _ in args[:-1]:
            f = PPLTLHistorically(f)
        return f

    def ppltl_once(self, args):
        """Parse PPLTL Once."""
        if len(args) == 1:
            return args[0]
        f = args[-1]
        for _ in args[:-1]:
            f = PPLTLOnce(f)
        return f

    def ppltl_before(self, args):
        """Parse PPLTL Before."""
        if len(args) == 1:
            return args[0]
        f = args[-1]
        for _ in args[:-1]:
            f = PPLTLBefore(f)
        return f

    def ppltl_weak_before(self, args):
        """Parse PPLTL Weak Before."""
        if len(args) == 1:
            return args[0]
        f = args[-1]
        for _ in args[:-1]:
            f = PPLTLWeakBefore(f)
        return f

    def ppltl_not(self, args):
        """Parse PPLTL Not."""
        if len(args) == 1:
            return args[0]
        f = args[-1]
        for _ in args[:-1]:
            f = PPLTLNot(f)
        return f

    def ppltl_wrapped(self, args):
        """Parse PPLTL wrapped formula."""
        if len(args) == 1:
            return args[0]
        if len(args) == 3:
            _, formula, _ = args
            return formula
        raise ParsingError

    def ppltl_atom(self, args):
        """Parse PPLTL Atom."""
        check_(len(args) == 1)
        return args[0]

    def ppltl_true(self, _args):
        """Parse PPLTL True."""
        return PPLTLTrue()

    def ppltl_false(self, _args):
        """Parse PPLTL False."""
        return PPLTLFalse()

    def ppltl_start(self, _args):
        """Parse PPLTL Last."""
        return PPLTLStart()

    # def ppltl_end(self, args):
    #     raise NotImplementedError("PPLTL end not supported, yet")

    def ppltl_symbol(self, args):
        """Parse PPLTL Symbol."""
        check_(len(args) == 1)
        token = args[0]
        symbol = str(token)
        return PPLTLAtomic(symbol)


_ppltl_parser_lark = PPLTL_GRAMMAR_FILE.read_text()


class PPLTLParser:
    """PPLTL Parser class."""

    def __init__(self):
        """Initialize."""
        self._transformer = PPLTLTransformer()
        self._parser = Lark(
            _ppltl_parser_lark, parser="lalr", import_paths=[PARSERS_DIRECTORY]
        )

    def __call__(self, text):
        """Call."""
        tree = self._parser.parse(text)
        formula = self._transformer.transform(tree)
        return formula
