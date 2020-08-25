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
"""Small helper class to check the correct parsing."""

from typing import List, Optional, Tuple

from lark import Lark
from lark.lexer import Token  # type: ignore


class ParsingCheck:
    def __init__(self, lark):
        """\
        Constructor

        :param lark: path of a lark grammar file.
        """
        self.parser = Lark(open(lark), parser="lalr", debug=True)

    def precedence_check(
        self, formula: str, tokens: List[str], print_tree: bool = False
    ) -> Tuple[bool, Optional[str]]:
        """\
        Parse the formula and check that operators have the expected
        precedence.

        :param formula: expression to parse.
        :param tokens: a list of symbols (top-down, left-right) order in the
            parsing tree.
        :param print_tree: verbose parsing tree.

        :returns: false boolean, if parsing did not respect the order;
            and a string of error
        """

        # Parse
        tree = self.parser.parse(formula)
        topdown_it = tree.iter_subtrees_topdown()

        if print_tree:
            print(tree.pretty())

        # Navigate
        token_i = 0
        for elem in topdown_it:
            for child in elem.children:

                # Filter tokens and match
                if isinstance(child, Token):

                    if child != tokens[token_i]:
                        err = "Next expected '{}', got '{}'".format(
                            tokens[token_i], str(child)
                        )
                        return False, err
                    token_i += 1

        # Check length
        if token_i != len(tokens):
            return False, "The input was too short"

        return True, None
