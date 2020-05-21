# -*- coding: utf-8 -*-
"""Small helper class to check the correct parsing."""

from lark import Lark
from lark.lexer import Token
from typing import List, Tuple, Optional


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
