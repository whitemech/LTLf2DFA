# -*- coding: utf-8 -*-
"""Implementation of the PLTLf parser."""

from pathlib import Path

from lark import Lark, Transformer

from ltlf2dfa.helpers import ParsingError
from ltlf2dfa.pltlf import (
    PLTLfEquivalence,
    PLTLfImplies,
    PLTLfOr,
    PLTLfAnd,
    PLTLfNot,
    PLTLfSince,
    PLTLfHistorically,
    PLTLfOnce,
    PLTLfBefore,
    PLTLfAtomic,
    PLTLfTrue,
    PLTLfFalse,
    PLTLfLast,
)
from ltlf2dfa.parser import CUR_DIR
from ltlf2dfa.parser.pl import PLTransformer


class PLTLfTransformer(Transformer):
    """PLTLf Transformer."""

    def __init__(self):
        """Initialize."""
        super().__init__()
        self._pl_transformer = PLTransformer()

    def start(self, args):
        """Entry point."""
        assert len(args) == 1
        return args[0]

    def pltlf_formula(self, args):
        """Parse PLTLf formula."""
        assert len(args) == 1
        return args[0]

    def pltlf_equivalence(self, args):
        """Parse PLTLf Equivalence."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PLTLfEquivalence(subformulas)
        else:
            raise ParsingError

    def pltlf_implication(self, args):
        """Parse PLTLf Implication."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PLTLfImplies(subformulas)
        else:
            raise ParsingError

    def pltlf_or(self, args):
        """Parse PLTLf Or."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PLTLfOr(subformulas)
        else:
            raise ParsingError

    def pltlf_and(self, args):
        """Parse PLTLf And."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PLTLfAnd(subformulas)
        else:
            raise ParsingError

    def pltlf_since(self, args):
        """Parse PLTLf Since."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return PLTLfSince(subformulas)
        else:
            raise ParsingError

    def pltlf_historically(self, args):
        """Parse PLTLf Historically."""
        if len(args) == 1:
            return args[0]
        else:
            f = args[-1]
            for _ in args[:-1]:
                f = PLTLfHistorically(f)
            return f

    def pltlf_once(self, args):
        """Parse PLTLf Once."""
        if len(args) == 1:
            return args[0]
        else:
            f = args[-1]
            for _ in args[:-1]:
                f = PLTLfOnce(f)
            return f

    def pltlf_before(self, args):
        """Parse PLTLf Before."""
        if len(args) == 1:
            return args[0]
        else:
            f = args[-1]
            for _ in args[:-1]:
                f = PLTLfBefore(f)
            return f

    def pltlf_not(self, args):
        """Parse PLTLf Not."""
        if len(args) == 1:
            return args[0]
        else:
            f = args[-1]
            for _ in args[:-1]:
                f = PLTLfNot(f)
            return f

    def pltlf_wrapped(self, args):
        """Parse PLTLf wrapped formula."""
        if len(args) == 1:
            return args[0]
        elif len(args) == 3:
            _, formula, _ = args
            return formula
        else:
            raise ParsingError

    def pltlf_atom(self, args):
        """Parse PLTLf Atom."""
        assert len(args) == 1
        return args[0]

    def pltlf_true(self, args):
        """Parse PLTLf True."""
        return PLTLfTrue()

    def pltlf_false(self, args):
        """Parse PLTLf False."""
        return PLTLfFalse()

    def pltlf_last(self, args):
        """Parse PLTLf Last."""
        return PLTLfLast()

    # def pltlf_end(self, args):
    #     raise NotImplementedError("PLTLf end not supported, yet")

    def pltlf_symbol(self, args):
        """Parse PLTLf Symbol."""
        assert len(args) == 1
        token = args[0]
        symbol = str(token)
        return PLTLfAtomic(symbol)


class PLTLfParser:
    """PLTLf Parser class."""

    def __init__(self):
        """Initialize."""
        self._transformer = PLTLfTransformer()
        self._parser = Lark(open(str(Path(CUR_DIR, "pltlf.lark"))), parser="lalr")

    def __call__(self, text):
        """Call."""
        tree = self._parser.parse(text)
        formula = self._transformer.transform(tree)
        return formula


if __name__ == "__main__":
    parser = PLTLfParser()
    while True:
        try:
            s = input("pltlf > ")
            if not s:
                continue
            result = parser(s)
            print("result:", result, type(result))
        except EOFError:
            break
        except Exception as e:
            print(str(e))
