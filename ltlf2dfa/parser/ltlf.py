# -*- coding: utf-8 -*-
"""Implementation of the LTLf parser."""

from pathlib import Path

from lark import Lark, Transformer

from ltlf2dfa.helpers import ParsingError
from ltlf2dfa.ltlf import (
    LTLfEquivalence,
    LTLfImplies,
    LTLfOr,
    LTLfAnd,
    LTLfNot,
    LTLfUntil,
    LTLfRelease,
    LTLfAlways,
    LTLfEventually,
    LTLfNext,
    LTLfWeakNext,
    LTLfTrue,
    LTLfAtomic,
    LTLfFalse,
    LTLfLast,
)
from ltlf2dfa.parser import CUR_DIR
from ltlf2dfa.parser.pl import PLTransformer


class LTLfTransformer(Transformer):
    """LTLf Transformer."""

    def __init__(self):
        """Initialize."""
        super().__init__()
        self._pl_transformer = PLTransformer()

    def start(self, args):
        """Entry point."""
        assert len(args) == 1
        return args[0]

    def ltlf_formula(self, args):
        """Parse LTLf formula."""
        assert len(args) == 1
        return args[0]

    def ltlf_equivalence(self, args):
        """Parse LTLf Equivalence."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LTLfEquivalence(subformulas)
        else:
            raise ParsingError

    def ltlf_implication(self, args):
        """Parse LTLf Implication."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LTLfImplies(subformulas)
        else:
            raise ParsingError

    def ltlf_or(self, args):
        """Parse LTLf Or."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LTLfOr(subformulas)
        else:
            raise ParsingError

    def ltlf_and(self, args):
        """Parse LTLf And."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LTLfAnd(subformulas)
        else:
            raise ParsingError

    def ltlf_until(self, args):
        """Parse LTLf Until."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LTLfUntil(subformulas)
        else:
            raise ParsingError

    def ltlf_release(self, args):
        """Parse LTLf Release."""
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LTLfRelease(subformulas)
        else:
            raise ParsingError

    def ltlf_always(self, args):
        """Parse LTLf Always."""
        if len(args) == 1:
            return args[0]
        else:
            f = args[-1]
            for _ in args[:-1]:
                f = LTLfAlways(f)
            return f

    def ltlf_eventually(self, args):
        """Parse LTLf Eventually."""
        if len(args) == 1:
            return args[0]
        else:
            f = args[-1]
            for _ in args[:-1]:
                f = LTLfEventually(f)
            return f

    def ltlf_next(self, args):
        """Parse LTLf Next."""
        if len(args) == 1:
            return args[0]
        else:
            f = args[-1]
            for _ in args[:-1]:
                f = LTLfNext(f)
            return f

    def ltlf_weak_next(self, args):
        """Parse LTLf Weak Next."""
        if len(args) == 1:
            return args[0]
        else:
            f = args[-1]
            for _ in args[:-1]:
                f = LTLfWeakNext(f)
            return f

    def ltlf_not(self, args):
        """Parse LTLf Not."""
        if len(args) == 1:
            return args[0]
        else:
            f = args[-1]
            for _ in args[:-1]:
                f = LTLfNot(f)
            return f

    def ltlf_wrapped(self, args):
        """Parse LTLf wrapped formula."""
        if len(args) == 1:
            return args[0]
        elif len(args) == 3:
            _, formula, _ = args
            return formula
        else:
            raise ParsingError

    def ltlf_atom(self, args):
        """Parse LTLf Atom."""
        assert len(args) == 1
        return args[0]

    def ltlf_true(self, args):
        """Parse LTLf True."""
        return LTLfTrue()

    def ltlf_false(self, args):
        """Parse LTLf False."""
        return LTLfFalse()

    def ltlf_last(self, args):
        """Parse LTLf Last."""
        return LTLfLast()

    # def ltlf_end(self, args):
    #     raise NotImplementedError("LTLf end not supported, yet")

    def ltlf_symbol(self, args):
        """Parse LTLf Symbol."""
        assert len(args) == 1
        token = args[0]
        symbol = str(token)
        return LTLfAtomic(symbol)


class LTLfParser:
    """LTLf Parser class."""

    def __init__(self):
        """Initialize."""
        self._transformer = LTLfTransformer()
        self._parser = Lark(open(str(Path(CUR_DIR, "ltlf.lark"))), parser="lalr")

    def __call__(self, text):
        """Call."""
        tree = self._parser.parse(text)
        formula = self._transformer.transform(tree)
        return formula


if __name__ == "__main__":
    parser = LTLfParser()
    while True:
        try:
            s = input("ltlf > ")
            if not s:
                continue
            result = parser(s)
            print("result:", result, type(result))
        except EOFError:
            break
        except Exception as e:
            print(str(e))
