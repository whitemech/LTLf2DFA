# -*- coding: utf-8 -*-
"""Test the Propositional Logic."""
import os
import pytest
import lark

from ltlf2dfa.parser.pl import PLParser
from ltlf2dfa.pl import (
    PLAnd,
    PLAtomic,
    PLNot,
    PLEquivalence,
    PLOr,
    PLImplies,
    PLFalse,
    PLTrue,
)
from .parsing import ParsingCheck


def test_parser():
    parser = PLParser()
    sa, sb = "A", "B"
    a, b = PLAtomic(sa), PLAtomic(sb)

    a_and_b = parser("A & B")
    true_a_and_b = PLAnd([a, b])
    assert a_and_b == true_a_and_b

    material_implication = parser("!A | B <-> !(A & !B) <-> A->B")
    true_material_implication = PLEquivalence(
        [PLOr([PLNot(a), b]), PLNot(PLAnd([a, PLNot(b)])), PLImplies([a, b])]
    )

    assert material_implication == true_material_implication

    true_a_and_false_and_true = PLAnd([a, PLFalse(), PLTrue()])
    a_and_false_and_true = parser("A & false & true")

    assert a_and_false_and_true == true_a_and_false_and_true


def test_nnf():
    parser = PLParser()
    sa, sb = "A", "B"
    a, b = PLAtomic(sa), PLAtomic(sb)

    not_a_and_b = parser("!(A&B)")
    nnf_not_a_and_b = parser("!A | !B")
    assert not_a_and_b.to_nnf() == nnf_not_a_and_b
    assert nnf_not_a_and_b == nnf_not_a_and_b.to_nnf()

    dup = parser("!(A | A)")
    nnf_dup = dup.to_nnf()
    assert nnf_dup == PLAnd([PLNot(a), PLNot(a)])

    material_implication = parser("!A | B <-> !(A & !B) <-> A->B")
    nnf_material_implication = parser(
        "((!A | B) & (!A | B) & (!A | B)) | ((A & !B) & (A & !B) & (A & !B))"
    )
    nnf_m = material_implication.to_nnf()
    assert nnf_m == nnf_material_implication.to_nnf()


def test_find_labels():
    parser = PLParser()

    # complete formula
    f = "!A | B <-> !(A & !B) <-> A->B"
    formula = parser(f)
    assert formula.find_labels() == {"A", "B"}

    # more than one character
    f = "!A & (!AB & !A0)"
    formula = parser(f)
    assert formula.find_labels() == {c for c in {"A", "AB", "A0"}}


def test_names():

    good = [
        "A",
        "b",
        "Hello",
        "PropZero",
        "Prop0",
        "this_is_fine_2",
        '"This is also allowed!"',
        PLParser()("A -> B"),
    ]
    bad = ["!", "&", "Invalid:", "", '"', "="]

    for name in good:
        PLAtomic(name)
    for name in bad:
        with pytest.raises(ValueError):
            PLAtomic(name)


class TestParsingTree:
    """
    The parsing tree should give the right priority to the operators.
    """

    @classmethod
    def setup_class(cls):

        # Path to grammar
        this_path = os.path.dirname(os.path.abspath(__file__))
        grammar_path = "../ltlf2dfa/parser/pl.lark"
        grammar_path = os.path.join(this_path, *grammar_path.split("/"))

        cls.checker = ParsingCheck(grammar_path)

    def test_unary(self):

        ok, err = self.checker.precedence_check("!a & b", list("&!ab"))
        assert ok, err

        ok, err = self.checker.precedence_check("a & !b", list("&a!b"))
        assert ok, err

        ok, err = self.checker.precedence_check("a | !b", list("|a!b"))
        assert ok, err

    def test_and_or(self):

        ok, err = self.checker.precedence_check("a & b & c", list("&&abc"))
        assert ok, err

        ok, err = self.checker.precedence_check("a & b | c", list("|&abc"))
        assert ok, err

        ok, err = self.checker.precedence_check("a | b & c", list("|a&bc"))
        assert ok, err

    def test_implications(self):

        ok, err = self.checker.precedence_check(
            "a <-> b -> c3", "<->,a,->,b,c3".split(",")
        )
        assert ok, err

        ok, err = self.checker.precedence_check(
            "(a <-> b) -> c", "->,(,),<->,a,b,c".split(",")
        )
        assert ok, err

    def test_misc(self):

        ok, err = self.checker.precedence_check(
            "!a&(b->c)", "&,!,a,(,),->,b,c".split(",")
        )
        assert ok, err

    def test_bad_examples(self):

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("!a&", list("!a&"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("!&b", list("!&b"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("a|b|", list("a|b|"))
