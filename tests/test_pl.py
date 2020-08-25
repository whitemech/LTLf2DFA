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
"""Test the Propositional Logic."""
import os

import lark
import pytest

from ltlf2dfa.parser.pl import PLParser
from ltlf2dfa.pl import (
    PLAnd,
    PLAtomic,
    PLEquivalence,
    PLFalse,
    PLImplies,
    PLNot,
    PLOr,
    PLTrue,
)

from .parsing import ParsingCheck


def test_parser():
    parser = PLParser()
    sa, sb, sc = "A", "B", "C"
    a, b, c = PLAtomic(sa), PLAtomic(sb), PLAtomic(sc)
    parsed_a = parser("A")
    assert parsed_a == a

    parsed_b = parser("B")
    assert parsed_b == b

    parsed_not_a = parser("~A")
    assert parsed_not_a == PLNot(a)

    a_and_b = parser("A & B")
    true_a_and_b = PLAnd([a, b])
    assert a_and_b == true_a_and_b

    a_or_b = parser("A | B")
    true_a_or_b = PLOr([a, b])
    assert a_or_b == true_a_or_b

    material_implication = parser("!A | B <-> !(A & !B) <-> A->B")
    true_material_implication = PLEquivalence(
        [PLOr([PLNot(a), b]), PLNot(PLAnd([a, PLNot(b)])), PLImplies([a, b])]
    )
    assert material_implication == true_material_implication

    a_imply_b = parser("A -> B")
    true_a_imply_b = PLImplies([a, b])
    assert a_imply_b == true_a_imply_b

    a_imply_b_imply_c = parser("A -> B -> C")
    true_a_imply_b_imply_c = PLImplies([a, b, c])
    assert a_imply_b_imply_c == true_a_imply_b_imply_c

    true_a_and_false_and_true = PLAnd([a, PLFalse(), PLTrue()])
    a_and_false_and_true = parser("A & false & true")
    assert a_and_false_and_true == true_a_and_false_and_true


def test_negate():
    sa, sb, sc = "A", "B", "c"
    a, b, c = PLAtomic(sa), PLAtomic(sb), PLAtomic(sc)

    a_and_b = PLAnd([a, b])
    not_a_or_not_b = PLOr([PLNot(a), PLNot(b)])
    assert a_and_b.negate() == not_a_or_not_b

    a_and_b_and_c = PLAnd([a, b, c])
    not_a_or_not_b_or_not_c = PLOr([PLNot(a), PLNot(b), PLNot(c)])
    assert a_and_b_and_c.negate() == not_a_or_not_b_or_not_c

    a_or_b = PLOr([a, b])
    not_a_and_not_b = PLAnd([PLNot(a), PLNot(b)])
    assert a_or_b.negate() == not_a_and_not_b


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
    assert formula.find_labels() == ["A", "B"]

    # more than one character
    f = "!A & (!AB & !A0)"
    formula = parser(f)
    assert formula.find_labels() == [c for c in ["A", "AB", "A0"]]

    # another formula
    f = "!A | B <-> !(C & !B) <-> C->A"
    formula = parser(f)
    assert formula.find_labels() == ["A", "B", "C"]


def test_find_atomics():
    parser = PLParser()
    sa, sb, sab, sa0 = "A", "B", "AB", "A0"
    a, b, ab, a0 = PLAtomic(sa), PLAtomic(sb), PLAtomic(sab), PLAtomic(sa0)

    # complete formula
    f = "!A | B <-> !(A & !B) <-> A->B"
    formula = parser(f)
    assert formula.find_atomics() == {a, b}

    # more than one character
    f = "!A & (!AB & !A0)"
    formula = parser(f)
    assert formula.find_atomics() == {c for c in {a, ab, a0}}


def test_mona():
    # parser = PLParser()
    a, b, c = [PLAtomic(c) for c in "abc"]
    true = PLTrue()
    false = PLFalse()

    assert a.to_mona(v="0") == "(0 in A)"
    assert b.to_mona(v="0") == "(0 in B)"
    assert c.to_mona(v="0") == "(0 in C)"
    assert true.to_mona(v="0") == "true"
    assert false.to_mona(v="0") == "false"


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
