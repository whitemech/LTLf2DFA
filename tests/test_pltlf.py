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
"""Test PLTLf."""
import os

import lark
import pytest

from ltlf2dfa.parser.pltlf import PLTLfParser
from ltlf2dfa.pltlf import (
    PLTLfAnd,
    PLTLfAtomic,
    PLTLfBefore,
    PLTLfEquivalence,
    PLTLfFalse,
    PLTLfHistorically,
    PLTLfImplies,
    PLTLfNot,
    PLTLfOnce,
    PLTLfOr,
    PLTLfPastRelease,
    PLTLfSince,
    PLTLfStart,
    PLTLfTrue,
    PLTLfWeakBefore,
)

# from .conftest import LTLfFixtures
from .parsing import ParsingCheck


def test_parser():
    parser = PLTLfParser()
    a, b, c = [PLTLfAtomic(c) for c in "abc"]

    assert parser("!a | b <-> !(a & !b) <-> a->b") == PLTLfEquivalence(
        [
            PLTLfOr([PLTLfNot(a), b]),
            PLTLfNot(PLTLfAnd([a, PLTLfNot(b)])),
            PLTLfImplies([a, b]),
        ]
    )

    assert parser("(Y a)") == PLTLfBefore(a)

    assert parser("a & O(b)") == PLTLfAnd([a, PLTLfOnce(b)])

    assert parser("(O (a&b)) <-> !(H (!a | !b) )") == PLTLfEquivalence(
        [
            PLTLfOnce(PLTLfAnd([a, b])),
            PLTLfNot(PLTLfHistorically(PLTLfOr([PLTLfNot(a), PLTLfNot(b)]))),
        ]
    )

    assert parser("(a S b S !c)") == PLTLfSince([a, b, PLTLfNot(c)])

    assert parser("a & start") == PLTLfAnd([a, PLTLfStart()])


def test_negate():
    parser = PLTLfParser()
    sa, sb, sc = "a", "b", "c"
    a, b, c = PLTLfAtomic(sa), PLTLfAtomic(sb), PLTLfAtomic(sc)

    a_and_b = PLTLfAnd([a, b])
    not_a_or_not_b = PLTLfOr([PLTLfNot(a), PLTLfNot(b)])
    assert a_and_b.negate() == not_a_or_not_b

    a_and_b_and_c = PLTLfAnd([a, b, c])
    not_a_or_not_b_or_not_c = PLTLfOr([PLTLfNot(a), PLTLfNot(b), PLTLfNot(c)])
    assert a_and_b_and_c.negate() == not_a_or_not_b_or_not_c

    before_a = PLTLfBefore(a)
    weakBefore_not_a = PLTLfWeakBefore(PLTLfNot(a))
    assert before_a.negate() == weakBefore_not_a

    once_a = PLTLfOnce(a)
    false_pastRelease_not_a = PLTLfPastRelease([PLTLfFalse(), PLTLfNot(a)])
    assert once_a.negate() == false_pastRelease_not_a

    historically_a = PLTLfHistorically(a)
    true_since_not_a = PLTLfSince([PLTLfTrue(), PLTLfNot(a)])
    assert historically_a.negate() == true_since_not_a


def test_names():

    good = ["a", "b", "name", "complex_name", "proposition10"]
    bad = ["Future", "X", "$", "", "40a", "niceName"]

    for name in good:
        str(PLTLfAtomic(name)) == name

    for name in bad:
        with pytest.raises(ValueError):
            str(PLTLfAtomic(name)) == name


def test_nnf():
    parser = PLTLfParser()
    a, b, c = [PLTLfAtomic(c) for c in "abc"]

    f = parser("!(a & !b)")
    assert f.to_nnf() == PLTLfOr([PLTLfNot(a), b])

    f = parser("!(!a | b)")
    assert f.to_nnf() == PLTLfAnd([a, PLTLfNot(b)])

    f = parser("!(a <-> b)")
    assert f.to_nnf() == PLTLfAnd(
        [PLTLfOr([PLTLfNot(a), PLTLfNot(b)]), PLTLfOr([a, b])]
    )

    # Yesterday and Weak Yesterday
    f = parser("!(Y (a & b))")
    assert f.to_nnf() == PLTLfWeakBefore(PLTLfOr([PLTLfNot(a), PLTLfNot(b)]))

    f = parser("!(WY (a & b))")
    assert f.to_nnf() == PLTLfBefore(PLTLfOr([PLTLfNot(a), PLTLfNot(b)]))

    # Once and Historically
    f = parser("!(O (a | b))")
    assert (
        f.to_nnf() == PLTLfHistorically(PLTLfAnd([PLTLfNot(a), PLTLfNot(b)])).to_nnf()
    )

    # Since
    f = parser("!(a S b)")
    assert f.to_nnf() == PLTLfPastRelease([PLTLfNot(a), PLTLfNot(b)])
    f = parser("!(a P b)")
    assert f.to_nnf() == PLTLfSince([PLTLfNot(a), PLTLfNot(b)])

    f = parser("!(O (a | b))")
    assert (
        f.to_nnf() == PLTLfHistorically(PLTLfAnd([PLTLfNot(a), PLTLfNot(b)])).to_nnf()
    )
    f = parser("!(H (a | b))")
    assert f.to_nnf() == PLTLfOnce(PLTLfAnd([PLTLfNot(a), PLTLfNot(b)])).to_nnf()


def test_mona():
    parser = PLTLfParser()
    a, b, c = [PLTLfAtomic(c) for c in "abc"]
    tt = PLTLfTrue()
    ff = PLTLfFalse()

    assert a.to_mona(v="max($)") == "(max($) in A)"
    assert b.to_mona(v="max($)") == "(max($) in B)"
    assert c.to_mona(v="max($)") == "(max($) in C)"
    assert tt.to_mona(v="max($)") == "true"
    assert ff.to_mona(v="max($)") == "false"

    f = parser("!(a & !b)")
    assert f.to_mona(v="max($)") == "~(((max($) in A) & ~((max($) in B))))"

    f = parser("!(!a | b)")
    assert f.to_mona(v="max($)") == "~((~((max($) in A)) | (max($) in B)))"

    f = parser("!(a <-> b)")
    assert (
        f.to_nnf().to_mona(v="max($)")
        == "((~((max($) in A)) | ~((max($) in B))) & ((max($) in A) | (max($) in B)))"
    )

    # Before
    f = parser("Y(a & b)")
    assert (
        f.to_mona(v="max($)")
        == "(ex1 v_1: v_1 in $ & v_1=max($)-1 & max($)>0 & ((v_1 in A) & (v_1 in B)))"
    )

    # Since
    f = parser("a S b")
    assert (
        f.to_mona(v="max($)")
        == "(ex1 v_1: v_1 in $ & 0<=v_1&v_1<=max($) & (v_1 in B) & (all1 v_2: v_2 in $ & v_1<v_2&v_2<=max($)"
        " => (v_2 in A)))"
    )

    # Once and Historically
    f = parser("O(a & b)")
    assert (
        f.to_mona(v="max($)")
        == "(ex1 v_1: v_1 in $ & 0<=v_1&v_1<=max($) & ((v_1 in A) & (v_1 in B)) & (all1 v_2: "
        "v_2 in $ & v_1<v_2&v_2<=max($) => true))"
    )
    f = parser("a & O(b)")
    assert (
        f.to_mona(v="max($)")
        == "((max($) in A) & (ex1 v_1: v_1 in $ & 0<=v_1&v_1<=max($) & (v_1 in B) & (all1 v_2: v_2 in $ & v_1<v_2&v_2<=max($) => "
        "true)))"
    )
    f = parser("H(a | b)")
    assert (
        f.to_mona(v="max($)")
        == "~((ex1 v_1: v_1 in $ & 0<=v_1&v_1<=max($) & ~(((v_1 in A) | (v_1 in B))) & (all1 v_2: "
        "v_2 in $ & v_1<v_2&v_2<=max($) => true)))"
    )


# @pytest.fixture(scope="session", params=LTLfFixtures.ltlf_formulas)
# def ltlf_formula_automa_pair(request):
#     formula_obj = parser(request.param)
#     automaton = formula_obj.to_automaton()
#     return formula_obj, automaton
#
#
# @pytest.fixture(scope="session", params=LTLfFixtures.ltlf_formulas)
# def ltlf_formula_nnf_pair(request):
#     formula_obj = parser(request.param)
#     nnf = formula_obj.to_nnf()
#     return formula_obj, nnf


class TestParsingTree:
    @classmethod
    def setup_class(cls):

        # Path to grammar
        this_path = os.path.dirname(os.path.abspath(__file__))
        grammar_path = "../ltlf2dfa/parser/pltlf.lark"
        grammar_path = os.path.join(this_path, *grammar_path.split("/"))

        cls.checker = ParsingCheck(grammar_path)

    def test_propositional(self):

        ok, err = self.checker.precedence_check("a & !b | c", list("|&a!bc"))
        assert ok, err

        ok, err = self.checker.precedence_check(
            "!a&(b->c)", "&,!,a,(,),->,b,c".split(",")
        )
        assert ok, err

    def test_unary(self):

        ok, err = self.checker.precedence_check("Y Y a", list("YYa"))
        assert ok, err

        ok, err = self.checker.precedence_check(
            "Y(H faLse)", "Y ( ) H faLse".split(" ")
        )
        assert ok, err

        ok, err = self.checker.precedence_check("Y H a", list("YHa"))
        assert ok, err

        ok, err = self.checker.precedence_check("HY a", list("HYa"))
        assert ok, err

        ok, err = self.checker.precedence_check(
            "YHYO H prop0", "Y H Y O H prop0".split(" ")
        )
        assert ok, err

        ok, err = self.checker.precedence_check(
            "YY!(!HHH a)", "Y Y ! ( ) ! H H H a".split(" ")
        )
        assert ok, err

    def test_bad_termination(self):

        # Wrong termination or space
        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("!a&", list("!a&"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("!&b", list("!&b"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("a|b|", list("a|b|"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("H", list("H"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("(a)(", list("(a)("))

        with pytest.raises(lark.UnexpectedInput) as exc:
            self.checker.precedence_check("aSa", list("aSa"))

        with pytest.raises(lark.UnexpectedInput) as exc:
            self.checker.precedence_check("Ya", list("Ya"))

    def test_bad_names(self):

        # Invalid names
        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("H Y H", list("HYH"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("Past", ["Past"])

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("Y O", list("YO"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("!Y", list("!Y"))
