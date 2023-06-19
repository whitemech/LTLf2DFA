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
"""Test PPLTL."""
import os

import lark
import pytest

from ltlf2dfa.parser.ppltl import PPLTLParser
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

# from .conftest import LTLfFixtures
from .parsing import ParsingCheck


def test_parser():
    parser = PPLTLParser()
    a, b, c = [PPLTLAtomic(c) for c in "abc"]

    assert parser("!a | b <-> !(a & !b) <-> a->b") == PPLTLEquivalence(
        [
            PPLTLOr([PPLTLNot(a), b]),
            PPLTLNot(PPLTLAnd([a, PPLTLNot(b)])),
            PPLTLImplies([a, b]),
        ]
    )

    assert parser("(Y a)") == PPLTLBefore(a)

    assert parser("a & O(b)") == PPLTLAnd([a, PPLTLOnce(b)])

    assert parser("(O (a&b)) <-> !(H (!a | !b) )") == PPLTLEquivalence(
        [
            PPLTLOnce(PPLTLAnd([a, b])),
            PPLTLNot(PPLTLHistorically(PPLTLOr([PPLTLNot(a), PPLTLNot(b)]))),
        ]
    )

    assert parser("(a S b S !c)") == PPLTLSince([a, b, PPLTLNot(c)])

    assert parser("a & start") == PPLTLAnd([a, PPLTLStart()])


def test_negate():
    parser = PPLTLParser()
    sa, sb, sc = "a", "b", "c"
    a, b, c = PPLTLAtomic(sa), PPLTLAtomic(sb), PPLTLAtomic(sc)

    a_and_b = PPLTLAnd([a, b])
    not_a_or_not_b = PPLTLOr([PPLTLNot(a), PPLTLNot(b)])
    assert a_and_b.negate() == not_a_or_not_b

    a_and_b_and_c = PPLTLAnd([a, b, c])
    not_a_or_not_b_or_not_c = PPLTLOr([PPLTLNot(a), PPLTLNot(b), PPLTLNot(c)])
    assert a_and_b_and_c.negate() == not_a_or_not_b_or_not_c

    before_a = PPLTLBefore(a)
    weakBefore_not_a = PPLTLWeakBefore(PPLTLNot(a))
    assert before_a.negate() == weakBefore_not_a

    once_a = PPLTLOnce(a)
    false_pastRelease_not_a = PPLTLPastRelease([PPLTLFalse(), PPLTLNot(a)])
    assert once_a.negate() == false_pastRelease_not_a

    historically_a = PPLTLHistorically(a)
    true_since_not_a = PPLTLSince([PPLTLTrue(), PPLTLNot(a)])
    assert historically_a.negate() == true_since_not_a


def test_names():
    good = ["a", "b", "name", "complex_name", "proposition10"]
    bad = ["Future", "X", "$", "", "40a", "niceName"]

    for name in good:
        str(PPLTLAtomic(name)) == name

    for name in bad:
        with pytest.raises(ValueError):
            str(PPLTLAtomic(name)) == name


def test_nnf():
    parser = PPLTLParser()
    a, b, c = [PPLTLAtomic(c) for c in "abc"]

    f = parser("!(a & !b)")
    assert f.to_nnf() == PPLTLOr([PPLTLNot(a), b])

    f = parser("!(!a | b)")
    assert f.to_nnf() == PPLTLAnd([a, PPLTLNot(b)])

    f = parser("!(a <-> b)")
    assert f.to_nnf() == PPLTLAnd(
        [PPLTLOr([PPLTLNot(a), PPLTLNot(b)]), PPLTLOr([a, b])]
    )

    # Yesterday and Weak Yesterday
    f = parser("!(Y (a & b))")
    assert f.to_nnf() == PPLTLWeakBefore(PPLTLOr([PPLTLNot(a), PPLTLNot(b)]))

    f = parser("!(WY (a & b))")
    assert f.to_nnf() == PPLTLBefore(PPLTLOr([PPLTLNot(a), PPLTLNot(b)]))

    # Once and Historically
    f = parser("!(O (a | b))")
    assert (
        f.to_nnf() == PPLTLHistorically(PPLTLAnd([PPLTLNot(a), PPLTLNot(b)])).to_nnf()
    )

    # Since
    f = parser("!(a S b)")
    assert f.to_nnf() == PPLTLPastRelease([PPLTLNot(a), PPLTLNot(b)])
    f = parser("!(a P b)")
    assert f.to_nnf() == PPLTLSince([PPLTLNot(a), PPLTLNot(b)])

    f = parser("!(O (a | b))")
    assert (
        f.to_nnf() == PPLTLHistorically(PPLTLAnd([PPLTLNot(a), PPLTLNot(b)])).to_nnf()
    )
    f = parser("!(H (a | b))")
    assert f.to_nnf() == PPLTLOnce(PPLTLAnd([PPLTLNot(a), PPLTLNot(b)])).to_nnf()


def test_mona():
    parser = PPLTLParser()
    a, b, c = [PPLTLAtomic(c) for c in "abc"]
    tt = PPLTLTrue()
    ff = PPLTLFalse()

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
        grammar_path = "../ltlf2dfa/parser/ppltl.lark"
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
