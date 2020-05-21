# -*- coding: utf-8 -*-
"""Test LTLf."""
import pytest
import os
import lark

from ltlf2dfa.ltlf import (
    LTLfAtomic,
    LTLfAnd,
    LTLfEquivalence,
    LTLfOr,
    LTLfNot,
    LTLfImplies,
    LTLfEventually,
    LTLfAlways,
    LTLfUntil,
    LTLfRelease,
    LTLfNext,
    LTLfWeakNext,
    LTLfTrue,
    LTLfFalse,
)
from ltlf2dfa.parser.ltlf import LTLfParser

# from ltlf2dfa.pl import PLAtomic, PLTrue, PLFalse, PLAnd, PLOr

# from .conftest import LTLfFixtures
from .parsing import ParsingCheck


def test_parser():
    parser = LTLfParser()
    a, b, c = [LTLfAtomic(c) for c in "abc"]

    assert parser("!a | b <-> !(a & !b) <-> a->b") == LTLfEquivalence(
        [
            LTLfOr([LTLfNot(a), b]),
            LTLfNot(LTLfAnd([a, LTLfNot(b)])),
            LTLfImplies([a, b]),
        ]
    )

    assert parser("(X a) & (WX !b)") == LTLfAnd([LTLfNext(a), LTLfWeakNext(LTLfNot(b))])

    assert parser("(F (a&b)) <-> !(G (!a | !b) )") == LTLfEquivalence(
        [
            LTLfEventually(LTLfAnd([a, b])),
            LTLfNot(LTLfAlways(LTLfOr([LTLfNot(a), LTLfNot(b)]))),
        ]
    )

    assert parser("(a U b U c) <-> !(!a R !b R !c)") == LTLfEquivalence(
        [
            LTLfUntil([a, b, c]),
            LTLfNot(LTLfRelease([LTLfNot(a), LTLfNot(b), LTLfNot(c)])),
        ]
    )


def test_names():

    good = ["a", "b", "name", "complex_name", "proposition10"]
    bad = ["Future", "X", "$", "", "40a", "niceName"]

    for name in good:
        str(LTLfAtomic(name)) == name

    for name in bad:
        with pytest.raises(ValueError):
            str(LTLfAtomic(name)) == name


def test_nnf():
    parser = LTLfParser()
    a, b, c = [LTLfAtomic(c) for c in "abc"]

    f = parser("!(a & !b)")
    assert f.to_nnf() == LTLfOr([LTLfNot(a), b])

    f = parser("!(!a | b)")
    assert f.to_nnf() == LTLfAnd([a, LTLfNot(b)])

    f = parser("!(a <-> b)")
    assert f.to_nnf() == LTLfAnd([LTLfOr([LTLfNot(a), LTLfNot(b)]), LTLfOr([a, b])])

    # Next and Weak Next
    f = parser("!(X (a & b))")
    assert f.to_nnf() == LTLfWeakNext(LTLfOr([LTLfNot(a), LTLfNot(b)]))

    f = parser("!(WX (a & b))")
    assert f.to_nnf() == LTLfNext(LTLfOr([LTLfNot(a), LTLfNot(b)]))

    # Eventually and Always
    f = parser("!(F (a | b))")
    assert f.to_nnf() == LTLfAlways(LTLfAnd([LTLfNot(a), LTLfNot(b)])).to_nnf()

    # Until and Release
    f = parser("!(a U b)")
    assert f.to_nnf() == LTLfRelease([LTLfNot(a), LTLfNot(b)])
    f = parser("!(a R b)")
    assert f.to_nnf() == LTLfUntil([LTLfNot(a), LTLfNot(b)])

    f = parser("!(F (a | b))")
    assert f.to_nnf() == LTLfAlways(LTLfAnd([LTLfNot(a), LTLfNot(b)])).to_nnf()
    f = parser("!(G (a | b))")
    assert f.to_nnf() == LTLfEventually(LTLfAnd([LTLfNot(a), LTLfNot(b)])).to_nnf()


def test_mona():
    parser = LTLfParser()
    a, b, c = [LTLfAtomic(c) for c in "abc"]
    tt = LTLfTrue()
    ff = LTLfFalse()

    assert a.to_mona(v="0") == "(0 in A)"
    assert b.to_mona(v="0") == "(0 in B)"
    assert c.to_mona(v="0") == "(0 in C)"
    assert tt.to_mona(v="0") == "true"
    assert ff.to_mona(v="0") == "false"

    f = parser("!(a & !b)")
    assert f.to_mona(v="0") == "~(((0 in A) & ~((0 in B))))"

    f = parser("!(!a | b)")
    assert f.to_mona(v="0") == "~((~((0 in A)) | (0 in B)))"

    f = parser("!(a <-> b)")
    assert (
        f.to_nnf().to_mona(v="0")
        == "((~((0 in A)) | ~((0 in B))) & ((0 in A) | (0 in B)))"
    )

    # Next and Weak Next
    f = parser("X(a & b)")
    assert f.to_mona(v="0") == "(ex1 v_1: v_1=1 & ((v_1 in A) & (v_1 in B)))"

    f = parser("WX (a & b)")
    assert (
        f.to_mona(v="0")
        == "((0 = max($)) | (ex1 v_1: v_1=1 & ((v_1 in A) & (v_1 in B))))"
    )

    # Until and Release
    f = parser("a U b")
    assert (
        f.to_mona(v="0")
        == "(ex1 v_1: 0<=v_1&v_1<=max($) & (v_1 in B) & (all1 v_2: 0<=v_2&v_2<v_1"
        " => (v_2 in A)))"
    )
    f = parser("a R b")
    assert (
        f.to_mona(v="0")
        == "((ex1 v_1: 0<=v_1&v_1<=max($) & (v_1 in A) & (all1 v_2: 0<=v_2&v_2<=v_1"
        " => (v_2 in B))) | (all1 v_2: 0<=v_2&v_2<=max($) => (v_2 in B)))"
    )

    # Eventually and Always
    f = parser("F(a & b)")
    assert (
        f.to_mona(v="0")
        == "(ex1 v_1: 0<=v_1&v_1<=max($) & ((v_1 in A) & (v_1 in B)) & (all1 v_2: "
        "0<=v_2&v_2<v_1 => true))"
    )
    f = parser("G(a | b)")
    assert (
        f.to_mona(v="0")
        == "((ex1 v_1: 0<=v_1&v_1<=max($) & false & (all1 v_2: 0<=v_2&v_2<=v_1 => "
        "((v_2 in A) | (v_2 in B)))) | (all1 v_2: 0<=v_2&v_2<=max($) => ((v_2 in A) "
        "| (v_2 in B))))"
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
        grammar_path = "../ltlf2dfa/parser/ltlf.lark"
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

        ok, err = self.checker.precedence_check("X X a", list("XXa"))
        assert ok, err

        ok, err = self.checker.precedence_check(
            "X(G faLse)", "X ( ) G faLse".split(" ")
        )
        assert ok, err

        ok, err = self.checker.precedence_check("X G a", list("XGa"))
        assert ok, err

        ok, err = self.checker.precedence_check("GX a", list("GXa"))
        assert ok, err

        ok, err = self.checker.precedence_check(
            "XGXFWX G prop0", "X G X F WX G prop0".split(" ")
        )
        assert ok, err

        ok, err = self.checker.precedence_check(
            "XXWX!(!WXGGG a)", "X X WX ! ( ) ! WX G G G a".split(" ")
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
            self.checker.precedence_check("G", list("G"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("(a)(", list("(a)("))

        with pytest.raises(lark.UnexpectedInput) as exc:
            self.checker.precedence_check("aUa", list("aUa"))

        with pytest.raises(lark.UnexpectedInput) as exc:
            self.checker.precedence_check("Xa", list("Xa"))

    def test_bad_names(self):

        # Invalid names
        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("G X G", list("GXG"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("Future", ["Future"])

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("X F", list("XF"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("!X", list("!X"))
