# -*- coding: utf-8 -*-
"""Misc tests."""
import os


def test_ltlf_example_readme():
    from ltlf2dfa.parser.ltlf import LTLfParser

    parser = LTLfParser()
    formula = "G(a -> X b)"
    parsed_formula = parser(formula)

    assert str(parsed_formula) == "G((a -> X(b)))"
    assert parsed_formula.find_labels() == {c for c in "ab"}

    # dfa = parsed_formula.to_dfa()


def test_pltlf_example_readme():
    from ltlf2dfa.parser.pltlf import PLTLfParser

    parser = PLTLfParser()
    formula = "H(a -> Y b)"
    parsed_formula = parser(formula)

    assert str(parsed_formula) == "H((a -> Y(b)))"
    assert parsed_formula.find_labels() == {c for c in "ab"}

    # dfa = parsed_formula.to_dfa()


def test_hash_consistency_after_pickling():
    from ltlf2dfa.parser.ltlf import LTLfParser
    import pickle

    parser = LTLfParser()
    formula = "F (a & !b)"
    old_obj = parser(formula)

    h = hash(old_obj)
    pickle.dump(old_obj, open("temp", "wb"))
    new_obj = pickle.load(open("temp", "rb"))

    assert new_obj._hash is None
    assert h == hash(new_obj)

    os.remove("temp")


def test_QuotedFormula():
    from ltlf2dfa.base import QuotedFormula
    from ltlf2dfa.parser.ltlf import LTLfParser
    from ltlf2dfa.ltlf import LTLfAtomic, LTLfAnd

    f = LTLfParser()("!(G a)")
    qf = QuotedFormula(f)
    atomf = LTLfAnd([LTLfAtomic(f), LTLfAtomic(f)])

    assert qf.wrapped is f

    dir_qf = dir(qf)
    for member in dir(f):
        assert member in dir_qf
        assert hasattr(qf, member)
