# -*- coding: utf-8 -*-

"""Main module of the pakage."""

import itertools as it
from subprocess import PIPE, Popen, TimeoutExpired
import os
import re
import signal

from sympy import symbols, And, Not, Or, simplify

from ltlf2dfa.base import MonaProgram

PACKAGE_DIR = os.path.dirname(os.path.abspath(__file__))

UNSAT_DOT = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];
 node [shape = doublecircle];
 node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;
 1 -> 1 [label="true"];
}"""


def get_value(text, regex, value_type=float):
    """Dump a value from a file based on a regex passed in."""
    pattern = re.compile(regex, re.MULTILINE)
    results = pattern.search(text)
    if results:
        return value_type(results.group(1))
    else:
        print("Could not find the value {}, in the text provided".format(regex))
        return value_type(0.0)


def ter2symb(ap, ternary):
    """Translate ternary output to symbolic."""
    expr = And()
    i = 0
    for value in ternary:
        if value == "1":
            expr = And(expr, ap[i] if isinstance(ap, tuple) else ap)
        elif value == "0":
            assert value == "0"
            expr = And(expr, Not(ap[i] if isinstance(ap, tuple) else ap))
        else:
            assert value == "X", "[ERROR]: the guard is not X"
        i += 1
    return expr


def simplify_guard(guards):
    """Make a big OR among guards and simplify them."""
    final = Or()
    for g in guards:
        final = Or(final, g)
    return simplify(final)


def parse_mona(mona_output):
    """Parse mona output and construct a dot."""
    free_variables = get_value(
        mona_output, r".*DFA for formula with free variables:[\s]*(.*?)\n.*", str
    )
    if "state" in free_variables:
        free_variables = None
    else:
        free_variables = symbols(
            " ".join(
                x.strip().lower() for x in free_variables.split() if len(x.strip()) > 0
            )
        )

    # initial_state = get_value(mona_output, '.*Initial state:[\s]*(\d+)\n.*', int)
    accepting_states = get_value(mona_output, r".*Accepting states:[\s]*(.*?)\n.*", str)
    accepting_states = [
        str(x.strip()) for x in accepting_states.split() if len(x.strip()) > 0
    ]
    # num_states = get_value(mona_output, '.*Automaton has[\s]*(\d+)[\s]states.*', int) - 1

    dot = """digraph MONA_DFA {
 rankdir = LR;
 center = true;
 size = "7.5,10.5";
 edge [fontname = Courier];
 node [height = .5, width = .5];\n"""
    dot += " node [shape = doublecircle]; {};\n".format("; ".join(accepting_states))
    dot += """ node [shape = circle]; 1;
 init [shape = plaintext, label = ""];
 init -> 1;\n"""

    dot_trans = dict()  # maps each couple (src, dst) to a list of guards
    for line in mona_output.splitlines():
        if line.startswith("State "):
            orig_state = get_value(line, r".*State[\s]*(\d+):\s.*", int)
            guard = get_value(line, r".*:[\s](.*?)[\s]->.*", str)
            if free_variables:
                guard = ter2symb(free_variables, guard)
            else:
                guard = ter2symb(free_variables, "X")
            dest_state = get_value(line, r".*state[\s]*(\d+)[\s]*.*", int)
            if orig_state:
                if (orig_state, dest_state) in dot_trans.keys():
                    dot_trans[(orig_state, dest_state)].append(guard)
                else:
                    dot_trans[(orig_state, dest_state)] = [guard]

    for c, guards in dot_trans.items():
        simplified_guard = simplify_guard(guards)
        dot += ' {} -> {} [label="{}"];\n'.format(
            c[0], c[1], str(simplified_guard).lower()
        )

    dot += "}"
    return dot


def compute_declare_assumption(s):
    """Compute declare assumptions."""
    pairs = list(it.combinations(s, 2))

    if pairs:
        first_assumption = "~(ex1 y: 0<=y & y<=max($) & ~("
        for symbol in s:
            if symbol == s[-1]:
                first_assumption += "y in " + symbol + "))"
            else:
                first_assumption += "y in " + symbol + " | "

        second_assumption = "~(ex1 y: 0<=y & y<=max($) & ~("
        for pair in pairs:
            if pair == pairs[-1]:
                second_assumption += (
                    "(y notin " + pair[0] + " | y notin " + pair[1] + ")));"
                )
            else:
                second_assumption += (
                    "(y notin " + pair[0] + " | y notin " + pair[1] + ") & "
                )

        return first_assumption + " & " + second_assumption
    else:
        return None


def createMonafile(p: str):
    """Write the .mona file."""
    try:
        with open("{}/automa.mona".format(PACKAGE_DIR), "w+") as file:
            file.write(p)
    except IOError:
        print("[ERROR]: Problem opening the automa.mona file!")


def invoke_mona():
    """Execute the MONA tool."""
    command = "mona -q -w {}/automa.mona".format(PACKAGE_DIR)
    process = Popen(
        args=command,
        stdout=PIPE,
        stderr=PIPE,
        preexec_fn=os.setsid,
        shell=True,
        encoding="utf-8",
    )
    try:
        output, error = process.communicate(timeout=30)
        return str(output).strip()
    except TimeoutExpired:
        os.killpg(os.getpgid(process.pid), signal.SIGTERM)
        return False


def output2dot(mona_output):
    """Parse the mona output or return the unsatisfiable dot."""
    if "Formula is unsatisfiable" in mona_output:
        return UNSAT_DOT
    else:
        return parse_mona(mona_output)


def to_dfa(f) -> str:
    """Translate to deterministic finite-state automaton."""
    p = MonaProgram(f)
    mona_p_string = p.mona_program()
    createMonafile(mona_p_string)
    mona_dfa = invoke_mona()
    return output2dot(mona_dfa)
