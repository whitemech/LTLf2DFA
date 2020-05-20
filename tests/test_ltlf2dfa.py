# -*- coding: utf-8 -*-
"""Test the ltlf2dfa tool."""
import pytest
import os
import lark

from ltlf2dfa.parser.ltlf import LTLfParser
from ltlf2dfa.parser.pltlf import PLTLfParser

