# -*- coding: utf-8 -*-
"""This module contains the configurations for the tests."""
import inspect
import os
from pathlib import Path

TEST_ROOT_DIR = os.path.dirname(inspect.getfile(inspect.currentframe()))  # type: ignore
ROOT_DIR = str(Path(TEST_ROOT_DIR, "..").resolve())  # type: ignore
