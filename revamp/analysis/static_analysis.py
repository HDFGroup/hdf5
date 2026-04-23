"""
analysis/static_analysis.py — Static Analysis Runner
=====================================================
Runs cppcheck and clang-tidy against HDF5 C source and diffs.
Warnings from either tool block the static gate (gates/static_gate.py).

Config built from scratch — does NOT inherit HDF5's existing
cppcheck/clang-tidy files, per project requirements.

Owner: Sydney (Static Analysis & Safety Lead)
"""

from pathlib import Path
from typing import List


def run_cppcheck(src_dir: Path) -> List[dict]:
    """
    Run cppcheck on a source directory.

    Returns:
        List of warning dicts with keys: file, line, severity, message.
    """
    raise NotImplementedError("cppcheck runner not yet implemented.")


def run_clang_tidy(src_dir: Path, compile_commands: Path) -> List[dict]:
    """
    Run clang-tidy using a compile_commands.json database.

    Args:
        src_dir:          Path to source directory.
        compile_commands: Path to compile_commands.json (from CMake).

    Returns:
        List of warning dicts.
    """
    raise NotImplementedError("clang-tidy runner not yet implemented.")


def new_warnings(baseline: List[dict], current: List[dict]) -> List[dict]:
    """Return warnings present in current but not in baseline."""
    raise NotImplementedError("Warning diffing not yet implemented.")
