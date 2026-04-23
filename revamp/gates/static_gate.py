"""
gates/static_gate.py — Static Analysis Gate
============================================
Gate 3: Runs cppcheck and clang-tidy on the patched source. Any new
warnings (not present in the baseline) cause the diff to be discarded.

Owner: Sydney (Static Analysis & Safety Lead)
"""

from pathlib import Path


def run(patched_dir: Path, baseline_warnings: list, config: dict) -> bool:
    """Returns True if no new static analysis warnings are introduced."""
    raise NotImplementedError("Static gate not yet implemented.")
