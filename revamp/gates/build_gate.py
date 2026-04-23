"""
gates/build_gate.py — CMake Build Gate
=======================================
Gate 1: Verifies that the HDF5 C library builds cleanly after a diff
is applied. Any build failure causes the diff to be discarded.

Uses the same CMake flags as ci-basic.yml to ensure parity with CI.

Owner: Abhinav (Pipeline & CI Lead)
"""

from pathlib import Path


def run(patched_dir: Path, config: dict) -> bool:
    """
    Run CMake configure + build on a patched source directory.

    Returns:
        True if build succeeds, False otherwise.
    """
    raise NotImplementedError("Build gate not yet implemented.")
