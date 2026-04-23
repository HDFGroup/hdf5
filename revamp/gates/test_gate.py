"""
gates/test_gate.py — CTest Regression Gate
===========================================
Gate 5: Runs the HDF5 serial test suite (CTest) on the patched source.
Any test regressions cause the diff to be discarded.

Parallel/MPI tests are excluded (no MPI on CI runner) — those are
covered by HDF Group's own CDash infrastructure.

Owner: Abhinav (Pipeline & CI Lead)
"""

from pathlib import Path


def run(build_dir: Path, config: dict) -> bool:
    """Returns True if all CTest serial tests pass with no regressions."""
    raise NotImplementedError("Test gate not yet implemented.")
