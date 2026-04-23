"""
gates/apply.py — Diff Application Utility
==========================================
Applies a candidate diff to the sandbox repo in a temporary working
directory for gate testing. Rolls back cleanly on any failure.

Owner: Abhinav (Pipeline & CI Lead)
"""

from pathlib import Path
from typing import Optional


def apply_diff(repo_root: Path, diff: str) -> Optional[Path]:
    """
    Apply a unified diff to a temporary clone of the repo.

    Returns:
        Path to the patched working directory, or None on failure.
    """
    raise NotImplementedError("Diff application not yet implemented.")


def rollback(work_dir: Path) -> None:
    """Remove a temporary patched working directory."""
    raise NotImplementedError("Rollback not yet implemented.")
