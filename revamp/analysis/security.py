"""
analysis/security.py — CodeQL Security Analysis
================================================
Interfaces with CodeQL to scan for security vulnerabilities in HDF5 C source.
Integrated per Gerd's feedback post-meeting (second verification layer).

Note: HDF Group's own CI already runs CodeQL and recently addressed 300
issues. This layer focuses on net-new issues introduced by Revamp diffs.

Owner: Sydney (Static Analysis & Safety Lead)
"""

from pathlib import Path
from typing import List


def run_codeql(src_dir: Path, database_path: Path) -> List[dict]:
    """
    Run CodeQL analysis against a pre-built CodeQL database.

    Args:
        src_dir:       Path to HDF5 source.
        database_path: Path to CodeQL database directory.

    Returns:
        List of finding dicts with keys: rule, severity, file, line, message.
    """
    raise NotImplementedError("CodeQL runner not yet implemented.")


def new_findings(baseline: List[dict], current: List[dict]) -> List[dict]:
    """Return CodeQL findings introduced by a diff (not present in baseline)."""
    raise NotImplementedError("Finding diffing not yet implemented.")
