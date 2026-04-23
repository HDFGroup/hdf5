"""
tree_sitter_parser.py — HDF5 C Source Parser
=============================================
Parses HDF5 C source files using tree-sitter to produce a structured
representation of functions, signatures, call graphs, and symbol tables.

Scope: C library only (src/*.c, src/*.h). C++ interface excluded.

Output feeds into:
    - embedder.py   (function-level embeddings)
    - complexity.py (complexity analysis input)

Owner: Firdavs (Indexing & RAG Lead)
"""

from pathlib import Path
from typing import Iterator


def parse_file(path: Path) -> dict:
    """
    Parse a single C source file and return its AST summary.

    Args:
        path: Path to a .c or .h file.

    Returns:
        Dictionary with keys: functions, structs, macros, includes.
    """
    raise NotImplementedError("tree-sitter parsing not yet implemented.")


def parse_directory(src_dir: Path) -> Iterator[dict]:
    """
    Recursively parse all C files in src_dir.

    Args:
        src_dir: Root source directory (e.g., hdf5-sandbox/src/).

    Yields:
        Parsed file dictionaries.
    """
    raise NotImplementedError("Directory parsing not yet implemented.")
