"""
llm/primary.py — Primary LLM Refactoring Engine
================================================
Drives Claude Sonnet (primary) or GPT-4o (fallback) to generate
candidate refactoring diffs for HDF5 C functions.

Constraints enforced via system prompt:
    - No new public API additions
    - No new symbol exports
    - No new files
    - Maximum N files changed per diff (configurable)
    - C library only — no C++ interface changes

Owner: Basil (LLM Engine Lead)
"""

from pathlib import Path
from typing import Optional


def generate_diff(
    function_context: dict,
    rag_examples: list,
    config: dict,
) -> Optional[str]:
    """
    Generate a candidate refactoring diff for a target function.

    Args:
        function_context: Parsed function data from tree_sitter_parser.
        rag_examples:     Similar functions retrieved from ChromaDB (few-shot).
        config:           Pipeline configuration dictionary.

    Returns:
        Unified diff string if the LLM produces a valid candidate, else None.
    """
    raise NotImplementedError("Primary LLM engine not yet implemented.")
