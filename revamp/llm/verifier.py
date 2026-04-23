"""
llm/verifier.py — LLM Verification Layer
=========================================
Second LLM pass (GPT-4o) that independently reviews the diff generated
by primary.py before it enters the binary gate sequence.

Verifier checks for:
    - Semantic equivalence (same behavior before and after)
    - Scope creep (new API, new exports, new files)
    - HDF5-specific anti-patterns (error handling conventions, H5E usage)
    - Correctness of type conversion logic (especially H5T module)

Added per Gerd's post-meeting feedback as a second verification layer.

Owner: Basil (LLM Engine Lead)
"""

from typing import Optional


def verify_diff(diff: str, function_context: dict, config: dict) -> dict:
    """
    Run the verifier LLM against a candidate diff.

    Args:
        diff:             Unified diff string from primary.py.
        function_context: Original function context from tree_sitter_parser.
        config:           Pipeline configuration dictionary.

    Returns:
        Dict with keys: approved (bool), rationale (str), issues (list).
    """
    raise NotImplementedError("Verifier LLM not yet implemented.")
