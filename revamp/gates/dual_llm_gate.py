"""
gates/dual_llm_gate.py — Dual LLM Approval Gate
=================================================
Gate 2: Both the primary LLM (Claude Sonnet) and verifier LLM (GPT-4o)
must independently approve the diff. If either rejects, the diff is
discarded. This is the first layer of the LLM verification chain.

Owner: Basil (LLM Engine Lead)
"""

from pathlib import Path


def run(diff: str, function_context: dict, config: dict) -> bool:
    """Returns True only if both LLMs approve the diff."""
    raise NotImplementedError("Dual LLM gate not yet implemented.")
