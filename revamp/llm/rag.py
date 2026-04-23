"""
llm/rag.py — Retrieval-Augmented Generation Interface
======================================================
Retrieves relevant function examples from ChromaDB to use as few-shot
context in primary LLM prompts. Helps the model understand HDF5 coding
conventions, error handling patterns, and acceptable refactoring style.

Owner: Basil (LLM Engine Lead) / Firdavs (Indexing & RAG Lead)
"""

from typing import List


def retrieve_examples(query: str, n: int = 5) -> List[dict]:
    """
    Retrieve the top-n most relevant HDF5 functions for a given query.

    Args:
        query: Description of the target function or refactoring goal.
        n:     Number of examples to retrieve.

    Returns:
        List of function dicts with source code and metadata.
    """
    raise NotImplementedError("RAG retrieval not yet implemented.")
